library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

make_rect <-
  function(xmin, ymin, xmax, ymax) {
    st_sfc(
      st_point(c(xmin, ymin)),
      st_point(c(xmax, ymax))) %>%
      st_bbox() %>%
      st_as_sfc()
  }

make_margins <-
  function(bbox, margin_size, crs) {
    top <- bbox[["ymax"]]
    right <- bbox[["xmax"]]
    bottom <- bbox[["ymin"]]
    left <- bbox[["xmin"]]
    xstep <- (bbox[["xmax"]] - bbox[["xmin"]]) * margin_size
    ystep <- (bbox[["ymax"]] - bbox[["ymin"]]) * margin_size

    c(make_rect(left, top, right, top - ystep),
      make_rect(right, top, right - xstep, bottom),
      make_rect(left, bottom, right, bottom + ystep),
      make_rect(left, top, left + xstep, bottom)) %>%
      st_sf(crs = crs) %>%
      mutate(direction = c("north", "east", "south", "west"))
  }

# Generate a bounding box that captures X% of features (i.e. Drop outliers)
squeeze_bbox <-
  function(features, capture_targ = 0.9995, step_size = 0.025, max_steps = 50, sample_rate = 0.5) {
    message("Finding optimal bounding box covering ", capture_targ, " of features.")
    features <-
      features %>%
      sample_n(nrow(.) * sample_rate) %>%
      filter(st_is_simple(.))
    targ_count <- nrow(features) * capture_targ
    bbox <- st_bbox(features)
    message(
      "Original bounding box (", bbox[["xmin"]], " ", bbox[["ymin"]], ", ",
      bbox[["xmax"]], " ", bbox[["ymax"]], ") has an area of ",
      bbox %>% st_as_sfc() %>% st_area() %>% round(), ".")

    for (i in 1:max_steps) {
      # Find the margin (north/south/east/west) with the fewest features and
      # remove all the features in it (i.e. Trim that margin away)
      margins <- make_margins(bbox, step_size, crs = st_crs(features))
      marginal_features <- st_join(margins, features)
      emptiest_margin <-
        marginal_features %>%
        group_by(direction) %>%
        count() %>%
        arrange(n) %>%
        first()
      message(
        "Next emptiest margin is in the ", emptiest_margin$direction,
        ", with ", emptiest_margin$n, " features...")

      message(
        "Current bounding box (",
        bbox[["xmin"]], " ", bbox[["ymin"]], ", ",
        bbox[["xmax"]], " ", bbox[["ymax"]], ") has an area of ",
        bbox %>% st_as_sfc() %>% st_area() %>% round(), ".")

      if (nrow(features) - emptiest_margin$n < targ_count) {
        message(
          "Target feature count reached. Optimal bounding box (",
          bbox[["xmin"]], " ", bbox[["ymin"]], ", ",
          bbox[["xmax"]], " ", bbox[["ymax"]], ") has an area of ",
          bbox %>% st_as_sfc() %>% st_area() %>% round(), ".")
        return(bbox)
      }
      else {
        kill_features <-
          filter(marginal_features, direction == emptiest_margin$direction)

        features <-
          filter(features, !cluster_id %in% kill_features$cluster_id)

        bbox <- st_bbox(features)
      }
    }
    message(
      "Maximum iteration count reached. Best-effort bounding box (",
      bbox[["xmin"]], " ", bbox[["ymin"]], ", ",
      bbox[["xmax"]], " ", bbox[["ymax"]], ") has an area of ",
      bbox %>% st_as_sfc() %>% st_area() %>% round(), ".")
    return(bbox)
  }

make_choropleth <-
  function(features, fill_col, fill_scale, fill_opacity = 0.8, stroke = FALSE, ...) {
    message("Creating choropleth on column '", fill_col, "'...")
    features %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor = ~fill_scale(get({{fill_col}})),
        fillOpacity = fill_opacity,
        smoothFactor = 0,
        stroke = stroke,
        ...
      )
  }

make_bubblemap <-
  function(features, radius_col, radius_scale, fill_col, fill_scale, fill_opacity = 0.8, stroke = FALSE, ...) {
    message("Creating bubble map on column '", radius_col, "'...")
    features %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircles(
        radius = ~radius_scale(get({{radius_col}})),
        fillColor = ~fill_scale(get({{fill_col}})),
        fillOpacity = fill_opacity,
        stroke = stroke,
        ...
      ) %>%
      addLegend(
        pal = fill_scale,
        values = ~get({{fill_col}}),
        title = fill_col,
        opacity = 1
      )
  }

render_map <-
  function(map, out_fn, bbox = NULL, timeout = 7200) {
    message("Writing map to file '", out_fn, "'...")
    # Set browser timeout
    # All the rendering work is outsourced to a headless Chrome browser, so we need
    # to give it time to finish. The default is 10 seconds. We need to up it a lot.
    browser <- chromote::Chromote$new()
    browser$default_timeout <- timeout # 2 hours
    chromote::set_default_chromote_object(browser)
    if (!is.null(bbox)) {
      map <-
        fitBounds(
          map,
          bbox[["xmin"]], bbox[["ymin"]],
          bbox[["xmax"]], bbox[["ymax"]])
    }
    mapshot2(
      map,
      file = out_fn, selfcontained = FALSE,
      vwidth = 3840, vheight = 2160)
  }
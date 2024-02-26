library(tidyverse)
library(sf)

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

#' Squeeze bounding box
#' 
#' Generate a more efficient bounding box by dropping outliers
#' @name squeeze_bbox
#' @param features features in a spatial frame
#' @param capture_targ aim for a box that fits this proportion of features
#' @param step_size shrink the box by this much every step
#' @param max_steps maximum numbers of steps to attempt before giving up
#' @param sample_rate apply on a random sample of the features instead
#' @export
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

#' Create choropleth
#' 
#' Create a Leaflet choropleth instance. The function will create a map
#' object. You can assign it to a variable and view the variable with the
#' build-in viewer, or you can use render_map() to generate a static image
#' of the map object.
#' 
#' You can also add legends or additional layers before rendering.
#' @name make_choropleth
#' @param features features in a spatial frame
#' @param fill_col string; name of the column to use for colour
#' @param fill_scale
#'   Leaflet scale object (e.g. colorFactor/colorNumeric) or colour
#' @param fill_opacity number
#' @param stroke color
#' @param hide_legend boolean
#' @param ... additional parameters to pass to the addPolygons() step
#' @export
make_choropleth <-
  function(features, fill_col, fill_scale, fill_opacity = 0.8, stroke = FALSE, hide_legend = FALSE, ...) {
    message("Creating choropleth on column '", fill_col, "'...")
    map <-
      features %>%
      leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%
      leaflet::addPolygons(
        fillColor = ~fill_scale(get({{fill_col}})),
        fillOpacity = fill_opacity,
        smoothFactor = 0,
        stroke = stroke,
        ...)
    if (hide_legend == FALSE) {
      map <-
        map %>%
        leaflet::addLegend(
          pal = fill_scale,
          values = ~get({{fill_col}}),
          title = fill_col,
          opacity = 1)
    }
    return(map)
  }

#' Create bubblemap
#' 
#' Create a Leaflet bubblemap instance. The function will create a map
#' object. You can assign it to a variable and view the variable with the
#' build-in viewer, or you can use render_map() to generate a static image
#' of the map object.
#' 
#' You can also add legends or additional layers before rendering.
#' @name make_bubblemap
#' @param features features in a spatial frame
#' @param radius_col string; name of the column to use for radius
#' @param radius_scale number; how much to scale the radius by
#' @param fill_col string; name of the column to use for colour
#' @param fill_scale
#'   Leaflet scale object (e.g. colorFactor/colorNumeric) or colour
#' @param fill_opacity number
#' @param stroke color
#' @param hide_legend boolean
#' @param ... additional parameters to pass to the addCircles() step
#' @export
make_bubblemap <-
  function(features, radius_col, radius_scale, fill_col, fill_scale, fill_opacity = 0.8, stroke = FALSE, hide_legend = FALSE, ...) {
    message("Creating bubble map on column '", radius_col, "'...")
    map <-
      features %>%
      leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%
      leaflet::addCircles(
        radius = ~radius_scale(get({{radius_col}})),
        fillColor = ~fill_scale(get({{fill_col}})),
        fillOpacity = fill_opacity,
        stroke = stroke,
        ...)
    if (hide_legend == FALSE) {
      map <-
        map %>%
        leaflet::addLegend(
          pal = fill_scale,
          values = ~get({{fill_col}}),
          title = fill_col,
          opacity = 1)
    }
    return(map)
  }

#' Render map
#' 
#' Generates a static image from a map object using mapshot2. At high
#' resolutions and high object density, this can be a very expensive
#' operation.
#' 
#' The rendering is done via a headless Chrome browser. See README for setup.
#' @name make_bubblemap
#' @param map Leaflet map object
#' @param out_fn string; filename to render to
#' @param bbox bounding box of area to render; if not provided, map will fit to
#'   include all features
#' @param timeout integer; time to wait for rendering to finish. Defaults to
#'   7200 (2 hours)
#' @export
render_map <-
  function(map, out_fn, bbox = NULL, timeout = 7200) {
    message("Writing map to file '", out_fn, "'...")
    # Set browser timeout
    # All the rendering work is outsourced to a headless Chrome browser, so we need
    # to give it time to finish. The default is 10 seconds. We need to up it a lot.
    browser <- chromote::Chromote$new()
    browser$default_timeout <- timeout
    chromote::set_default_chromote_object(browser)
    if (!is.null(bbox)) {
      map <-
        leaflet::fitBounds(
          map,
          bbox[["xmin"]], bbox[["ymin"]],
          bbox[["xmax"]], bbox[["ymax"]])
    }
    mapview::mapshot2(
      map,
      file = out_fn, selfcontained = FALSE,
      vwidth = 3840, vheight = 2160)
  }

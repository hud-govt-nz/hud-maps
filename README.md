# HUD mapping tools
**CAUTION: This repo is public. Do not include sensitive data or key materials.**

## Installation
You'll need `devtools::install_github` to install the package:
```R
library(devtools)
install_github("hud-govt-nz/hud-maps")
```


## Usage


## Examples


## Maintaining this package
If you make changes to this package, you'll need to rerun document from the root directory to update all the R generated files.
```R
library(roxygen2)
roxygenise()
```

I had real problems installing `roxygen2`, because there's a problem with the upstream library `cli`. It's been fixed, but it's not in the CRAN version as of 29-08-2022. You might need the Github version:
```R
library(devtools)
install_github("r-lib/cli")
install_github("r-lib/roxygen2")
library(cli)
library(roxygen2)
roxygenise()
```

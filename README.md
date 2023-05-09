<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/sword)](https://CRAN.R-project.org/package=sword)
[![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

# Geomasking Tool 

The geomasking tool was developed by the New York State Department of Health (NYSDOH) Environmental Public Health Tracking (EPHT) Program with funding administered through Health Research Incorporated. The geomasker is maintained by Abigail Stamm at the NYSDOH. 

### Installing the geomasker

Run the code below in R to install GAT directly from GitHub.

``` r
# install devtools if you don't already have it
install.packages("devtools")
# install the development version of the geomasker from Github 
# with all required packages from CRAN
devtools::install_github("ajstamm/geomask", dependencies = TRUE, 
                         build_vignette = TRUE)
```

### Why create the geomasker


### How the geomasker works



### Disclaimer

The geomasker is provided as is. We welcome feedback on what worked well, suggestions for improvement, and bugs you encounter. Report all issues via the "Issues" tab or by emailing [NYSDOH EPHT](mailto:epht@health.ny.gov?subject=[Geomask in R]).

The geomasker was written in R-2.13.0 in Windows XP and was revised and converted to a package in R-4.2.0 in Windows 10 using RStudio-1.4.1103 and devtools-2.3.2. The latest version of GAT was compiled in R-4.2.2 and runs in R-4.0 through R-4.2.

If you are interested in the geomasker's history, including the original R script, visit the [archive](archive/). 




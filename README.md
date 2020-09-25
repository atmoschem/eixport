
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eixport <img src="man/figures/logo.png" align="right" alt="" width="140" />

[![Travis-CI Build
Status](https://travis-ci.org/atmoschem/eixport.svg?branch=master)](https://travis-ci.org/atmoschem/eixport)[![Build
status](https://ci.appveyor.com/api/projects/status/frk36kmayf8yff70?svg=true)](https://ci.appveyor.com/project/Schuch666/eixport)
[![Coverage
Status](https://img.shields.io/codecov/c/github/atmoschem/eixport/master.svg)](https://codecov.io/github/atmoschem/eixport?branch=master)
[![DOI](https://zenodo.org/badge/106145968.svg)](https://zenodo.org/badge/latestdoi/106145968)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eixport)](http://cran.r-project.org/web/packages/eixport)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/eixport?color=orange)](http://cran.r-project.org/package=eixport)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00607/status.svg)](https://doi.org/10.21105/joss.00607)
[![cran
checks](https://cranchecks.info/badges/worst/eixport)](https://cran.r-project.org/web/checks/check_results_eixport.html)
[![Github
Stars](https://img.shields.io/github/stars/atmoschem/eixport.svg?style=social&label=Github)](https://github.com/atmoschem/eixport)

## Exporting emissions to atmospheric models, eixport: 0.4.10

![](https://i.imgur.com/BcZ2tfW.png)

<iframe width="560" height="315" src="https://www.youtube.com/embed/gXt3hOlpYts" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>

</iframe>

Emissions are mass that affects atmosphere in complex ways, not only
physical, but also, in the health of humans, ecosystems, economically,
etc.

There are several models whose inputs are emissions, such as
[R-Line](https://www.cmascenter.org/r-line/) or
[WRF-Chem](https://ruc.noaa.gov/wrf/wrf-chem/). This R-Package provide
functions to read emissions from
[VEIN](https://github.com/ibarraespinosa/vein) and from other models in
different formats and export the emissions into the appropiate format
suitable to other models.

## Install

To install the [CRAN](https://CRAN.R-project.org/package=eixport)
version:

``` r
install.packages("eixport")
```

To install the development version:

``` r
devtools::install_github("atmoschem/eixport")
```

``` r
library(eixport)
a <- list.files(
system.file("extdata", package = "eixport"), full.names = T)
wrf <- grep(pattern = "d01", x = a, value = T)
wrf_meta(file = wrf)
#>    vars  description                memory_order field_type stagger coordinates
#> 1 Times         <NA>                        <NA>       <NA>      NA          NA
#> 2  XLAT  degree east LONGITUDE, WEST IS NEGATIVE         XY     104          NA
#> 3 XLONG degree north LATITUDE, SOUTH IS NEGATIVE         XY     104          NA
```

## Some functions:

  - [get\_edgar](https://atmoschem.github.io/eixport/reference/get_edgar.html):
    Download EDGAR emissions data.
  - [to\_rline](https://atmoschem.github.io/eixport/reference/to_rline.html):
    Export emissions to other formats
  - [to\_wrf](https://atmoschem.github.io/eixport/reference/to_wrf.html):
    Combine total/spatial/temporal/split and write emission to file
  - [to\_brams\_spm](https://atmoschem.github.io/eixport/reference/to_brams_spm.html):
    inputs for SPM BRAMS
  - [wrf\_profile](https://atmoschem.github.io/eixport/reference/wrf_profile.html):
    Create spatial profile for WRF-Chem
  - [wrf\_create](https://atmoschem.github.io/eixport/reference/wrf_create.html):
    Create emission files to the WRF-Chem
  - [wrf\_plot](https://atmoschem.github.io/eixport/reference/wrf_plot.html):
    simple plot for emissions
  - [wrf\_get](https://atmoschem.github.io/eixport/reference/wrf_get.html):
    Read variables of emission files
  - [wrf\_put](https://atmoschem.github.io/eixport/reference/wrf_put.html):
    Write variables in emission files
  - [to\_as4wrf](https://atmoschem.github.io/eixport/reference/to_as4wrf.html):
    Create WRF-Chem inputs using NCL scrip AS4WRF.ncl.
  - [to\_munich](https://atmoschem.github.io/eixport/reference/to_munich.html):
    To generate inputs for MUNICH model.

## Download EDGAR 5

``` r
get_edgar(dataset = "v50_AP", destpath = tempdir(), sector = "TOTALS", year = 2014)
```

## Paper accepted on Journal of Open Source Software

<https://doi.org/10.21105/joss.00607>

    @article{eixport,
        title = {eixport: An R package to export emissions to atmospheric models},
        journal = {The Journal of Open Source Software},
        author = {Sergio Ibarra-Espinosa and Daniel Schuch and Edmilson {Dias de Freitas}},
        year = {2018},
        doi = {10.21105/joss.00607},
        url = {http://joss.theoj.org/papers/10.21105/joss.00607},
      }

## Contributing

Please, read
[this](https://github.com/atmoschem/eixport/blob/master/CONTRIBUTING.md)
guide. Contributions of all sorts are welcome, issues and pull requests
are the preferred ways of sharing them. When contributing pull requests,
please follow the [Google’s R Style
Guide](https://google.github.io/styleguide/Rguide.xml). This project is
released with a [Contributor Code of
Conduct](https://github.com/atmoschem/eixport/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

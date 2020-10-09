
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

## Exporting emissions to atmospheric models, eixport: 0.5.0

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

\=(

Currently, not on CRAN, but we will submit in the following days

``` r
#install.packages("eixport")
```

To install the development version:

``` r
devtools::install_github("atmoschem/eixport")
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
    simple but useful plot
  - [wrf\_get](https://atmoschem.github.io/eixport/reference/wrf_get.html):
    Read variables
  - [wrf\_put](https://atmoschem.github.io/eixport/reference/wrf_put.html):
    Write variables
  - [to\_as4wrf](https://atmoschem.github.io/eixport/reference/to_as4wrf.html):
    Create WRF-Chem inputs using NCL scrip AS4WRF.ncl.
  - [to\_munich](https://atmoschem.github.io/eixport/reference/to_munich.html):
    To generate inputs for MUNICH model.

## Download EDGAR 5

``` r
get_edgar(dataset = "v50_AP", destpath = tempdir(), sector = "TOTALS", year = 2014)
```

check this [video](https://www.youtube.com/embed/gXt3hOlpYts)

### Summary

``` r
library(eixport)
file = paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
wrf_summary(file = file,
            name = c("XLAT", "XLONG"),
            fn = "mean")
#>                 Times      XLAT     XLONG
#> 1 2011-08-01_00:00:00 -23.70379 -46.50304
```

### Attributes as data.frame

``` r
file = paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
wrf_meta(file)
#> $global
#>                               att                                       vars
#> 1                           TITLE  OUTPUT FROM REAL_EM V3.9.1.1 PREPROCESSOR
#> 2                      START_DATE                        2011-08-01_00:00:00
#> 3           SIMULATION_START_DATE                        2011-08-01_00:00:00
#> 4        WEST-EAST_GRID_DIMENSION                                         64
#> 5      SOUTH-NORTH_GRID_DIMENSION                                         52
#> 6       BOTTOM-TOP_GRID_DIMENSION                                         35
#> 7                              DX                                       3000
#> 8                              DY                                       3000
#> 9                        GRIDTYPE                                          C
#> 10                       DIFF_OPT                                          1
#> 11                         KM_OPT                                          4
#> 12                       DAMP_OPT                                          3
#> 13                       DAMPCOEF                          0.200000002980232
#> 14                          KHDIF                                          0
#> 15                          KVDIF                                          0
#> 16                     MP_PHYSICS                                         10
#> 17                  RA_LW_PHYSICS                                          4
#> 18                  RA_SW_PHYSICS                                          4
#> 19              SF_SFCLAY_PHYSICS                                          1
#> 20             SF_SURFACE_PHYSICS                                          2
#> 21                 BL_PBL_PHYSICS                                          1
#> 22                     CU_PHYSICS                                          0
#> 23                SF_LAKE_PHYSICS                                          0
#> 24           SURFACE_INPUT_SOURCE                                          1
#> 25                     SST_UPDATE                                          0
#> 26                      GRID_FDDA                                          0
#> 27               GFDDA_INTERVAL_M                                          0
#> 28                    GFDDA_END_H                                          0
#> 29                     GRID_SFDDA                                          0
#> 30              SGFDDA_INTERVAL_M                                          0
#> 31                   SGFDDA_END_H                                          0
#> 32                HYPSOMETRIC_OPT                                          2
#> 33                    USE_THETA_M                                          0
#> 34                 USE_MAXW_LEVEL                                          0
#> 35                 USE_TROP_LEVEL                                          0
#> 36                        GWD_OPT                                          0
#> 37               SF_URBAN_PHYSICS                                          1
#> 38               SF_OCEAN_PHYSICS                                          0
#> 39 SIMULATION_INITIALIZATION_TYPE                             REAL-DATA CASE
#> 40   WEST-EAST_PATCH_START_UNSTAG                                          1
#> 41     WEST-EAST_PATCH_END_UNSTAG                                         63
#> 42     WEST-EAST_PATCH_START_STAG                                          1
#> 43       WEST-EAST_PATCH_END_STAG                                         64
#> 44 SOUTH-NORTH_PATCH_START_UNSTAG                                          1
#> 45   SOUTH-NORTH_PATCH_END_UNSTAG                                         51
#> 46   SOUTH-NORTH_PATCH_START_STAG                                          1
#> 47     SOUTH-NORTH_PATCH_END_STAG                                         52
#> 48  BOTTOM-TOP_PATCH_START_UNSTAG                                          1
#> 49    BOTTOM-TOP_PATCH_END_UNSTAG                                         34
#> 50    BOTTOM-TOP_PATCH_START_STAG                                          1
#> 51      BOTTOM-TOP_PATCH_END_STAG                                         35
#> 52                        GRID_ID                                          2
#> 53                      PARENT_ID                                          1
#> 54                 I_PARENT_START                                         48
#> 55                 J_PARENT_START                                         40
#> 56              PARENT_GRID_RATIO                                          3
#> 57                             DT                                         15
#> 58                        CEN_LAT                          -23.7047119140625
#> 59                        CEN_LON                          -46.5030517578125
#> 60                       TRUELAT1                                        -23
#> 61                       TRUELAT2                                        -24
#> 62                   MOAD_CEN_LAT                          -23.5499954223633
#> 63                      STAND_LON                                        -45
#> 64                       POLE_LAT                                         90
#> 65                       POLE_LON                                          0
#> 66                            GMT                                          0
#> 67                          JULYR                                       2011
#> 68                         JULDAY                                        213
#> 69                       MAP_PROJ                                          1
#> 70                  MAP_PROJ_CHAR                          Lambert Conformal
#> 71                         MMINLU                   MODIFIED_IGBP_MODIS_NOAH
#> 72                   NUM_LAND_CAT                                         21
#> 73                        ISWATER                                         17
#> 74                         ISLAKE                                         21
#> 75                          ISICE                                         15
#> 76                        ISURBAN                                         13
#> 77                     ISOILWATER                                         14
#> 78                     HYBRID_OPT                                         -1
#> 79                           ETAC                                          0
#> 
#> $vars
#>    vars MemoryOrder                 description        units stagger FieldType
#> 1  XLAT          XY LATITUDE, SOUTH IS NEGATIVE degree north               104
#> 2 XLONG          XY LONGITUDE, WEST IS NEGATIVE  degree east               104
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

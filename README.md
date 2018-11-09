# eixport
[![Travis-CI Build Status](https://travis-ci.org/atmoschem/eixport.svg?branch=master)](https://travis-ci.org/atmoschem/eixport)[![Build status](https://ci.appveyor.com/api/projects/status/frk36kmayf8yff70?svg=true)](https://ci.appveyor.com/project/Schuch666/eixport)
[![Coverage Status](https://img.shields.io/codecov/c/github/atmoschem/eixport/master.svg)](https://codecov.io/github/atmoschem/eixport?branch=master)
[![DOI](https://zenodo.org/badge/106145968.svg)](https://zenodo.org/badge/latestdoi/106145968)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/eixport)](http://cran.r-project.org/web/packages/eixport) 
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/eixport?color=orange)](http://cran.r-project.org/package=eixport)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00607/status.svg)](https://doi.org/10.21105/joss.00607)
[![cran checks](https://cranchecks.info/badges/worst/eixport)](https://cran.r-project.org/web/checks/check_results_eixport.html)


```{r echo = FALSE}
knitr::include_graphics("https://i.imgur.com/BcZ2tfW.png")
```


## Exporting emissions to atmospheric models, eixport: `r packageVersion("eixport")`

Emissions are mass that affects atmosphere in complex ways, not only physical, but also, in the health of humans, ecosystems, economically, etc.

There are several models whose inputs are emissions, such as [R-Line](https://www.cmascenter.org/r-line/) or [WRF-Chem](https://ruc.noaa.gov/wrf/wrf-chem/). 
This R-Package provide functions to read emissions from [VEIN](https://github.com/ibarraespinosa/vein) and from other models 
in different formats and export the emissions into the appropiate format suitable to other models.


## Install

To install the [CRAN](https://CRAN.R-project.org/package=eixport) version:

```{r eval = FALSE}
install.packages("eixport")
```

To install the development version:

```{r eval = FALSE}
devtools::install_github("atmoschem/eixport")
```

```{r}
library(eixport)
```

## Some functions:

- [to_rline](https://atmoschem.github.io/eixport/reference/to_rline.html): Export emissions to other formats
- [to_wrf](https://atmoschem.github.io/eixport/reference/to_wrf.html):	Combine total/spatial/temporal/split and write emission to file
- [to_brams_spm](https://atmoschem.github.io/eixport/reference/to_brams_spm.html):	inputs for SPM BRAMS
- [wrf_profile](https://atmoschem.github.io/eixport/reference/wrf_profile.html): Create spatial profile for WRF-Chem
- [wrf_create](https://atmoschem.github.io/eixport/reference/wrf_create.html):	Create emission files to the WRF-Chem
- [wrf_plot](https://atmoschem.github.io/eixport/reference/wrf_plot.html): simple plot for emissions
- [wrf_get](https://atmoschem.github.io/eixport/reference/wrf_get.html):	Read variables of emission files
- [wrf_put](https://atmoschem.github.io/eixport/reference/wrf_put.html):	Write variables in emission files
- [to_as4wrf](https://atmoschem.github.io/eixport/reference/to_as4wrf.html): Create WRF-Chem inputs using NCL scrip AS4WRF.ncl.

## Paper accepted on Journal of Open Source Software

https://doi.org/10.21105/joss.00607

```
@article{eixport,
    title = {eixport: An R package to export emissions to atmospheric models},
    journal = {The Journal of Open Source Software},
    author = {Sergio Ibarra-Espinosa and Daniel Schuch and Edmilson {Dias de Freitas}},
    year = {2018},
    doi = {10.21105/joss.00607},
    url = {http://joss.theoj.org/papers/10.21105/joss.00607},
  }
```


## Contributing

Please, read [this](https://github.com/atmoschem/eixport/blob/master/CONTRIBUTING.md) guide.
Contributions of all sorts are welcome, issues and pull requests are the preferred ways of sharing them.
When contributing pull requests, please follow the [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml).
This project is released with a [Contributor Code of Conduct](https://github.com/atmoschem/eixport/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

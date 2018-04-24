---
title: 'eixport: An R package to export emissions to atmospheric models'
tags:
- eixport
- emissions
- R
- air quality model
authors:
- affiliation: '1'
  name: Sergio Ibarra-Espinosa
  orcid: 0000-0002-3162-1905
- affiliation: '1'
  name: Daniel Schuch
  orcid: 0000-0001-5977-4519
- affiliation: '1'
  name: Edmilson Dias de Freitas
  orcid: 0000-0001-8783-2747
date: "23 February 2018"
output:
  pdf_document: default
bibliography: paper.bib
affiliations:
- index: 1
  name: Departamento de Ciências Atmosféricas, Universidade de São Paulo, Brasil
---

# Summary

Emissions are the pollutant mass released into the atmosphere [@pulles2010art]. The origin of the emissions can be human-made or anthropogenic or biogenic. The consequences of this pollution are complex affecting the atmosphere, human health, ecosystems, and infrastructure [@SeinfeldPandis2016]. In fact, pollution caused 9 million premature deaths in 2015, 16% of all deaths worldwide
[@landrigan2017lancet].



An important tool for policy decision is air quality models. They have been used not only to study the impact of different emissions scenarios for policy making but also to understand the dynamics of air pollutants in various parts of the world [@Andradeetal2015]. The inputs for an air quality models are meteorology and emissions. Currently, there are tools for developing emissions inventories such as the VEIN [@vein] and the EmissV models [@emissv].  However, the existing tools for inputting the emissions into the air quality models are not written in a user-friendly way, such as PREP-Chem written in Fortran and C [@freitas2011prep]. Also, as the R language has a growing community, including statistical R packages for model evaluation and validation, such as openair [], air quality modelers already familiar with R would tend to use 'eixport' for inputing data into the appropiate format for each model without the need of deep knowledge in a specific model language. Therefore, we developed **eixport**, a tool for inputting data into atmospheric models using R [@R]. 

eixport imports functions form the R packages sf [@sf] which provides functions for spatial vector data, providing bindings to the GDAL, GEOS, and Proj.4 C++ libraries. Also, eixport import functions from the package ncdf4 [@ncdf4], which interface to Unidata netCDF Format Data Files, and from the raster package [@raster], which provides functions to gridded data.

## Functions and data

eixport count with the following functions:

| Function     | Description                                                   |
|--------------|---------------------------------------------------------------|
| emisco       | Dataset of Emissions from VEIN demo                           |
| emis_opt     | List of WRF emission species                                  |
| rawprofile   | A matrix to temporally distribute emissions                   |
| wrf_create   | Create WRF-Chem inputs based on a WRFinput file               |
| wrf_get      | Reads variables from WRF-Chem inputs                          |
| wrf_grid     | Creates spatial feature (sf) polygon grid from WRFinput file  |
| wrf_plot     | Simple plot from wrf emission file                            |
| wrf_profile  | returns a traffic intensity profile (based on wrf file Times) |
| wrf_put      | Function to write variables in WRF-Chem inputs                |
| to_brams_spm | Produce inputs for BRAMS SPM [@freitas2005simple]             |
| to_rline     | Produce inputs for R-Line [@snyder2013rline]                  |
| to_wrf       | Distribution of emissions for WRF-Chem [@Grelletal2005]       |
| to_as4wrf    | Produce data-frame to be used with NCL AS4WRF [@vara2016]     |

## Examples

The following example creates a directory **EMISS**  and then
create a wrfchem input in that file. The package already counts with wrfinput files required to run eixport and create inputs for WRF-Chem. The line `data(Lights)` load a matrix of night light to spatially distribute the emissions. The perfil argument is used to temporally distribute the emissions. Lastly, the function `to_wrf` in one line reads the 1521983 $t \cdot y^{-1}$ of  CO, spatially distribute it using nighttime traffic matrix Lights, temporally distribute it with the perfil, injecting the array of emissions directly into the wrfchemi file. The colour palette is 
"mpl_inferno" from the R package cptcity [@cpt].

```
library(eixport)
dir.create(file.path(tempdir(), "EMISS"))
wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
          wrfchemi_dir = file.path(tempdir(), "EMISS"))

# get the name of created file
files <- list.files(path = file.path(tempdir(), "EMISS"),
                   pattern = "wrfchemi",
                   full.names = TRUE)

# load end write some data in this emission file
data(Lights)
to_wrf(Lights, files[1], total = 1521983, names = "E_CO")

wrf_plot(files[1], "E_CO")
# [1] "EMISS/wrfchemi_d01_2011-08-01_00:00:00"
# [1] "E_CO"
# [1] "Max value: 26.6966304779053, Min value: 0"
```

The resulting plot can be seen in the Fig. 1.

![WRF-Chem emisisons of CO (t/y)](https://i.imgur.com/BcZ2tfW.png)

The R package eixport is available at the repository  https://github.com/atmoschem/eixport. To ensure the usability of the package, in any commit to GitHub, eixport is installed in Ubuntu via Travis-CI (https://travis-ci.org/atmoschem/eixport) and Windows via Appveyor (https://ci.appveyor.com/project/Schuch666/eixport). Also, eixport is already on CRAN https://CRAN.R-project.org/package=eixport. Moreover, this packages tests functions
with the suite CodeCov (https://codecov.io/) and the r package covr [@covr], achieving
89% of coverage (https://codecov.io/github/atmoschem/eixport).

\pagebreak


# Acknowledgements

The development of eixport was supported by postdoc grans fro the Fundação de Universidade de São Paulo and Fundação Coordenação de Aperfeiçoamento de Pessoal de Nível Superior.


# References

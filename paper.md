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
date: "23 February 2018"
output:
  word_document: default
  pdf_document: default
bibliography: paper.bib
affiliations:
- index: 1
  name: Departamento de Ciências Atmosféricas, Universidade de São Paulo, Brasil
---

# Summary

Emissions are the pollutant mass released into the atmosphere [@pulles2010art]. The origin of the emissions can be human-made or anthropogenic or biogenic. The consequences of this pollution are complex affecting the atmosphere, human health, ecosystems, and infrastructure [@SeinfeldPandis2016]. In fact, pollution caused 9 million premature deaths in 2015, 16% of all deaths worldwide
[@landrigan2017lancet].


An important tool for policy decision is air quality models. They have been used not only to study the impact of different emissions scenarios for policy making but also to understand the dynamics of air pollutants in various parts of the world [@Andradeetal2015]. The inputs for an air quality models are meteorology and emissions. Currently, there are tools for developing emissions inventories such as the VEIN [@Ibarraetal2017] and the EmissV models [@emissv].  However, the existing tools for inputting the emissions into the air quality models are not written with a high-level language, such as PREP-Chem written in Fortran and C [@freitas2011prep]. Therefore, we developed **eixport**, a tool for doing the mentioned task, using R [@R], a high-level programming. 

eixport imports functions form the R packages sf [@sf] which provides functions for spatial vector data, providing bindings to the GDAL, GEOS, and Proj.4 C++ libraries. Also, eixport import functions from the package ncdf4 [@ncdf4], which interface to Unidata netCDF Format Data Files, and from the raster package [@raster], which provides functions to gridded data.

## Functions and data

eixport count with the folllwing functions:

| Function     | Description                                                   |
|--------------|---------------------------------------------------------------|
| emisco       | Dataset of Emissions from VEIN demo                           |
| emis_opt     | List of WRF emission species                                  |
| rawprofile   | A matrix to temporally distribute emissions                   |
| to_brams_spm | Produce inputs for BRAMS SPM [@freitas2005simple]             |
| to_rline     | Produce inputs for R-Line [@snyder2013rline]                  |
| to_wrf       | Distribution of emissions for WRF-Chem [@Grelletal2005]       |
| wrf_create   | Create WRF-Chem inputs based on a WRFinput file               |
| wrf_get      | Reads variables from WRF-Chem inputs                          |
| wrf_grid     | Creates spatial feature (sf) polygon grid from WRFinput file  |
| wrf_plot     | Simple plot from wrf emission file                            |
| wrf_profile  | returns a traffic intensity profile (based on wrf file Times) |
| wrf_put      | Function to write variables in WRF-Chem inputs                |

## Examples

The following example creates a directory **EMISS**  and then
create a wrfchem input in that file. The package already counts with wrfinput files required to run eixport and create inputs for WRF-Chem. The line `data(Lights)` load a matrix of night light to spatially distribute the emissions. The perfil argument is used to temporally distribute the emissions. Lastly, the function `to_wrf` in one line reads the 1521983 $t \cdot y^{-1}$ of  CO, spatially distribute it using nighttime traffic matrix Lights, temporally distribute it with the perfil, injecting the array of emissions directly into the wrfchemi file.

```
library(eixport)
dir.create("EMISS")
wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
          wrfchemi_dir = "EMISS",
          frames_per_auxinput5 = 24)

# get the name of created file
files <- list.files(path = "EMISS",pattern = "wrfchemi",full.names = T)

data(Lights)

perfil <- c(0.010760058, 0.005280596, 0.002883553, 0.002666932,
           0.005781312, 0.018412838, 0.051900411, 0.077834636,
           0.067919758, 0.060831614, 0.055852868, 0.052468599,
           0.050938043, 0.051921718, 0.052756244, 0.052820165,
           0.058388406, 0.072855890, 0.075267137, 0.063246412,
           0.042713523, 0.029108975, 0.022091855, 0.015298458)

plot(perfil,ty = "l",col= "purple",xlab = "Hour",main = "Time profile",
    ylab = "Weight",axes = F,xlim = c(0,24))
axis(2)
axis(1,at = c(0,6,12,18,24),labels = c("00:00","06:00","12:00","18:00","00:00"))

to_wrf(Lights,files[1],total = 1521983,profile = perfil,names = "E_CO")
wrf_plot(files[1])
# [1] "EMISS/wrfchemi_d01_2011-08-01_00:00:00"
# [1] "E_CO"
# [1] "Max value: 26.6966304779053, Min value: 0"
```

The resulting plot can be seen in the Fig. 1.

![WRF-Chem emisisons of CO](https://i.imgur.com/5zfCeWT.png)

The R package eixport is available at the repository  https://github.com/atmoschem/eixport. To ensure the usability of the package, in any commit to GitHub, eixport is installed in Ubuntu via Travis-CI (https://travis-ci.org/atmoschem/eixport) and Windows via Appveyor (https://ci.appveyor.com/project/Schuch666/eixport). Also, eixport already pass the CRAN tests it will send to CRAN soon.

\pagebreak


# Acknowledgements

The development of eixport was supported by postdoc grans fro the Fundação de Universidade de São Paulo and Fundação Coordenação de Aperfeiçoamento de Pessoal de Nível Superior.


# References

# eixport

## Exporting emissions to other models

Emissions are mass that affects atmosphere in complex ways, not only physical,
but also, in the health of humans, ecosystems, economically, etc.

There are several models whose inputs are emissions, such as [R-Line](https://www.cmascenter.org/r-line/) or [WRF-Chem](https://ruc.noaa.gov/wrf/wrf-chem/).

This R-Package provide functions to read emissions from [VEIN](https://github.com/ibarraespinosa/vein) and also other 
in different formats and export the to format suitable to other models.

The first casetof this package will exporting VEIN emissions into the model R-Line.

functions:

- to_rline: Export emissions to other formats
- wrf_create:	Create emission files to the WRF-Chem
- wrf_emission:	Combine total/spacial/temporal/split and write emission to file
- wrf_get:	Funtions to read variables of emission files
- wrf_put:	Funtions to write variables in emission files


```{r eval=F}
# 0.0.2.9000
library(devtools)
install_github("ibarraespinosa/eixport")
library(eixport)
?wrf_emission
```



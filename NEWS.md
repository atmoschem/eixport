NEWS
===========

# eixport 0.4.4 (Release date: 23 feb 2020)
- update wrf_grid.

# eixport 0.4.3 (Release date: 07 feb 2020)
- internal changes on wrf_create, os replaced for separator for minimal structural changes

# eixport 0.4.3 (Release date: 28 jan 2020)
- add argument change_latlon in wrf_grid

# eixport 0.4.3 (Release date: 22 jan 2020)
- add argument 'os' in wrf_create to control the name in the resulting file

# eixport 0.4.2 (Release date: 22 nov 2019)
- added 'time' option to wrf_get to return a POSIXlt object from model time
- wrf_put works with POSIXlt objects for Times/time variable

# eixport 0.4.1 (Release date: 22 nov 2019)
- improves wrf_put, argument mult now can have any length supporting different hourly profiles

# eixport 0.4.0 (Release date: 14 nov 2019)
- adds sfx_explode to split lines with https://github.com/hypertidy/silicate/issues/102
- adds as-raster in wrf_grid

# eixport 0.3.9 (Release date: 24 oct 2019)
- adds get_edgar

# eixport 0.3.8 (Release date: 19 fev 2019)
- wrf_create update: windowns files has '%3A' on ':' to autorename when transferred with WinSCP
- wrf_profile update
- to_wrf update

# eixport 0.3.7 (Release date: 01 nov 2018)
- added wrf_add function

# eixport 0.3.6 (Release date: 05 out 2018)

- update wrf_create for WRF 4.0.1 and detailed documentation for this function ( fix #36 )
- update emis_opt for WRF 4.0.1

# eixport 0.3.5 (Release date: 21 Jun 2018)

- Add to_munich. Fix #18.

# eixport 0.3.4 (Release date: 26 May 2018)

### eixport 0.3.4 (Release date: 26 May 2018)

- Fix to_as4wrf when sdf is list..


# eixport 0.3.3 (Release date: 23 April 2018)

### eixport 0.3.3 (Release date: 23 April 2018)

- Added data gCO.
- Added function as4wrf.
- Added tests. 

### eixport 0.3.2 (Release date: 29 March 2018)

- Documentation.

### eixport 0.3.1 (Release date: 4 March 2014)

- Documentation.

## eixport 0.3.0 (Release date: 23 February 2018)

### eixport 0.3.0 (Release date: 23 February 2018)

- Remove regrid.
- Prepare submissions to JOSS
- exemple fix
- remove warning message

## eixport v0.2.10 (Release date: 19 January 2018)

- Fix wrf_grid. add regrid.

## eixport v0.2.9 (Release date: 15 January 2018)

- Adding data rawprofile. Deleting wrf_temporal.R and adding wrf_profile.R
- Adding option in wrf_grid to read wrfinput from real or geo_em files from geogrid.  

## eixport v0.2.8 (Release date: 15 January 2018)

- Adding wrf_grid to create spatial grids bassed on wrf files.

## eixport v0.2.7 (Release date: 26 December 2017)

- Fix #6: Incuding functions to SPM BRAMS.
- Improve documentation of emisco, example of to_rline and Readme.md. 

## eixport v0.2.6 (Release date: 24 November 2017)

- Adding Lights data

## eixport v0.2.5 (Release date: 24 November 2017)

- Adding to_smoke and to_cmaq initial functions

## eixport v0.2.4 (Release date: 24 November 2017)

- some fixes
- add the _style_emissions = 1 to wrf_create
- add the internal kemit to the wrf_emission function

## eixport v0.2.0.0000 (Release date: 24 November 2017)

- new funtions plot wrf emission

## eixport v0.1.0.0000 (Release date: 24 November 2017)

- new set to funtions to work with WRF-chem emissions

## eixport v0.0.2.9000 (Release date: 15 November 2017)

- improving format for to_rline output. Deleting argument x in to_rline. 
  Adding Daniel Schuch

## vein v0.0.1.9000 (Release date: 29 October 2017)

- testing first function: to_rline

## eixport v0.0.0.9000 (Release date: 8 October 2017)

- First commit

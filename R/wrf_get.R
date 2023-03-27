#' Function to read variables of emission files
#'
#' @description Read a variable
#'
#' @param file name of file interactively (default) or specified
#' @param name name of the variable (any variable) or time to return a POSIXlt object from model
#' @param as_raster return a raster instead of an array
#' @param raster_crs crs of outputif as_raster is TRUE, see details
#' @param raster_lev level for rasters from a 4D variable
#' @param k multiplier
#' @param verbose display additional information
#' @param ... additional parameters passed to wrf_raster
#'
#' @details wrf_get can return a raster object with the option as_raster = TRUE,
#' raster_crs can be used to specify the output crs of the raster object,
#' raster_crs = 'latlon' can be especifyed to use latlon option in wrf_raster.
#' If raster_crs is 'WRF' (default), the output projection is equivalent to
#' the WRF grid.
#'
#' @format array or raster object
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom raster raster brick flip
#'
#' @export
#'
#' @author Daniel Schuch
#'
#' @seealso \code{\link{wrf_plot}} and \code{\link{wrf_put}}
#'
#' @examples {
#'
#' # create the folder and emission file
#' dir.create(file.path(tempdir(), "EMISS"))
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'          wrfchemi_dir = file.path(tempdir(), "EMISS"))
#'
#' # get the name of created file
#' files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                    pattern = "wrfchemi",
#'                    full.names = TRUE)
#'
#' # open, put some numbers and write
#' CO <- wrf_get(file = files[1],
#'               name = "E_CO")
#'
#' CO[] = rnorm(length(CO))
#'
#' wrf_put(file = files[1],
#'         name = "E_CO",
#'         POL = CO)
#'
#' COr <- wrf_get(file = files[1],
#'                name = "E_CO",
#'                as_raster = TRUE)
#'
#'}
wrf_get <- function(file = file.choose(),
                    name = NA,
                    as_raster = FALSE,
                    raster_crs = 'WRF',
                    raster_lev = 1,
                    k = NA,
                    verbose = FALSE, ...){

  if(!is.na(name)){
    if(name == 'time'){
      wrfchem <- ncdf4::nc_open(file)                                                     # nocov
      if(verbose)                                                                         # nocov
        cat(paste0('reading Times from ', file,'\n'))                                     # nocov
      TIME   <- ncvar_get(wrfchem,'Times')                                                # nocov
      TIME   <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE) # nocov
      if(verbose)                                                                         # nocov
        cat('returning Times in POSIXct\n')                                               # nocov
      return(TIME)                                                                        # nocov
    }
  }
  if(verbose){
    if(missing(k)){                                                       # nocov
      cat(paste0('reading ',name,' from ', file,'\n'))                    # nocov
    }else{
      cat(paste0('reading ',name,' from ', file,' k = ',k,'\n'))          # nocov
    }
  }

  wrfchem <- ncdf4::nc_open(file)                                       # iteractive
  if(is.na(name)){                                                      # nocov start
    name  <- menu(names(wrfchem$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(wrfchem, names(wrfchem$var)[name])
    name  <- names(wrfchem$var)[name]                                   # nocov end
  }
  if(as_raster){
    if(raster_crs == 'latlon' |
       raster_crs == 'lonlat' |
       raster_crs == 'latlong'|
       raster_crs == 'longlat'){
      r <- wrf_raster(file    = file,           # nocov
                      name    = name,           # nocov
                      latlon  = TRUE,           # nocov
                      level   = raster_lev,     # nocov
                      verbose = verbose,        # nocov
                      ...)                      # nocov
    }else{
      r <- wrf_raster(file    = file,
                      name    = name,
                      latlon  = FALSE,
                      level   = raster_lev,
                      verbose = verbose,
                      ...)
      if(raster_crs != 'WRF'){
        r <- projectRaster(r, crs=raster_crs)
      }
    }
    if(!missing(k)){
      r <- k * r                                     # nocov
    }
    return(r)
  } else {
    POL   <- ncvar_get(wrfchem,name)
    ncdf4::nc_close(wrfchem)
    if(!missing(k)){
      POL <- k * POL                                 # nocov
    }
    return(POL)
  }
}

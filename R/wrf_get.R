#' Function to read variables of emission files
#'
#' @description Read a variable
#'
#' @param file name of file interactively (default) or specified
#' @param name name of the variable (any variable) or time to return a POSIXlt object from model
#' @param as_raster return a raster instead of an array
#' @param raster_crs crs to use if as_raster is TRUE
#' @param raster_lev level for rasters from a 4D variable
#' @param verbose display additional information
#'
#' @format array or raster object
#'
#' @import ncdf4
#' @importFrom raster raster brick flip
#' @importFrom sp CRS
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
#' CO <- wrf_get(file = files[1], name = "E_CO")
#' CO[] = rnorm(length(CO))
#' wrf_put(file = files[1], name = "E_CO", POL = CO)
#' COr <- wrf_get(file = files[1], name = "E_CO", as_raster = TRUE)
#'
#'}
wrf_get <- function(file = file.choose(), name = NA, as_raster = FALSE,
                    raster_crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                    raster_lev = 1, verbose = F){
  if(name == 'time'){
    wrfchem <- ncdf4::nc_open(file)                                                     # nocov
    if(verbose)                                                                         # nocov
      cat(paste0('reading Times from ', file,'\n'))                                     # nocov
    TIME   <- ncvar_get(wrfchem,'Times')                                                # nocov
    TIME   <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE) # nocov
    cat('returning Times in POSIXct\n')                                                 # nocov
    return(TIME)                                                                        # nocov
  }
  if(verbose)
    cat(paste0('reading ',name,' from ', file,'\n'))                     # nocov

  wrfchem <- ncdf4::nc_open(file)                                       # iteractive
  if(is.na(name)){                                                      # nocov start
    name  <- menu(names(wrfchem$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(wrfchem, names(wrfchem$var)[name])
    name  <- names(wrfchem$var)[name]                                   # nocov end
  }else{
    POL   <- ncvar_get(wrfchem,name)
  }
  if(as_raster){
    if(length(dim(POL)) >= 5)                                                  # nocov start
      stop("images with 5D or more not suported")

    if(length(dim(POL)) == 4){
      cat(paste0("4D images not supported, making a 3D RasterBrick using level ",
                 raster_lev," of the file\n"))
      POL <- POL[,,raster_lev,,drop = T]
    }                                                                          # nocov end

    lat    <- ncdf4::ncvar_get(wrfchem, varid = "XLAT")
    lon    <- ncdf4::ncvar_get(wrfchem, varid = "XLONG")
    time   <- ncdf4::ncvar_get(wrfchem, varid = "Times")
    r.lat  <- range(lat)
    r.lon  <- range(lon)
    n.lat  <- ncdf4::ncatt_get(wrfchem, varid = 0,
                               attname = "SOUTH-NORTH_PATCH_END_UNSTAG")$value
    n.lon  <- ncdf4::ncatt_get(wrfchem, varid = 0,
                               attname = "WEST-EAST_PATCH_END_UNSTAG")$value

    n      <- length(time)
    if(n == 1){
      r <- raster::raster(x = t(POL),
                          xmn=r.lon[1],
                          xmx=r.lon[2],
                          ymn=r.lat[1],
                          ymx=r.lat[2])
      r <- raster::flip(r,2)
    }
    if(n > 1){                                        # for emissions in 2D+time
      r <- raster::brick(x = aperm(POL, c(2, 1, 3)),  # nocov start
                         xmn = r.lon[1],
                         xmx = r.lon[2],
                         ymn = r.lat[1],
                         ymx = r.lat[2])
      r <- raster::flip(r,2)                          # nocov end
    }
    raster::crs(r)   <- sp::CRS(raster_crs)
    names(r) <- paste(name,time,sep="_")
    ncdf4::nc_close(wrfchem)
    return(r)
  } else {
    ncdf4::nc_close(wrfchem)
    return(POL)
  }
}

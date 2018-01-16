#' Function to read variables of emission files
#'
#' @description Read a variable
#'
#' @param file name of file interactively (default) or specified
#' @param as_raster return a raster instead of a array
#' @param name name of the variable (any variable)
#'
#' @format array or raster object
#'
#' @import ncdf4
#' @import raster
#'
#' @importFrom raster raster brick flip
#'
#' @export
#'
#' @author Daniel Schuch
#'
#' @seealso \code{\link{wrf_plot}} and \code{\link{wrf_put}}
#'
#' @examples \dontrun{
#' # Do not run
#'
#' # create the folder and emission file
#'dir.create("EMISS")
#'wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'          wrfchemi_dir = "EMISS")
#'
#'# get the name of created file
#'files <- list.files(path = "EMISS",pattern = "wrfchemi",full.names = T)
#'
#'# open, put some numbers and write
#'CO <- wrf_get(file = files[1],name = "E_CO")
#'CO[] = rnorm(length(CO))
#'wrf_put(file = files[1], name = "E_CO",POL = CO)
#'}
wrf_get <- function(file = file.choose(), name = NA, as_raster = F){
  wrfchem <- ncdf4::nc_open(file)
  if(is.na(name)){
   name  <- menu(names(wrfchem$var),title = "Chose the variable:")
    name  <- names(wrfchem$var)[name]
    POL   <- ncdf4::ncvar_get(wrfchem,name)
  }else{
    POL   <- ncdf4::ncvar_get(wrfchem,name)
  }
  if(as_raster){
    lat    <- ncdf4::ncvar_get(wrfchem, varid = "XLAT")
    lon    <- ncdf4::ncvar_get(wrfchem, varid = "XLONG")
    time   <- ncdf4::ncvar_get(wrfchem, varid = "Times")
    dx     <- ncdf4::ncatt_get(wrfchem, varid = 0,attname = "DX")$value #/ 1000 # km
    r.lat  <- range(lat)
    r.lon  <- range(lon)
    n.lat  <- ncdf4::ncatt_get(wrfchem,varid = 0,attname = "SOUTH-NORTH_PATCH_END_UNSTAG")$value
    n.lon  <- ncdf4::ncatt_get(wrfchem,varid = 0,attname = "WEST-EAST_PATCH_END_UNSTAG")$value

    n      <- length(time)
    if(n == 1){
      r <- raster::raster(x=t(POL),xmn=r.lon[1],xmx=r.lon[2],ymn=r.lat[1],ymx=r.lat[2])
      r <- raster::flip(r,2)
    }
    if(n >= 1){
      r <- raster::brick(x=aperm(POL,c(2,1,3)),xmn=r.lon[1],xmx=r.lon[2],ymn=r.lat[1],ymx=r.lat[2])
      r <- raster::flip(r,2)
    }
    raster::crs(r)   <- "+proj=longlat +ellps=GRS80 +no_defs"
    names(r) <- paste(name,time,sep="_")
    ncdf4::nc_close(wrfchem)
    return(r)
  }
  ncdf4::nc_close(wrfchem)
  return(POL)
}

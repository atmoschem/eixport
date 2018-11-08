#' Function to add values for variables on emission files
#'
#' @description Add values to a variable in a netCDF file, the main use is to combine different
#' emissions like top-down emission (EmissV emissions) and inventary emission (sush as EDGAR,
#' GAINS, RETRO, etc).
#'
#' @param file name of file interactively (default) or specified
#' @param name name of the variable (any variable)
#' @param POL  variable to be written
#'
#' @import ncdf4
#'
#' @export
#'
#' @author Daniel Schuch
#'
#' @examples{
#' # create the folder and emission file
#' dir.create(file.path(tempdir(), "EMISS"))
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = file.path(tempdir(), "EMISS"))
#'
#' # get the name of created file
#' files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                     pattern = "wrfchemi",
#'                     full.names = TRUE)
#'
#' # open, put some numbers and write
#' CO <- wrf_get(file = files[1], name = "E_CO")
#' CO[] = rnorm(length(CO),mean = 5,sd = 1)
#' wrf_put(file = files[1], name = "E_CO", POL = CO)
#' # open, put some different numbers and write
#' CO[] = rnorm(length(CO),mean = 10,sd = 1)
#' wrf_add(file = files[1], name = "E_CO", POL = CO)
#' }

wrf_add <- function(file = file.choose(), name = NA,POL){
  wrfchem <- ncdf4::nc_open(file,write = T)                             # iteractive
  if(is.na(name)){                                                      # nocov start
    name  <- menu(names(wrfchem$var), title = "Choose the variable:")
    POL0   <- ncdf4::ncvar_get(wrfchem, names(wrfchem$var)[name])
    name  <- names(wrfchem$var)[name]                                   # nocov end
  }else{
    POL0   <- ncvar_get(wrfchem,name)
  }
  ncdf4::ncvar_put(wrfchem,varid = name,POL0+POL)
  ncdf4::nc_close(wrfchem)
}

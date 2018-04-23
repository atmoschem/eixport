#' Function to write variables in emission files
#'
#' @description Extract variable
#'
#' @param file name of file interactively (default) or specified
#' @param name name of the variable (any variable)
#' @param POL input
#'
#' @export
#'
#' @author Daniel Schuch
#'
#' @import ncdf4
#'
#' @seealso \code{\link{wrf_plot}} and \code{\link{wrf_get}}
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
#' CO[] = rnorm(length(CO))
#' wrf_put(file = files[1], name = "E_CO", POL = CO)
#' }
wrf_put <- function(file = file.choose(),name = NA,POL){
  wrfchem <- ncdf4::nc_open(file,write = T)
  ncdf4::ncvar_put(wrfchem,varid = name,POL)
  ncdf4::nc_close(wrfchem)
}

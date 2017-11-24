#' Funtion to write variables in emission files
#'
#' @description Extract variable
#'
#' @param file name of file interactively (defoult) or specified
#' @param name name of the variable (any variable)
#' @param POL input
#'
#' @export
#'
#' @author Daniel Schuch
#'
#' @seealso wrf_plot and \code{\link{wrf_get}}
#'
#' @examples \dontrun{
#' # Do not run
#'
#' # create the folder and emission file
#'dir.create("EMISS")
#'wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'            rfchemi_dir = "EMISS")
#'
#'# get the name of created file
#'files <- list.files(path = "EMISS",pattern = "wrfchemi",full.names = T)
#'
#'# open, put some numbers and write
#'CO <- wrf_get(file = files[1],name = "E_CO")
#'CO[] = rnorm(length(CO))
#'wrf_put(file = files[1],name = "E_CO",POL = CO)
#'}

wrf_put <- function(file = file.choose(),name = NA,POL){
  wrfchem <- nc_open(file,write = T)
  ncvar_put(wrfchem,varid = name,POL)
  nc_close(wrfchem)
}

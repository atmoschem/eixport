#' Funtion to read variables of emission files
#'
#' @description Write variable
#'
#' @param file name of file interactively (defoult) or specified
#' @param name name of the variable (any variable)
#'
#' @export
#'
#' @author Daniel Schuch
#'
#' @seealso wrf_plot and \code{\link{wrf_put}}
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

wrf_get <- function(file = file.choose(),name = NA){
  wrfchem <- nc_open(file)
  if(is.na(name)){
    name  <- menu(names(wrfchem$var),title = "Chose the variable:")
    POL   <- ncvar_get(wrfchem,names(wrfchem$var)[name])
  }else{
    POL   <- ncvar_get(wrfchem,name)
  }
  nc_close(wrfchem)
  return(POL)
}

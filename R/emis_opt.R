#' List of WRF emission species
#'
#' @description  Emission package definitions from WRF 3.9.1.1
#'
#' @format A list of emision variables names, same number than emis_opt in namelist.
#'
#' @examples \dontrun{
#' # Do not run
#' dir.create("EMISS")
#' data(emis_opt)
#' names(emis_opt)
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'            wrfchemi_dir = "EMISS",
#'            variaveis    = emis_opt["eradmsorg"],
#'            n_aero       = 17)
#' }
#'
#' @note
#' look to the number of aerosol of the emis_opt in wrf domumentation / code.
#'
#' @seealso \code{\link{to_wrf}}
#'
#' @author Daniel Schuch
#'
#' @source \url{http://www2.mmm.ucar.edu/wrf/users/download/get_source.html}
#' @usage data(emis_opt)
#' @docType data
"emis_opt"

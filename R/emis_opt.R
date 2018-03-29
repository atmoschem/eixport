#' List of WRF emission species
#'
#' @description  Emission package definitions from WRF 3.9.1.1
#'
#' @format A list of emision variables names, same number as emis_opt in namelist.
#'
#' @examples \dontrun{
#' # Do not run
#' data(emis_opt)
#' names(emis_opt)
#' emis_opt["eradm"]
#' }
#'
#' @note
#' look to the number of aerosol of the emis_opt in WRF domumentation / code.
#'
#' @seealso \code{\link{wrf_create}}
#'
#' @author Daniel Schuch
#'
#' @source \url{http://www2.mmm.ucar.edu/wrf/users/download/get_source.html}
#' @usage data(emis_opt)
#' @docType data
"emis_opt"

#' List of WRF emission species
#'
#' @description  Emission package definitions from WRF 4.0.1, for use in wrf_create function.
#'
#' @format A list of emision variables names, same number as emis_opt in namelist.
#'
#' @examples
#' data(emis_opt)
#' names(emis_opt)
#' emis_opt[["eradm"]]
#'
#' @note
#' look to the number of aerosol of the emis_opt in WRF domumentation / code.
#'
#' @seealso \code{\link{wrf_create}}
#'
#' @author Daniel Schuch
#'
#' @source \url{https://github.com/wrf-model/WRF}
#' @usage data(emis_opt)
#' @docType data
"emis_opt"

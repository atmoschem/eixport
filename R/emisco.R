#' Emissions from VEIN demo
#'
#' Emissions iwth units for R-LINE
#'
#' @format A sf object of lines with 288 rows and 15 variables:
#' \describe{
#'   \item{ldv}{Light Duty Vehicles (1/h)}
#'   \item{hdv}{Heavy Duty Vehicles (1/h)}
#'   \item{lkm}{Length of the link (km)}
#'   \item{ps}{Peak Speed (km/h)}
#'   \item{ffs}{Free Flow Speed (km/h)}
#'   \item{tstreet}{Type of street}
#'   \item{lanes}{Number of lanes per link}
#'   \item{capacity}{Capacity of vehicles in each link (1/h)}
#'   \item{tmin}{Time for travelling each link (min)}
#'   \item{V8}{Emissions}
#'   \item{xmin}{Initial x coordinates}
#'   \item{xmax}{Ending x coordinates}
#'   \item{ymin}{Initial y coordinates}
#'   \item{ymax}{Ending y coordinates}
#'   \item{geometry}{geometry column of the sf object}
#'   data(emisco)
##' }
#' @source \url{https://github.com/ibarraespinosa/vein}
#' @usage data(emisco)
#' @docType data
"emisco"

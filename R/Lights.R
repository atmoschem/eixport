#' Spatial distribution example
#'
#' @description Spatial distribution for vehicular emissions based on an image of persistent lights of the
#' Defense Meteorological Satellite Program (DMSP) for 5 Brazilian states (Sao Paulo, Rio de Janeiro, Mato Grosso, and Santa Catarina e Parana).
#'
#' @format A matrix of spatial distribution
#'
#' @seealso \code{\link{to_wrf}}
#'
#' @examples \dontrun{
#'
#' dir.create(file.path(tempdir(), "EMISS"))
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'            wrfchemi_dir = file.path(tempdir(), "EMISS"),
#'            frames_per_auxinput5 = 24)
#'
#' # get the name of created file
#' files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                     pattern = "wrfchemi",
#'                     full.names = TRUE)
#'
#' data(Lights)
#'
#' perfil <- c(0.010760058, 0.005280596, 0.002883553, 0.002666932,
#'            0.005781312, 0.018412838, 0.051900411, 0.077834636,
#'            0.067919758, 0.060831614, 0.055852868, 0.052468599,
#'            0.050938043, 0.051921718, 0.052756244, 0.052820165,
#'            0.058388406, 0.072855890, 0.075267137, 0.063246412,
#'            0.042713523, 0.029108975, 0.022091855, 0.015298458)
#'
#' plot(perfil,
#'      ty = "l",
#'      col= "purple",
#'      xlab = "Hour",
#'      main = "Time profile",
#'      ylab = "Weight",
#'      axes = FALSE,
#'      xlim = c(0, 24))
#' axis(2)
#' axis(1,
#'      at = c(0, 6, 12, 18, 24),
#'      labels = c("00:00","06:00","12:00","18:00","00:00"))
#'
#' to_wrf(Lights,
#'        files[1],
#'        total = 1521983,
#'        profile = perfil,
#'        name = "E_CO")
#'}
#'
#' @author Daniel Schuch
#'
#' @source \url{https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html}
#'
#' @details  \url{https://en.wikipedia.org/wiki/Defense_Meteorological_Satellite_Program}
#'
#' @usage data(Lights)
#' @docType data
"Lights"

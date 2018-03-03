#' Combine total/spasial/temporal/split and write emission to file
#'
#' @description Function to expand, split and write emissions. The input is expand into
#' time by profile and split between variables with diferent weights.
#'
#' @param x matrix or array of emissions of spacial weights
#' @param file emission file name
#' @param total total of emited specie
#' @param norm if the spacial weights need to be normalized
#' @param profile temporal profile to expand the emissions
#' @param names species to be write
#' @param weights weight of eath specie
#' @param verbose display adicional information
#'
#' @note length(profile) must be the number of times in the emission
#' file (value of frames_per_auxinput5 if wrf_create() was used to
#' create this file).
#'
#' @note total is a additional way to calculate or correct the total emissions
#'
#' @note sum(profile) = 1 and sum(weights) = 1 to conserve mass
#'
#' @note names and weights must have the same length
#'
#' @author Daniel Schuch
#'
#' @import ncdf4
#'
#' @export
#'
#' @seealso \code{\link{wrf_create}}, \code{\link{wrf_get}},\code{\link{wrf_profile}}  and \code{\link{wrf_plot}}
#'
#' @examples \dontrun{
#' dir.create(file.path(tempdir(), "EMISS"))
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = file.path(tempdir(), "EMISS"),
#'           frames_per_auxinput5 = 24)
#'
#' # get the name of created file
#' files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                    pattern = "wrfchemi",
#'                    full.names = TRUE)
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
#' plot(perfil, ty = "l", col= "purple", xlab = "Hour", main = "Time profile",
#'     ylab = "Weight", axes = FALSE, xlim = c(0, 24))
#' axis(2)
#' axis(1, at = c(0, 6, 12, 18, 24),
#'     labels = c("00:00","06:00","12:00","18:00","00:00"))
#'
#'to_wrf(Lights, files[1], total = 1521983, profile = perfil, names = "E_CO")
#'}
to_wrf <- function(x, file = file.choose(), total = NA, norm = F,
                   profile = 1, names = NA, weights = 1, verbose = T){

  if(is.matrix(x)){
    kemit <- 1
  }else{
    kemit <- dim(x)[3]
  }
  wrf          <- ncdf4::nc_open(file)
  g_atributos  <- ncdf4::ncatt_get(wrf,0)
  if(is.matrix(x)){
    VAR          <- array(0, c(g_atributos$`WEST-EAST_PATCH_END_UNSTAG`,
                              g_atributos$`SOUTH-NORTH_PATCH_END_UNSTAG`,
                              length(profile)))
  }else{
    VAR          <- array(0, c(g_atributos$`WEST-EAST_PATCH_END_UNSTAG`,
                              g_atributos$`SOUTH-NORTH_PATCH_END_UNSTAG`,
                              kemit,
                              length(profile)))
  }

  ncdf4::nc_close(wrf)

  if(norm)
    x <- x / sum(x)

  if(!is.na(total))
    x <- total * x

  for(i in 1:length(names)){
    for(j in 1:length(profile)){
      if(is.matrix(x)){
        VAR[,,j] = profile[j] * x
      }else{
        VAR[,,,j] = profile[j] * x
      }
    }
    if(verbose)
      print(paste("writing emissions:", names[i],"weight", weights[i]))
    wrf_put(file,name = names[i],weights[i]*VAR)
  }
}

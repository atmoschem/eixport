#' Combine total/spacial/temporal/split and write emission to file
#'
#' @description Function to espand, split and write emissions. The input is expand into
#' time by profile and split betwen variables with diferent weights.
#'
#' @param x matrix of emissions of spacial weights
#' @param file emission file name
#' @param total total of emited specie
#' @param profile temporal profile to expand the emissions
#' @param names species to be write
#' @param weights weight of eath specie
#'
#' @note length(profile) must be the number of times in the emission
#' file (value of frames_per_auxinput5 if wrf_create() was used to
#' create this file).
#'
#' @note total is a aditional way to calcule or correct the total emissions
#'
#' @note sum(profile) = 1 and sum(weights) = 1 to conserve mass
#'
#' @note names and weights must have the same length
#'
#' @author Daniel Schuch
#'
#' @export
#'
#' @seealso \code{\link{wrf_create}}, \code{\link{wrf_get}} and \code{\link{wrf_plot}}
#'
#' @examples \dontrun{
#' # Do not run
#'
#'# create the folder and emission file
#'dir.create("EMISS")
#'wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = "EMISS",
#'           frames_per_auxinput5 = 24)
#'
#'# get the name of created file
#'files <- list.files(path = "EMISS",pattern = "wrfchemi",full.names = T)
#'
#'# open, put some numbers and write
#'CO <- wrf_get(file = files[1],name = "E_CO")
#'CO <- CO[,,1]
#'CO[] = rnorm(length(CO))
#'
#'perfil <- c(0.010760058, 0.005280596, 0.002883553, 0.002666932,
#'            0.005781312, 0.018412838, 0.051900411, 0.077834636,
#'            0.067919758, 0.060831614, 0.055852868, 0.052468599,
#'            0.050938043, 0.051921718, 0.052756244, 0.052820165,
#'            0.058388406, 0.072855890, 0.075267137, 0.063246412,
#'            0.042713523, 0.029108975, 0.022091855, 0.015298458)
#'
#'wrf_emission(CO,files[1],profile = perfil,names = "E_CO")
#'}


wrf_emission <- function(x,file = file.choose(),total = NA,profile = 1,names = NA,weights = 1){
  wrf          <- nc_open(file)
  g_atributos  <- ncatt_get(wrf,0)
  VAR          <- array(0,c(g_atributos$`WEST-EAST_PATCH_END_UNSTAG`,
                            g_atributos$`SOUTH-NORTH_PATCH_END_UNSTAG`,
                            length(profile)))
  nc_close(wrf)

  if(!is.na(total))
    x <- total * x / sum(x)

  for(i in 1:length(names)){
    for(j in 1:length(profile)){
      VAR[,,j] = profile[j] * x
    }
    wrf_put(file,name = names[i],weights[i]*VAR)
  }
}

#' Create a spatial profile from a wrf emission file and a data frame with
#'
#' @description returns a traffic intensity profile (based on wrf file Times)
#' and a traffic intensity data frame
#'
#' @param x data.frame of intenticy of traffic by hours (rows) and weekdays
#' (columns)
#' @param file emission file name
#' @param verbose display additional information
#'
#' @format a numeric vector
#'
#' @author Daniel Schuch
#'
#' @import ncdf4
#'
#' @export
#'
#' @seealso \code{\link{wrf_create}} and \code{\link{to_wrf}}
#'
#' @examples \dontrun{
#' # Do not run
#'
#' # Profile based on Sao Paulo tunnel experiments
#' data(rawprofile)
#' rawprofile  <- matrix(rawprofile, nrow = 24, byrow = TRUE)
#' rawprofile <- as.data.frame(rawprofile)
#' names(rawprofile) <- c("Sunday","Monday","Tuesday","Wednesday","Thursday",
#'                        "Friday","Saturday")
#' row.names(rawprofile) <- c("00:00","01:00","02:00","03:00","04:00","05:00",
#'                            "06:00","07:00","08:00","09:00","10:00","11:00",
#'                            "12:00","13:00","14:00","15:00","16:00","17:00",
#'                            "18:00","19:00","20:00","21:00","22:00","23:00")
#'
#' print(rawprofile)
#'
#' # create the folder and emission file
#' dir.create(file.path(tempdir(), "EMISS"))
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = file.path(tempdir(), "EMISS"),
#'           frames_per_auxinput5 = 24)
#'
#' files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                     pattern = "wrfchemi",
#'                     full.names = TRUE)
#'
#' profile <- wrf_profile(rawprofile,files[1])
#'
#' plot(profile, ty="l", lty = 2, axe = FALSE,
#'      main = "Traffic Intensity for Sao Paulo", xlab = "hour")
#' axis(2)
#' axis(1, at = 0.5 + c(0, 6, 12, 18, 24),
#'     labels = c("00:00","06:00","12:00","18:00", "00:00"))
#'}
wrf_profile <- function(x,file,verbose = T){
  x       <- as.data.frame(x)
  times   <- wrf_get(file,"Times")
  profile <- vector(mode = "numeric",length = length(times))

  for(i in 1:length(profile)){
    data     <- times[i]
    data     <- unlist(strsplit(data,"_"))
    hora     <- unlist(strsplit(data[2],":"))
    hora     <- as.numeric(hora[1])
    s        <- as.POSIXlt(as.Date(data[1]))$wday
    dia      <- weekdays(as.Date(data[1]))
    profile[i] <- as.numeric(x[hora+1,s+1])
    if(verbose){
      print(unlist(strsplit(times[i],"_")))
      print(paste0("Weekday: ",dia," Traffic Intensity: ",profile[i]))
    }
  }
  return(profile)
}

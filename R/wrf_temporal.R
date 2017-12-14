#' Traffic intensity profile for WRF
#'
#' @description returns a traffic intensity profile (based on wrf file Times) and a traffic intensity data.frame
#'
#' @param x data.frame of intenticy of traffic by hours (rows) and weekdays (columns)
#' @param file emission file name
#' @param verbose display adicional information
#'
#' @format a numeric vector
#'
#' @author Daniel Schuch
#'
#' @export
#'
#' @seealso \code{\link{wrf_create}} and \code{\link{to_wrf}}
#'
#' @examples \dontrun{
#' # Do not run
#'
#' # Profile based on Sao Paulo tunnel experiments
#' raw.profile <- matrix(c(0.36327310, 0.26651755, 0.17556734, 0.21101505, 0.34700058, 0.27672087, 0.5221281,
#'                         0.16184564, 0.13079591, 0.10256088, 0.11337991, 0.22957453, 0.15278581, 0.3029328,
#'                         0.09860270, 0.07142319, 0.06165967, 0.07019175, 0.11760197, 0.07731648, 0.1645724,
#'                         0.07300646, 0.06605765, 0.05259983, 0.06078008, 0.08426529, 0.06579377, 0.1229675,
#'                         0.07450177, 0.14319821, 0.09130205, 0.08760775, 0.11637054, 0.10432007, 0.1669473,
#'                         0.10687090, 0.45607047, 0.25904099, 0.27188309, 0.27038778, 0.26449448, 0.2875399,
#'                         0.19588600, 1.28552939, 0.95700033, 0.94925988, 0.86992033, 0.91715463, 0.6415772,
#'                         0.30451610, 1.92789827, 1.71160564, 1.71503606, 1.48924380, 1.55301450, 1.0928099,
#'                         0.42387726, 1.68231510, 1.57535624, 1.57693951, 1.46804554, 1.45986530, 1.3566007,
#'                         0.57595939, 1.50674776, 1.43075067, 1.44447237, 1.43462089, 1.37234551, 1.5984896,
#'                         0.70631550, 1.38342841, 1.28456184, 1.30619989, 1.40480259, 1.32088915, 1.7467015,
#'                         0.84837023, 1.29960293, 1.22474932, 1.26565052, 1.40506647, 1.35985524, 1.8118796,
#'                         0.87607750, 1.26169234, 1.19281998, 1.26037295, 1.42318615, 1.33944862, 1.7777513,
#'                         0.80966802, 1.28605715, 1.24964188, 1.34287904, 1.33619411, 1.44394461, 1.7450303,
#'                         0.83570405, 1.30672765, 1.30760725, 1.45731447, 1.46382348, 1.60068860, 1.7344751,
#'                         0.89929884, 1.30831092, 1.33549044, 1.52064537, 1.39926114, 1.69427760, 1.7377296,
#'                         0.99552663, 1.44623156, 1.49997487, 1.71978588, 1.50190998, 1.96492926, 1.7600714,
#'                         1.07803272, 1.80457892, 1.86324797, 2.07417506, 1.76130281, 2.29257872, 1.7653489,
#'                         1.13195195, 1.86430349, 1.93080093, 2.13530698, 1.74124802, 2.26707044, 1.6773894,
#'                         0.99183232, 1.56656028, 1.64783494, 1.85198914, 1.48722073, 2.06907341, 1.5187103,
#'                         0.79207610, 1.05797793, 1.11013797, 1.30716745, 1.05639466, 1.67958835, 1.1474328,
#'                         0.63823478, 0.72100475, 0.79858511, 0.95972707, 0.75698022, 1.40471463, 0.8726471,
#'                         0.44771431, 0.54719660, 0.59531050, 0.70657938, 0.57639919, 1.09720791, 0.7389485,
#'                         0.27144329, 0.37892991, 0.41085924, 0.52151240, 0.44173306, 0.83104220, 0.6489658),
#'                       nrow = 24,ncol = 7,byrow = T)
#' raw.profile <- as.data.frame(raw.profile)
#' names(raw.profile) <- c("Sunday","Monday","Tuesday","Wednesday","Thursday",
#'                         "Friday","Saturday")
#' row.names(raw.profile) <- c("00:00","01:00","02:00","03:00","04:00","05:00",
#'                             "06:00","07:00","08:00","09:00","10:00","11:00",
#'                             "12:00","13:00","14:00","15:00","16:00","17:00",
#'                             "18:00","19:00","20:00","21:00","22:00","23:00")
#'
#' print(raw.profile)
#'
#' # create the folder and emission file
#' dir.create("EMISS")
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = "EMISS",
#'           frames_per_auxinput5 = 24)
#'
#' files <- list.files(path = "EMISS",pattern = "wrfchemi",full.names = T)
#'
#' profile <- wrf_profile(raw.profile,files[1])
#'
#' plot(profile,ty="l",lty = 2,axe=F,main = "Traffic Intensity for Sao Paulo",xlab = "hour")
#' axis(2)
#' axis(1,at=0.5+c(0,6,12,18,24),labels = c("00:00","06:00","12:00","18:00","00:00"))
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

#' Function to write variables in emission files
#'
#' @description Extract variable
#'
#' @param file Character; name of file interactively (default) or specified
#' @param name Character; name of the variable (any variable)
#' @param POL Numeric; emissions input or string/POSIXlt time
#' @param mult Numeric; multiplier. If the length is more than 1, it multiplies POL for each
#' value of mult. It can be used if you want to add an hourly profile to your emissions.
#' @param verbose display additional information
#'
#' @export
#'
#' @author Daniel Schuch and Sergio Ibarra
#'
#' @importFrom  ncdf4 nc_open nc_close ncvar_put
#'
#' @seealso \code{\link{wrf_plot}} and \code{\link{wrf_get}}
#'
#' @examples{
#' # create the folder and emission file
#' dir.create(file.path(tempdir(), "EMISS"))
#' wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = file.path(tempdir(), "EMISS"))
#'
#' # get the name of created file
#' files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                     pattern = "wrfchemi",
#'                     full.names = TRUE)
#'
#' # open, put some numbers and write
#' CO <- wrf_get(file = files[1],
#'               name = "E_CO")
#'
#' CO[] = rnorm(length(CO))
#'
#' wrf_put(file = files[1],
#'         name = "E_CO",
#'         POL = CO)
#' }
wrf_put <- function (file = file.choose(),
                     name = NA,
                     POL,
                     mult = NA,
                     verbose = FALSE) {
  if(class(POL[1]) =="POSIXlt" || class(POL[1]) == "POSIXt"){
    cat('converting POSIXlt to string\n')      # nocov
    POL <- format(POL,"%Y-%m-%d_%H:%M:%OS")    # nocov
    if(name == 'time')                         # nocov
      name <- 'Times'                          # nocov
  }
  if (verbose) {
    if (missing(mult)) {                                                           # nocov
      cat(paste0("writing ", name, " to   ", file, "\n"))                          # nocov
    }
    else {
      cat(paste0("writing ", name, " to   ", file, " multiplier ",mult, "\n"))     # nocov
    }
  }
  wrfchem <- ncdf4::nc_open(file, write = TRUE)
  if (missing(mult)) {
    ncdf4::ncvar_put(wrfchem,
                     varid = name,
                     POL)
  }
  else {
    ncdf4::ncvar_put(wrfchem,                                      # nocov
                     varid = name,                                 # nocov
                     unlist(lapply(seq_along(mult),                # nocov
                                   function(i) {POL*mult[i]})))    # nocov
  }
  ncdf4::nc_close(wrfchem)
}

#' Function to write variables in emission files
#'
#' @description Extract variable
#'
#' @param file Character; name of file interactively (default) or specified
#' @param name Character; name of the variable (any variable)
#' @param POL Numeric; emissions input or string/POSIXlt time
#' @param k Numeric; multiplier. If the length is more than 1, it multiplies POL for each
#' value of k. It can be used if you want to add an hourly profile to your emissions.
#' @param check logic (default is FALSE), TRUE to check for NA and negative values and replace with zeros
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
                     k,
                     check = FALSE,
                     verbose = FALSE) {
  if(check){              # nocov start
    has_NA  = FALSE
    has_neg = FALSE
    n_neg   = 0
    n_NA    = 0
    for(i in 1:length(POL)){
      if(is.na(POL[i])){
        POL[i] = 0
        n_NA   = n_NA + 1
        has_NA = TRUE
      }
      if(POL[i] < 0){
        POL[i]  = 0
        n_neg   = n_neg + 1
        has_neg = TRUE
      }
    }
    if(has_NA){
      warning(paste0(n_NA,' NA values found!\nreplaced by zeros'))
    }
    if(has_neg){
      warning(paste0(n_neg,' negative values found!\nreplaced by zeros'))
    }
  }            # nocov end
  if(class(POL[1])[1] =="POSIXlt" || class(POL[1])[1] == "POSIXt"){
    cat('converting POSIXlt to string\n')      # nocov
    POL <- format(POL,"%Y-%m-%d_%H:%M:%OS")    # nocov
    if(name == 'time')                         # nocov
      name <- 'Times'                          # nocov
  }
  if (verbose) {
    if (missing(k)) {                                                    # nocov
      cat(paste0("writing ", name, " to   ", file, "\n"))                # nocov
    }
    else {
      cat(paste0("writing ", name, " to   ", file, " k = ",k, "\n"))     # nocov
    }
  }
  wrfchem <- ncdf4::nc_open(file, write = TRUE)
  if (missing(k)) {
    ncdf4::ncvar_put(wrfchem,
                     varid = name,
                     POL)
  }
  else {
    ncdf4::ncvar_put(wrfchem,                                   # nocov
                     varid = name,                              # nocov
                     unlist(lapply(seq_along(k),                # nocov
                                   function(i) {POL*k[i]})))    # nocov
  }
  ncdf4::nc_close(wrfchem)
}

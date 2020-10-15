#' Summary of variables inside WRF files
#'
#' @description This return returns a summary for each variable.
#'
#'
#' @param file String path to the wrf.
#' @return data.frame
#'
#' @export
#' @examples \dontrun{
#' # do not run
#' file = paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
#' wrf_summary(file = file)
#' }
wrf_summary <- function(file) {
  file
  nc <- ncdf4::nc_open(file)
  vars <- names(nc$var)

i = 1
rm(i)
  df <- lapply(seq_along(vars), function(i){
    v <- ncdf4::ncvar_get(nc = nc, varid = vars[i])
    if(vars[i] == "Times") {
      v <- as.POSIXct(v, format = "%Y-%m-%d_%H:%M:%S")
      c(summary(v), sum = NA)
    } else {
      c(summary(as.vector(v)), sum = sum(v))

    }
  })
  df <- as.data.frame(t(do.call("rbind", df)))
  names(df) <- vars
  return(df)

}

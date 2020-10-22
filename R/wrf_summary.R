#' Summary of variables inside WRF files
#'
#' @description This return returns a summary for each variable.
#'
#'
#' @param file String path to the wrf.
#' @param vars String of WRF variables. If missing, all variables.
#' @importFrom data.table rbindlist
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return data.frame
#'
#' @export
#' @examples \dontrun{
#' # do not run
#' file = paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
#' wrf_summary(file = file)
#' }
wrf_summary <- function(file, vars) {
  nc <- ncdf4::nc_open(file)

  if(missing(vars))   vars <- names(nc$var)

  pb <- utils::txtProgressBar(min = 0, max = length(vars), style = 3)

  df <- lapply(seq_along(vars), function(i){
    utils::setTxtProgressBar(pb, i)
    v <- ncdf4::ncvar_get(nc = nc, varid = vars[i])
    if(vars[i] == "Times") {
      v <- as.POSIXct(v, format = "%Y-%m-%d_%H:%M:%S")
      dt <- c(summary(v), sum = NA)
    } else {
      dt <- c(summary(as.vector(v)), sum = sum(v))
    }
    as.data.frame(t(as.matrix(dt)))
  })
  gc()

  ncdf4::nc_close(nc)
  df <- as.data.frame(data.table::rbindlist(df))
  row.names(df) <- vars
  return(df)

}

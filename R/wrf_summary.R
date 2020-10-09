#' Summary of WRF files
#'
#' @description This return returns a summary for each variable and hour.
#' \code{\link{wrf_summary}} internally reads the wrf file using
#'  \code{\link{wrf_get}}, then applies raster::cellStats.
#'
#'
#' @param file String path to the wrf.
#' @param name String for variables in wrf file, internally read
#' by \code{\link{wrf_get}} and transformed to raster.
#' @param fn String of the function, for instance, "sum"
#' @return data.frame
#' @importFrom raster cellStats
#'
#' @export
#' @examples \dontrun{
#' # do not run
#' file <- "/home/sergio/R/x86_64-pc-linux-gnu-library/4.0/eixport/extdata/wrfinput_d02"
#' wrf_summary(file = file,
#'             name = c("XLAT", "XLONG"),
#'             fn = "mean")
#' }
wrf_summary <- function(file, name, fn = "sum") {
  ti <- wrf_get(file = file,
                name = "Times")
  dft <- data.frame(Times = ti)

  la <- lapply(seq_along(name), function(i) {
    a <- wrf_get(file = file,
                 name = name[i],
                 as_raster = TRUE)
    as.data.frame(raster::cellStats(a, fn))
  })

  df <- do.call("cbind", la)
  dt <- cbind(dft, df)
  names(dt) <- c("Times", name)
  return(dt)
}

#' Returns metadata (attributes) of wrf file in a data.frame
#'
#' @description \code{\link{wrf_meta}} returns the attributes of a wrf
#' NetCDF file in a data.frame. Therefore, there is no need to
#' use ncdump -h "wrf_file"
#' @param file Character; name of file interactively (default) or specified
#' @importFrom ncdf4 nc_open ncatt_get nc_close
#' @export
#' @examples {
#' file = paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
#' wrf_meta(file)
#' }
wrf_meta <- function(file = file.choose()){
  nc <- ncdf4::nc_open(file)
  na <- names(nc$var)
  na <- data.frame(vars = names(nc$var))
  na <- data.frame(vars = na[na$vars != "Times", ])

  latts <- as.data.frame(unlist(ncdf4::ncatt_get(nc, 0)))
  latts <- data.frame(att = row.names(latts),
                      vars = latts[[1]])

  ti <- ncdf4::ncatt_get(nc, "Times")

  la <- lapply(1:nrow(na), function(i) {
    unlist(ncdf4::ncatt_get(nc = nc, varid = na$vars[i]))
  })
  a <- as.data.frame(do.call("rbind", la))

  a <- cbind(na, a)

  ncdf4::nc_close(nc)

  a <- a[order(a$vars), ]
  df <- list(global = latts,
             vars = a)
  return(df)
}

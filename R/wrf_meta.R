#' Returns metadata (attributes) of wrf file in a data.frame
#'
#' @description \code{\link{wrf_meta}} returns the attributes of a wrf
#' NetCDF file in a data.frame. Therefore, there is no need to
#' use ncdump -h "wrf_file"
#' @param file Character; name of file interactively (default) or specified
#' @export
#' @examples {
#' # Do not run
#' }
wrf_meta <- function(file = file.choose()){
  nc <- ncdf4::nc_open(file)
  na <- names(nc$var)
  a <- as.data.frame(
    do.call("rbind",
            lapply(1:length(na), function(i) {
              dl <- lapply(1:6, function(j){
                atts <- ncatt_get( nc, nc$var[[i]]$name )

                x <- ncdf4::ncatt_get(nc = nc, varid = na[i])[j]
                ifelse(is.null(x), NA, x)
              })
              df <- as.data.frame(do.call("cbind", dl))

              df$vars <- na[i]
              df
            })))

  names(a) <- c("field_type", "memory_order", "description",
                "units", "stagger", "coordinates", "vars")

  ncdf4::nc_close(nc)
  df <- data.frame(
    vars = as.character(unlist(a$vars)),
    description = as.character(
      ifelse(sapply(a$description, is.null),NA, unlist(a$description))),
    memory_order = as.character(
      ifelse(sapply(a$memory_order, is.null),NA, unlist(a$memory_order))),
    field_type = ifelse(sapply(a$field_type, is.null),NA, unlist(a$field_type)),
    stagger = ifelse(sapply(a$stagger, is.null),NA, unlist(a$stagger)),
    coordinates = ifelse(sapply(a$coordinates, is.null),NA, unlist(a$coordinates)),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$vars), ]
  return(df)
}

#' Creates grid from wrf file
#'
#' @description Return a Spatial Feature multipolygon or matrix
#'
#' @param filewrf wrf file
#' @param type Type of wrf file: "wrfinput" or "geo". When type is "geo", lat
#' long comes from mass grid, XLONG_M and XLAT_M
#' @param matrix if the output is matrix or polygon (sf)
#' @param epsg epsg code number (see http://spatialreference.org/ref/epsg/)
#' @param as_raster  logical, to return a raster
#' @import ncdf4
#' @importFrom raster raster
#' @importFrom sf st_polygon st_multipolygon st_sf st_sfc st_cast
#' @export
#' @examples {
#' # Do not run
#' wrf <- paste(system.file("extdata", package = "eixport"),
#'                          "/wrfinput_d02", sep="")
#' gwrf  <- wrf_grid(wrf)
#' plot(gwrf, axes = TRUE)
#'}
wrf_grid <- function(filewrf, type = "wrfinput", matrix = F, epsg = 4326, as_raster = F){
  cat(paste("using grid info from:", filewrf, "\n"))
  wrf <- ncdf4::nc_open(filewrf)
  if(type == "wrfinput"){
    lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT")
    lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG")
  } else if(type == "geo"){                            # nocov
    lat    <- ncdf4::ncvar_get(wrf, varid = "XLAT_M")  # nocov
    lon    <- ncdf4::ncvar_get(wrf, varid = "XLONG_M") # nocov
  }
  time   <- ncdf4::ncvar_get(wrf, varid = "Times")
  dx     <- ncdf4::ncatt_get(wrf, varid = 0,
                             attname = "DX")$value
  n.lat  <- ncdf4::ncatt_get(wrf, varid = 0,
                             attname = "SOUTH-NORTH_PATCH_END_UNSTAG")$value
  n.lon  <- ncdf4::ncatt_get(wrf, varid = 0,
                             attname = "WEST-EAST_PATCH_END_UNSTAG")$value
  cat(paste0("Number of lat points ", n.lat, "\n"))
  cat(paste0("Number of lon points ", n.lon, "\n"))
  ncdf4::nc_close(wrf)
  r.lat  <- range(lon)
  r.lon  <- range(lat)
  EM  <- matrix(0, nrow = n.lon, ncol = n.lat)

  points      <- data.frame(lat  = c(lat),
                            long = c(lon))
  points$lat  <- as.numeric(points$lat)
  points$long <- as.numeric(points$long)

  dx <- 1.0 * (r.lat[1] - r.lat[2]) / (n.lat+1)
  dy <- 1.0 * (r.lon[2] - r.lon[1]) / (n.lon+1)
  alpha = 0 * (pi / 180)
  dxl <- cos(alpha) * dx - sin(alpha) * dy
  dyl <- sin(alpha) * dx + cos(alpha) * dy

  grid = list()

  for(i in 1:nrow(points)){
    # for(i in 1:2){
    p1_lat = points$lat[i]  - dx/2
    p1_lon = points$long[i] + dy/2

    p2_lat = points$lat[i]  + dx/2
    p2_lon = points$long[i] + dy/2

    p3_lat = points$lat[i]  + dx/2
    p3_lon = points$long[i] - dy/2

    p4_lat = points$lat[i]  - dx/2
    p4_lon = points$long[i] - dy/2

    mat  <- matrix(c(p1_lon,p1_lat,
                     p2_lon,p2_lat,
                     p3_lon,p3_lat,
                     p4_lon,p4_lat,
                     p1_lon,p1_lat),
                   ncol=2, byrow=TRUE)
    cell <- sf::st_polygon(list(mat))
    grid[[i]] = cell
  }
  geometry <- sf::st_sfc(sf::st_multipolygon(grid))
  grid <- sf::st_cast(x = st_sf(geometry = geometry, crs = epsg),
                      to = "POLYGON")
  grid$id <- 1:nrow(grid)

  if (matrix == T){
    return(EM)
  } else if (as_raster){
    r <- raster::raster(EM, xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = "+init=epsg:4326")
    return(r)
  } else {
    return(grid)
  }

}

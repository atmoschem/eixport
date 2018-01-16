#' Creates grid from wrf file
#'
#' @description Return a spatialfeature polygon or matrix
#'
#' @param filewrf wrfinput file (also geo.nc?, other?)
#' @param matrix if the output is matrix or polygon (sf)
#' @param epsg epsg code number (see http://spatialreference.org/ref/epsg/)
#'
#' @import ncdf4
#' @importFrom sf st_polygon st_multipolygon st_sf st_sfc
#' @export
#' @examples \dontrun{
#' # Do not run
#' wrf <- paste(system.file("extdata", package = "EmissV"),"/wrfinput_d02",sep="")
#' gwrf  <- wrf_grid(wrf)
#' plot(gwrf, axes = T)
#'}
wrf_grid <- function(filewrf, matrix = F, epsg = 4326){
    print(paste("using grid info from:",filewrf))

  wrf    <- ncdf4::nc_open(filewrf)
  lat    <- ncdf4::ncvar_get(wrf,varid = "XLAT")
  lon    <- ncdf4::ncvar_get(wrf,varid = "XLONG")
  time   <- ncdf4::ncvar_get(wrf,varid = "Times")
  dx     <- ncdf4::ncatt_get(wrf,varid = 0,
                             attname = "DX")$value #/ 1000 # km
  n.lat  <- ncdf4::ncatt_get(wrf,varid = 0,
                             attname = "SOUTH-NORTH_PATCH_END_UNSTAG")$value
  n.lon  <- ncdf4::ncatt_get(wrf,varid = 0,
                             attname = "WEST-EAST_PATCH_END_UNSTAG")$value
  ncdf4::nc_close(wrf)
  r.lat  <- range(lon)
  r.lon  <- range(lat)
  EM  <- matrix(0,nrow = n.lon, ncol = n.lat) # o formato da saida

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
  grid <- st_sf(geometry = geometry, crs = epsg)
  if (matrix == T){
    return(mat)
  } else {
    return(grid)
  }

}

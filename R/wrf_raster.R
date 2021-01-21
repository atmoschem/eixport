#' Creates raster from a variable from a wrf file
#'
#' @description Return a Raster
#'
#' @param file wrf file
#' @param name variable name
#' @param raster_crs default crs is "+proj=longlat"
#' @param as_polygons logical, true to return a poligon instead of a raster
#' @param verbose display additional information
#'
#' @import ncdf4
#' @import raster
#' @importFrom rgdal project
#'
#' @export
#' @examples {
#'
#' wrf <- paste(system.file("extdata", package = "eixport"),
#'                          "/wrfinput_d02", sep="")
#'
#' r <- wrf_raster(file=wrf, name='XLAT')
#'
#' library(raster)
#' plot(r, axes = TRUE)
#'}
wrf_raster <- function(file = file.choose(),
                       name = NA,
                       raster_crs = NA,
                       as_polygons = FALSE,
                       verbose = FALSE){

  if(!is.na(name)){
    if(name == 'time'){
      wrfchem <- ncdf4::nc_open(file)                                                     # nocov
      if(verbose)                                                                         # nocov
        cat(paste0('reading Times from ', file,'\n'))                                     # nocov
      TIME   <- ncvar_get(wrfchem,'Times')                                                # nocov
      TIME   <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE) # nocov
      if(verbose)                                                                         # nocov
        cat('returning Times in POSIXct\n')                                               # nocov
      return(TIME)                                                                        # nocov
    }
  }
  cat(paste0('reading ',name,' from ', file,'\n'))                  # nocov

  wrf <- ncdf4::nc_open(file)                                       # iteractive
  if(is.na(name)){                                                  # nocov start
    name  <- menu(names(wrf$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(wrf, names(wrf$var)[name])
    name  <- names(wrf$var)[name]                                   # nocov end
  }else{
    POL   <- ncvar_get(wrf,name)
  }
  cat(paste("crating raster for",name,'\n'))

  coordNC <- tryCatch(suppressWarnings(ncdf4::nc_open(file)),
                      error=function(cond) {message(cond); return(NA)})

  coordvarList = names(coordNC[['var']])
  if ("XLONG_M" %in% coordvarList & "XLAT_M" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG_M")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT_M")
  } else if ("XLONG" %in% coordvarList & "XLAT" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT")
  } else if ("lon" %in% coordvarList & "lat" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "lon")
    inNCLat <- ncdf4::ncvar_get(coordNC, "lat")
  } else {
    stop('Error: Latitude and longitude fields not found (tried: XLAT_M/XLONG_M, XLAT/XLONG, lat/lon')
  }

  nrows <- dim(inNCLon)[2]
  ncols <- dim(inNCLon)[1]

  # Reverse column order to get UL in UL
  x <- as.vector(inNCLon[,ncol(inNCLon):1])
  y <- as.vector(inNCLat[,ncol(inNCLat):1])

  coords <- as.matrix(cbind(x, y))

  # Get geogrid and projection info
  map_proj <- ncdf4::ncatt_get(coordNC, varid=0, attname="MAP_PROJ")$value
  cen_lat  <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LAT")$value
  cen_lon  <- ncdf4::ncatt_get(coordNC, varid=0, attname="STAND_LON")$value
  truelat1 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT1")$value
  truelat2 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT2")$value
  if (map_proj==1) {
    geogrd.proj <- paste0("+proj=lcc +lat_1=",
                          truelat1, " +lat_2=", truelat2, " +lat_0=",
                          cen_lat, " +lon_0=", cen_lon,
                          " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  } else {
    stop('Error: Projection type not supported (currently this tool only works for Lambert Conformal Conic projections).')
  }

  dx <- ncdf4::ncatt_get(coordNC, varid=0, attname="DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid=0, attname="DY")$value
  if ( dx != dy ) {
    stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))
  }

  projcoords <- rgdal::project(coords, geogrd.proj)
  # projcoords <- sf::st_transform(coords, geogrd.proj)

  # coordinates here refere to the cell center,
  # We need to calculate the boundaries for the raster file

  xmn <- projcoords[1,1] - dx/2.0   # Left border
  ymx <- projcoords[1,2] + dy/2.0   # upper border
  xmx <- xmn + ncols*dx             # Right border
  ymn <- ymx - nrows*dy             # Bottom border

  # Create a raster
  r <- suppressWarnings(
    raster::raster(resolution = dx,
                   xmn = xmn,
                   xmx = xmx,
                   ymn = ymn,
                   ymx = ymx,
                   crs = geogrd.proj))

  f2 <- function(a, wh){
    dims <- seq_len(length(dim(a)))
    dims <- setdiff(dims, wh)
    x    <- apply(apply(a, dims, rev), dims, t)
    return(x)
  }

  if(is.matrix(POL)){
    values(r) <- apply(t(POL), 2, rev)
  }else{
    r         <- raster::brick(r,nl = dim(POL)[3])
    values(r) <- f2(POL,2)
    names(r)  <- paste(name,ncvar_get(wrf,'Times'),sep="_")
  }

  ncdf4::nc_close(wrf)

  if(as_polygons){
    return(rasterToPolygons(r))
  }else{
    if(is.na(raster_crs)){
      return(r)
    }else{
      return(projectRaster(r, crs=raster_crs))
    }
  }
}

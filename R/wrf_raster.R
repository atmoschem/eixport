#' Creates raster from a variable from a wrf file
#'
#' @description Return a Raster
#'
#' @param file wrf file
#' @param name variable name
#' @param latlon project the output in "+proj=longlat +datum=WGS84 +no_defs"
#' @param level only for 4d data, default is 1 (surface)
#' @param as_polygons logical, true to return a poligon instead of a raster
#' @param map (optional) file with lat-lon variables and grid information
#' @param ... extra arguments passed to ncdf4::ncvar_get
#' @param verbose display additional information
#'
#' @import ncdf4
#' @import raster
#' @import sf
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
                       latlon = F,
                       level = 1,
                       as_polygons = FALSE,
                       map,
                       verbose = FALSE,
                       ...){

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
  if(verbose) cat(paste0('reading ',name,' from ', file,'\n'))      # nocov

  wrf <- ncdf4::nc_open(file)
  if(is.na(name)){                                                  # nocov start
    name  <- menu(names(wrf$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(wrf, names(wrf$var)[name], ... )
    name  <- names(wrf$var)[name]                                   # nocov end
  }else{
    POL   <- ncdf4::ncvar_get(wrf,name, ... )
  }
  if(verbose)  cat(paste("creating raster for",name,'\n'))          # nocov

  if(missing(map)){                                                 # nocov
    coord_file = file                                               # nocov
  }else{                                                            # nocov
    coord_file = map                                                # nocov
    cat('using coods and grid information from',map,'file\n')        # nocov
  }

  coordNC <- tryCatch(suppressWarnings(ncdf4::nc_open(coord_file)),
                      error=function(cond) {message(cond); return(NA)})  # nocov

  coordvarList = names(coordNC[['var']])
  if ("XLONG_M" %in% coordvarList & "XLAT_M" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG_M")                      # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT_M")                       # nocov
  } else if ("XLONG" %in% coordvarList & "XLAT" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT")
  } else if ("lon" %in% coordvarList & "lat" %in% coordvarList) {        # nocov
    inNCLon <- ncdf4::ncvar_get(coordNC, "lon")                          # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "lat")                          # nocov
  } else if ("longitude" %in% coordvarList & "latitude" %in% coordvarList) { # nocov
    inNCLon <- ncdf4::ncvar_get(coordNC, "longitude")                        # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "latitude")                         # nocov
  } else {
    stop('Error: Latitude and longitude fields not found (tried: XLAT_M/XLONG_M, XLAT/XLONG, lat/lon longitude/latitude') # nocov
  }

  nrows <- dim(inNCLon)[2]
  ncols <- dim(inNCLon)[1]

  # Reverse column order to get UL in UL
  if(length(dim(inNCLon)) == 3){ # for special case of lat/lon has more dimensions
    x <- as.vector(inNCLon[, ncol(inNCLon):1,]) # nocov
    y <- as.vector(inNCLat[, ncol(inNCLat):1,]) # nocov
  }else{
    x <- as.vector(inNCLon[,ncol(inNCLon):1])
    y <- as.vector(inNCLat[,ncol(inNCLat):1])
  }

  # Get geogrid and projection info
  map_proj <- ncdf4::ncatt_get(coordNC, varid=0, attname="MAP_PROJ")$value
  cen_lat  <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LAT")$value
  cen_lon  <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LON")$value
  truelat1 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT1")$value
  truelat2 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT2")$value
  ref_lon  <- ncdf4::ncatt_get(coordNC, varid=0, attname="STAND_LON")$value

  if(map_proj == 1){
    geogrd.proj <- paste0("+proj=lcc +lat_1=", truelat1,
                          " +lat_2=", truelat2,
                          " +lat_0=", cen_lat,
                          " +lon_0=", ref_lon,
                          " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  } else if(map_proj == 2){                              # nocov
    if(cen_lat > 0){                                     # nocov
      hemis =  90                                        # nocov
    }else{                                               # nocov
      hemis = -90                                        # nocov
    }
    geogrd.proj <- paste0("+proj=stere +lat_0=",hemis,   # nocov
                          " +lon_0=",ref_lon,            # nocov
                          " +lat_ts=",truelat1,          # nocov
                          " +x_0=0 +y_0=0",              # nocov
                          " +a=6370000 +b=6370000",      # nocov
                          " +units=m +no_defs")          # nocov
  } else if(map_proj == 3){                              # nocov
    geogrd.proj <-paste0("+proj=merc +lat_ts=",truelat1, # nocov
                         " +lon_0=",ref_lon,             # nocov
                         " +a=6370000 +b=6370000",       # nocov
                         " +datum=WGS84")                # nocov
  } else if(map_proj %in% c(0, 6)){                      # nocov
    geogrd.proj <- paste0("+proj=eqc +lat_ts=",0,        # nocov
                          " +lat_0=",cen_lat,            # nocov
                          " +lon_0=",ref_lon,            # nocov
                          " +x_0=",0," +y_0=",0,         # nocov
                          " +ellps=WGS84 +units=m")      # nocov
  } else {
    stop('Error: Projection type not supported (currently Lambert Conformal, Cylindrical Equidistant, Polar and lat-lon WRF grids are suported).') # nocov
  }

  dx <- ncdf4::ncatt_get(coordNC, varid=0, attname="DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid=0, attname="DY")$value
  if ( dx != dy ) {
    stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))  # nocov
  }

  pontos     <- sf::st_multipoint(x = as.matrix(cbind(x, y)), dim = "XY")           # nocov
  coords     <- sf::st_sfc(x = pontos, crs = "+proj=longlat +datum=WGS84 +no_defs") # nocov
  transform  <- sf::st_transform(x = coords, crs = geogrd.proj)                     # nocov
  projcoords <- sf::st_coordinates(transform)[,1:2]                                 # nocov

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
    dims <- seq_len(length(dim(a)))                # nocov
    dims <- setdiff(dims, wh)                      # nocov
    x    <- apply(apply(a, dims, rev), dims, t)    # nocov
    return(x)                                      # nocov
  }

  if(is.matrix(POL)){
    values(r) <- apply(t(POL), 2, rev)
    names(r)  <- paste(name)
  }else{
    r         <- raster::brick(r,nl = dim(POL)[3])          # nocov start
    if(length(dim(POL)) == 4){
      cat('raster::brick only support 3d data, using level',level,'\n')
      POL <- POL[,,level,,drop = T]
    }
    values(r) <- f2(POL,2)
    ndim      <- length(dim(POL))

    if('Times' %in% names(wrf$var)){              # this is new!
      ntimes    <- length(ncvar_get(wrf,'Times'))
    }else{
      cat('variable Times not found\n')
      ntimes    <- 1
    }

    if(ntimes == 1 & ndim > 2){
      if(nlayers(r) == dim(r)[3])
        names(r)  <- paste(name,'level',formatC(1:dim(r)[3],width = 2, format = "d", flag = "0"),sep="_")
    }else{
      if(nlayers(r) == length(ncvar_get(wrf,'Times')))
        names(r)  <- paste(name,ncvar_get(wrf,'Times'),sep="_")
    }
  }                                                         # nocov end

  ncdf4::nc_close(wrf)

  if(as_polygons){
    if(latlon){
      return(st_transform(x   = st_as_sf(rasterToPolygons(r)),           # nocov
                          crs = st_crs('+proj=longlat')))                # nocov
    }else{
      return(st_as_sf(rasterToPolygons(r)))                              # nocov
    }
  }else{
    if(latlon){
      return(projectRaster(r, crs="+proj=longlat +datum=WGS84 +no_defs"))  # nocov
    }else{
      return(r)                                 # nocov
    }
  }
}

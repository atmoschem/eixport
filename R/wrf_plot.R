#' Simple plot from wrf emission file
#'
#' @description Create a quick plot from wrf emission file
#'
#' @param file emission file name
#' @param name pollutant name
#' @param time time from emission file
#' @param nivel level from the emission file
#' @param barra barblot if TRUE
#' @param lbarra length of barplot
#' @param col color vector
#' @param skip logical, skip plot of constant valuess
#' @param extra function call to plot map lines, points and annotation
#' @param map (optional) file with lat-lon variables and grid information
#' @param verbose if TRUE print some information
#' @param ... Arguments to be passed to plot methods
#'
#' @note If the file contains levels (kemit>1), and one frame (auxinput5_interval_m = 1)
#' time with control the level which will be ploted
#'
#' @note In case of an error related to plot.new() margins lbarra must be adjusted
#'
#' @note This function provide a fast visualization, the projection is assumed to be lat-long, use wrf_raster and raster::plot to deal with different projections.
#'
#' @author Daniel Schuch
#'
#' @import ncdf4
#' @importFrom grDevices cm.colors gray.colors
#' @importFrom graphics .filled.contour Axis axis box layout mtext par plot.new plot.window rect title
#' @importFrom utils menu
#' @importFrom cptcity cpt
#' @export
#'
#' @seealso \code{\link{Lights}}, \code{\link{to_wrf}} and \code{\link{wrf_create}}
#'
#' @examples {
#'
#'dir.create(file.path(tempdir(), "EMISS"))
#'wrf_create(wrfinput_dir = system.file("extdata", package = "eixport"),
#'           wrfchemi_dir = file.path(tempdir(), "EMISS"))
#'
#'# get the name of created file
#'files <- list.files(path = file.path(tempdir(), "EMISS"),
#'                    pattern = "wrfchemi",
#'                    full.names = TRUE)
#'
#'# load end write some data in this emission file
#'data(Lights)
#'to_wrf(Lights, files[1], total = 1521983, name = "E_CO")
#'
#' wrf_plot(files[1], "E_CO")
#'}
wrf_plot <- function(file = file.choose(),
                     name = NA,
                     time = 1,
                     nivel = 1,
                     barra = T,
                     lbarra = 0.2,
                     col = cptcity::cpt(n = 20, rev = T),
                     extra = NULL,
                     map,
                     skip = FALSE,
                     verbose = TRUE,
                     ...){

  wrfchem <- ncdf4::nc_open(file)                                      # iteractive
  if(is.na(name)){                                                     # nocov start
    name  <- menu(names(wrfchem$var)[c(-1,-2,-3)], title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(wrfchem, names(wrfchem$var)[name+3])
    name  <- names(wrfchem$var)[name+3]                                # nocov end
  }else{
    POL   <- ncvar_get(wrfchem,name)
  }

  if(missing(map)){                                                 # nocov
    coord_file = file                                               # nocov
  }else{                                                            # nocov
    coord_file = map                                                # nocov
    cat('using lat and long information from',map,'file\n')       # nocov
  }

  coordNC <- tryCatch(suppressWarnings(ncdf4::nc_open(coord_file)),
                      error=function(cond) {message(cond); return(NA)})      # nocov

  coordvarList = names(coordNC[['var']])
  if ("XLONG_M" %in% coordvarList & "XLAT_M" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG_M")                          # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT_M")                           # nocov
  } else if ("XLONG" %in% coordvarList & "XLAT" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT")
  } else if ("lon" %in% coordvarList & "lat" %in% coordvarList) {            # nocov
    inNCLon <- ncdf4::ncvar_get(coordNC, "lon")                              # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "lat")                              # nocov
  } else if ("longitude" %in% coordvarList & "latitude" %in% coordvarList) { # nocov
    inNCLon <- ncdf4::ncvar_get(coordNC, "longitude")                        # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "latitude")                         # nocov
  } else {
    stop('Error: Latitude and longitude fields not found (tried: XLAT_M/XLONG_M, XLAT/XLONG, lat/lon longitude/latitude') # nocov
  }

  lat   <- range(inNCLat)
  lon   <- range(inNCLon)
  y     <- inNCLat [1, ]
  x     <- inNCLon[ ,1]

  ncdf4::nc_close(wrfchem)

  if(length(dim(POL)) == 3){
    POL <- POL[,,max(time,nivel,na.rm=TRUE)]        # nocov
  }
  if(length(dim(POL)) == 4){
    POL <- POL[,,nivel,time]                        # nocov
  }

  if(verbose){
    cat(wrfchem$filename,"\n",name,": ",sep = "")         # nocov
    if(max(POL) == min(POL)){                              # nocov
      cat(paste("Max value = Min Value =",max(POL),"\n"))  # nocov
    }
    else{
      cat(paste("Max value: ",max(POL),", Min value: ",min(POL),sep = "","\n")) # nocov
    }
  }

  if(skip & max(POL) == min(POL)){
    cat('skiping plot\n') # nocov
    return()              # nocov
  }

  filled.contour2 <-  function (x = seq(0, 1, length.out = nrow(z)),
                                y = seq(0, 1, length.out = ncol(z)), z,
                                xlim = range(x, finite = TRUE),
                                ylim = range(y, finite = TRUE),
                                zlim = range(z, finite = TRUE),
                                levels = pretty(zlim, nlevels),
                                nlevels = length(col),
                                color.palette = cm.colors,
                                col = col,
                                plot.title,
                                plot.axes,
                                key.title,
                                key.axes,
                                map = extra,
                                asp = NA,
                                xaxs = "i",
                                yaxs = "i",
                                las = 1,
                                axes = TRUE,
                                frame.plot = axes,
                                mar, ...) {

    if (missing(z)) {
      if (!missing(x)) {                             # nocov
        if (is.list(x)) {                            # nocov
          z <- x$z                                   # nocov
          y <- x$y                                   # nocov
          x <- x$x                                   # nocov
        }
        else {
          z <- x                                     # nocov
          x <- seq.int(0, 1, length.out = nrow(z))   # nocov
        }
      }
      else stop("no 'z' matrix specified")           # nocov
    }
    else if (is.list(x)) {
      y <- x$y                                       # nocov
      x <- x$x                                       # nocov
    }
    if (any(diff(x) <= 0) | any(diff(y) <= 0))
      stop("increasing 'x' and 'y' values expected") # nocov
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    par(las = las)
    mar <- mar.orig
    plot.new()
    par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) | nrow(z) <= 1 | ncol(z) <= 1)
      stop("no proper 'z' matrix specified")         # nocov
    if (!is.double(z))
      storage.mode(z) <- "double"                    # nocov
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
    .filled.contour(x, y, z, c(levels[length(levels)],999999999), col[length(col)])
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1, las = 1)
        Axis(y, side = 2, las = 3)
      }
    }
    else plot.axes                                   # nocov
    if (frame.plot)
      box()
    if (missing(plot.title))
      title(...)
    else plot.title                                  # nocov
    invisible()
  }


  barras <- function(x,
                     levels = pretty(x,nlevels),
                     nlevels = length(col),
                     col    = gray.colors(length(levels)-1),
                     titulo = "",...){
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    box()
    axis(4)
    title(titulo)
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(barra){
    par(mar = c(0, 0, 0, 0))
    layout(matrix(c(1,2),
                  ncol = 2,
                  nrow = 1,
                  byrow = T),
           widths = c(1,lbarra))
    par(mar=c(3.5, 3.5, 3, 0))
  }
  filled.contour2(x, y, POL, col = col)
  mtext("Lat", 2, line = 2.9,cex = 1.2, las=3)
  mtext("Long", 1, line = 2.2,cex = 1.2, las=1)
  if(!is.null(extra)){
    extra  # nocov
  }
  if(barra){
    par(mar = c(3.5, 1, 3, 4))
    barras(POL, col = col)
    mtext(name, 3, line = 0.8)
    par(mfrow = c(1, 1))
  }
}

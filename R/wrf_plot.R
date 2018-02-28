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
#' @param verbose if TRUE print some information
#' @param ... Arguments to be passed to plot methods
#'
#' @note If the file contains levels (kemit>1), and one frame (auxinput5_interval_m = 1)
#' time with control the level which will be ploted
#'
#' @note In case of a error related to plot.new() margins lbarra must be adjusted
#'
#' @author Daniel Schuch
#'
#' @import ncdf4
#' @importFrom grDevices cm.colors gray.colors
#' @importFrom graphics .filled.contour Axis axis box layout mtext par plot.new plot.window rect title
#' @importFrom utils menu
#' @export
#'
#' @seealso \code{\link{wrf_get}} and \code{\link{wrf_create}}
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
#'# open, put some numbers and write
#'wrf_plot(files[1], "E_CO")
#'}
wrf_plot <- function(file = file.choose(),
                     name = NA,
                     time = 1,
                     nivel = 1,
                     barra = T,
                     lbarra = 0.2,
                     verbose = T,
                     ...){
  wrfchem <- ncdf4::nc_open(file)
  if(is.na(name)){
    name  <- menu(names(wrfchem$var), title = "Chose the variable:")
    POL   <- ncdf4::ncvar_get(wrfchem, names(wrfchem$var)[name])
    name  <- names(wrfchem$var)[name]
  }else{
    POL   <- ncvar_get(wrfchem,name)
  }

  xlat      <- ncdf4::ncvar_get(wrfchem, varid="XLAT")
  xlong     <- ncdf4::ncvar_get(wrfchem, varid="XLONG")
  lat       <- range(xlat)
  lon       <- range(xlong)
  y         <- xlat [1, ]
  x         <- xlong[ ,1]

  Times     <- ncdf4::ncvar_get(wrfchem, varid="Times")

  ncdf4::nc_close(wrfchem)

  if(length(dim(POL)) == 3){
    POL <- POL[,,time]
  }
  if(length(dim(POL)) == 4){
    POL <- POL[,,nivel,time]
  }

  if(verbose){
    print(wrfchem$filename)
    print(name)
    if(max(POL) == min(POL)){
      warning("Max value = Min Value!")
    }
    else{
      print(paste("Max value: ",max(POL),", Min value: ",min(POL),sep = ""))
    }
  }

  filled.contour2 <-  function (x = seq(0, 1, length.out = nrow(z)),
                                y = seq(0, 1, length.out = ncol(z)), z,
                                xlim = range(x, finite = TRUE),
                                ylim = range(y, finite = TRUE),
                                zlim = range(z, finite = TRUE),
                                levels = pretty(zlim, nlevels),
                                nlevels = 20,
                                color.palette = cm.colors,
                                col = gray.colors(length(levels)-1),
                                plot.title,
                                plot.axes,
                                key.title,
                                key.axes,
                                asp = NA,
                                xaxs = "i",
                                yaxs = "i",
                                las = 1,
                                axes = TRUE,
                                frame.plot = axes,
                                mar, ...) {
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
      stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    par(las = las)
    mar <- mar.orig
    plot.new()
    par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
      stop("no proper 'z' matrix specified")
    if (!is.double(z))
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
    .filled.contour(x, y, z, c(levels[length(levels)],999999), col[length(col)])
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot)
      box()
    if (missing(plot.title))
      title(...)
    else plot.title
    invisible()
  }


  barras <- function(x,
                     levels = pretty(x),
                     n = length(levels),
                     col = gray.colors(length(levels)-1),
                     titulo = "",...){
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    box()
    axis(4)
    title(titulo)
  }


  if(barra){
    old.par <- par(mar = c(0, 0, 0, 0))
    layout(matrix(c(1,2),
                  ncol = 2,
                  nrow = 1,
                  byrow = T),
           widths = c(1,lbarra))
    par(mar=c(3.5, 3.5, 3, 0))
  }
  filled.contour2(x, y, POL)
  mtext(paste("WRF-Chem emissions - Time:", Times[time]), 3, line = 0.8)
  mtext("Latitude", 2, line = 2.2,cex = 1.2, las=0)
  mtext("Longitude", 1, line = 2.2,cex = 1.2)
  if(barra){
    par(mar = c(3.5, 1, 3, 4))
    barras(POL)
    mtext(name, 3, line = 0.8)
    par(old.par)
    par(mfrow = c(1, 1))
  }
}

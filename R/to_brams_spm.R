#' Inputs for BRAMS-SPM
#'
#' @description Create inputs for BRAMS-SPM. The inputs consist of a data-frame
#' or a list of data-frames with daily emissions (mol/day), lat, long. Also,
#' including a functions describing the hourly profile.
#'
#' @param sdf Grid emissions, which can be a SpatialPolygonsDataFrame or polygon
#'  grid class sf' including the hourly emissions in mol/h for 24 hours. The
#'  object can also be a list of objects SpatialPolygonsDataFrame or Spatial
#'  Features polygon grid class 'sf'.
#' @param epsg Coordinate reference system, e.g: "4326". Used to transform the
#' coordinates of the output.
#' @return data-frame of daily gridded emissions, lat, long and a message with
#' function.
#' @note When the input is class 'Spatial', they are converted to 'sf'. If the
#' input is a data-frame, the output is a data-frame. If the input is a list,
#' the output is a list.
#'
#' @references SPM BRAMS: FREITAS, E. MARTINS, L.,  SILVA, P. and ANDRADE, M.
#' A simple photochemical module implemented in rams for tropospheric ozone
#' concentration forecast in the metropolitan area of são paulo, brazil:
#' Coupling and validation. Atmospheric Environment, Elsevier, n. 39, p.
#' 6352–6361, 2005.
#' @author Sergio Ibarra and Edmilson Freitas
#' @importFrom sf st_as_sf st_coordinates st_set_geometry st_transform
#' @export
#'
#' @examples \dontrun{
#' data(gCO)
#' df1 <- to_brams_spm(sdf = gCO,
#'                     epsg = 4326)
#' head(df1)
#' df2 <- to_brams_spm(sdf = list(co = gCO, pm = gCO),
#'                     epsg = 4326)
#' lapply(df2, head)
#'}
to_brams_spm <- function(sdf, epsg = 4326){
  if(inherits(x = sdf, what = "list")){
    if(class(sdf[[1]])[1] == "SpatialPolygonsDataFrame"){
      # message("SpatialPolygonsDataFrame")
      sdf <- lapply(sdf, sf::st_as_sf)     # nocov
    } else if(class(sdf[[1]])[1] == "sf"){

    }
    dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf[[1]], epsg)))
    dft$N <- paste0(dft[, 3], "_", dft[, 4])
    dft <- dft[!duplicated(dft$N),]
    dft <- dft[, 1:2]
    names(dft) <- c("long", "lat")
    ldf <- lapply(1:length(sdf),
                  function(i){
                    cbind(rowSums(sf::st_set_geometry(sdf[[i]], NULL)),
                          dft)
                  })
    nombres <- names(sdf)
    for(i in 1:length(sdf)){
      names(ldf[[i]]) <- c(nombres[i], "long", "lat")
    }
    # Initially, this function return rowsums and polygon separatly
    # sumdf <- sapply(sdf, rowSums, na.rm = TRUE)
    # names(sumdf) <- paste0("sum_" , names(sdf))
    return(ldf)
  } else {
    # message("sf")
    sdf <- sf::st_as_sf(sdf)
    dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf, epsg)))
    dft$N <- paste0(dft[, 3], "_", dft[, 4])
    dft <- dft[!duplicated(dft$N),]
    dft <- dft[, 1:2]
    names(dft) <- c("long", "lat")
    dft2 <- data.frame(e24h = rowSums(sf::st_set_geometry(sdf, NULL)))
    ldf <- cbind(dft, dft2)
    return(ldf)
  }
}

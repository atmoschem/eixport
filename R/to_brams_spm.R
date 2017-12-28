#' Inputs for BRAMS SPM
#'
#' @description Create inputs for SPM brams. The inputs consists in data-frame
#' or a lists of data-frames with daily emissions (mol/day), lat, long. Also,
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
#' @note When the input os class 'Spatial', they are converted to 'sf'. If the
#' input is a data-frame, the output is a data-frame. If he input is a list,
#' the output is a list.
#'
#' @references SPM BRAMS: FREITAS, E. MARTINS, L.,  SILVA, P. and ANDRADE, M.
#' A simple photochemical module implemented in rams for tropospheric ozone
#' concentration forecast in the metropolitan area of são paulo, brazil:
#' Coupling and validation. Atmospheric Environment, Elsevier, n. 39, p.
#' 6352–6361, 2005.
#' @author Sergio Ibarra
#' @importFrom sf st_as_sf st_coordinates st_set_geometry st_transform
#' @export
#'
#' @examples \dontrun{
#' # Do not run
#'
#'}
to_brams_spm <- function(sdf, epsg = 4326){
  if(inherits(x = sdf, what = "list")){
    if(class(sdf[[1]]) == "SpatialPolygonsDataFrame"){
      message("SpatialPolygonsDataFrame")
      sdf <- lapply(sdf, sf::st_as_sf)
    } else if(class(sdf[[1]] == "sf")){
        message("sf")
      }
      dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf[[1]], epsg)))
      ldf <- lapply(1:length(sdf),
                    function(i){
                      cbind(colSums(sf::st_set_geometry(sdf[[i]], NULL)),
                            dft)
                    })
      return(ldf)
      # Initially, this function return rowsums and polygon separatly
      sumdf <- sapply(sdf, rowSums, na.rm = T)
      names(sumdf) <- paste0("sum_" , names(sdf))
      print(sumdf)
  } else if(class(sdf) == "sf"){
    message("sf")
    dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf, epsg)))
    ldf <- as.data.frame(cbind(colSums(sf::st_geometry(sdf, NULL)), dft))
    return(ldf)
  } else if(class(sdf) == "SpatialPolygonsDataFrame") {
    message("SpatialPolygonsDataFrame")
    sdf <- sf::st_as_sf(sdf)
    dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf, epsg)))
    ldf <- as.data.frame(cbind(colSums(sf::st_set_geometry(sdf, NULL)), dft))
    sumdf <- rowSums(sdf, na.rm = T)
    names(sumdf) <- paste0("sum_" , names(sdf))
    print(sumdf)
    return(ldf)
  }
}

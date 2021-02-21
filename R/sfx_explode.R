#' splits line by vertex
#'
#' @description \code{\link{sfx_explode}} splits line by vertex
#'
#' @param x sf LINESTRING.
#' @return spatial lines
#' @importFrom sf st_sf st_as_sfc st_geometry st_crs st_cast st_set_geometry
#' @importFrom data.table as.data.table ".SD" ".N"
#' @export
#' @examples {
#' data(emisco)
#' dim(emisco)
#' dfco <- sfx_explode(emisco)
#' dim(dfco)
#' }
sfx_explode <- function(x) {
  x <- sf::st_cast(x, "LINESTRING")
  crs <- sf::st_crs(x)
  x$id <- 1:nrow(x)
  nosf <- sf::st_set_geometry(x, NULL)

    co <- data.table::as.data.table(sf::st_coordinates(x))

    names(co) <- c("xini", "yini", "id")
    co$xend <- co$xini[c(2:length(co$xini), NA)]
    co$yend <- co$yini[c(2:length(co$yini), NA)]

    id <- NULL
    #selecionar de primero a penultimo por id
    df <- co[,
             .SD[1:(.N-1)],
             by=id]

    df <- as.data.frame(df)

    df <- merge(nosf, df, by = "id", all.y = TRUE)

    sdf <- sf::st_sf(df,
                     geometry = sf::st_as_sfc(
                       paste0(
                         "LINESTRING (",
                         df$xini,
                         " ",
                         df$yini,
                         ", ",
                         df$xend,
                         " ",
                         df$yend,
                         ")"
                       )))
    sdf$xini <- sdf$yini <- sdf$xend <- sdf$yend <- NULL

  sf::st_crs(sdf) <- crs

  return(sdf)
}

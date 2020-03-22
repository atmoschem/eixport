#' splits line by vertex
#'
#' @description \code{\link{sfx_explode}} splits line by vertex
#'
#' @param x spatial lines.
#' @param ... ignored
#' @return spatial lines
#' @author Michael Summer github[dot]/hypertidy/silicate/issues/102
#' @export
#' @importFrom sf st_as_sf st_set_crs
#' @importFrom  silicate SC0
#' @importFrom  tidyr unnest
#' @importFrom sfheaders sf_linestring
#' @examples {
#' # Do not run
#' data(emisco)
#' emisco <- emisco[1:100, "V8"]
#' dfco <- sfx_explode(emisco)
#' etm <- to_munich(sdf = dfco)
#' names(etm)
#' class(etm)
#' head(etm$Emissions)
#' head(etm$Street)
#' write.table(x = etm$Emissions, file = paste0(tempfile(), "_Emissions.txt"),
#' row.names = FALSE, sep = " ", quote = FALSE)
#' write.table(x = etm$Street, file = paste0(tempfile(), "_Street.txt"),
#' row.names = FALSE, sep = " ", quote = FALSE)
#' }
sfx_explode <- function(x, ...) {
  ##############################################################################
  # From mdsummer
  # copied github[dot]/hypertidy/silicate/issues/102
  # Once available at github, it will be imported from hypertidy/silicate
  x <- sf::st_as_sf(x)
  ## decompose to segments (edges), will work on lines or polygons doesn't matter
  sc <- silicate::SC0(x)
  ## unnest the nested segment indexes
  ## can remove unnest with a bit of do.call and lapply nrow set-up
  segs <- tidyr::unnest(sc$object, cols = c("topology_"))[c(".vx0", ".vx1")]

  ## index vertices x_ y_ by segment matrix
  coords <- sc$vertex[as.vector(t(as.matrix(segs[c(".vx0", ".vx1")]))), c("x_", "y_")]
  ## column name to avoid clash with existing data
  idname <- ".crazy_id_name_873987387387_"
  ## create line id for each coordinate pair
  coords[[idname]] <- rep(seq_len(dim(segs)[1L]), each = 2L)
  ## build segment linestrings with sfheaders
  sflines <- sfheaders::sf_linestring(coords,
                                      x = "x_", y = "y_", linestring_id = idname)

  ## drop vertex columns from silicate output now that we have used them
  segs[[".vx0"]] <- NULL
  segs[[".vx1"]] <- NULL
  ## if any object attributes remaining stick them onto the sf
  if (dim(segs)[2] > 0) {
    ## add columns back in
    sflines[names(segs)] <- segs
  }
  sflines[[idname]] <- NULL
  ## restore projection metadata
  sf::st_set_crs(sflines, sf::st_crs(x))
}

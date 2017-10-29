#' Emissions database from vein package (g/ms)
#'
#' Emissions data of CO from vein demo. When runingn the demo in the vein
#' package, it produces an object called 'net' which includes the emissions
#' for 168 hours of CO in g/h. This object was exported to shapefiles and open
#' in QGIS to split the line vector by its vertices with the interface with
#' GRASS inside QGIS (v.split.vert) and max vert per link = 2.
#' Then the emissions were divided by the length of the link in meters and by
#' 3600 to obtain emissions in g/(ms).
#'
#' It includes parameters coordinates of initial and final link calculates in
#' QGIS
#'
#' @usage data(emisco)
#' @docType data
"emisco"

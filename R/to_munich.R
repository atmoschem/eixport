#' Export emissions to Model of Urban Network of Intersecting Canyons and Highways (MUNICH)
#'
#' @description \code{\link{to_munich}} Export spatial emissions objects
#' according the format required by MUNICH. This function was designed to
#' read street emissions from VEIN by it can be used to read any other.
#'
#' @param sdf Street Emissions object class 'sf' LINESTRING or "SpatialLinesdataFrame".
#' The columns are the emissions.
#' @param idbrin Integer; id.
#' @param typo Integer; id2.
#' @param width Integer; width.
#' @param height Integer; heigth.
#' @param crs Numeric; Coordenade Reference System with default value of 4326.
#' @return A list with a data frame with columns "i", "idbrin", "typo", "xa",
#' "ya", "xb", "yb" and the pollutants; and another data.frame with "i",
#' "length" (m), "width" (with value 0) and "height" (with value 0). Width and
#' height must be obtained by the user.
#' @references  Kim, Y., Wu, Y., Seigneur, C., and Roustan, Y.:
#' Multi-scale modeling of urban air pollution: development and application of
#' a Street-in-Grid model (v1.0) by coupling MUNICH (v1.0) and Polair3D
#' (v1.8.1), Geosci. Model Dev., 11, 611-629,
#' https://doi.org/10.5194/gmd-11-611-2018, 2018.
#' @note The user must ensure that the spatial object has one line feature
#' per vertex and lines with more than one vertex must be previously splitted.
#' the resulting units must be \strong{ug/km/h}
#' @export
#' @examples {
#' library(vein)
#' library(units)
#' library(sf)
#' data(net)
#' data(pc_profile)
#' data(profiles)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#'
#' # Estimation for morning rush hour and local emission factors and speed
#' speed <- data.frame(S8 = net$ps)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed)
#' # rowSums drop units
#' net$CO  <- set_units(rowSums(E_CO), g/h)
#' # selecting only CO and exploding lines and updating emissions
#' df <- st_explode(net["CO"])
#' # st_explode should not drop units, must fix
#' df$CO  <- set_units(df$CO, g/h)
#' # now we have split line in vertex
#' # selecting 1000 links
#' dfco <- df[1:1000,"CO"]
#' ###########
#' #MUNICH relies in a python script that reads emissions with units ug/km/h
#' # Therefore
#' dfco$CO <- set_units(dfco$CO, ug/h)
#' dfco$CO<- dfco$CO/set_units(st_length(dfco), km)
#' etm <- to_munich(sdf = dfco)
#' names(etm)
#' class(etm)
#' head(etm$Emissions)
#' head(etm$Street)
#' write.table(x = etm$Emissions, file = paste0(tempfile(), "_Emissions.txt"),
#' row.names = FALSE, sep = " ", quote = FALSE)
#' write.table(x = etm$Street, file = paste0(tempfile(), "_Street.txt"),
#' row.names = FALSE, sep = " ", quote = FALSE)
#' ######
#' # todo: handle all unit checks and conversion internally
#' }
to_munich <- function (sdf, idbrin, typo, width, height, crs= 4326){
  sdf <- sf::st_as_sf(sdf)
  x <- sapply(sf::st_set_geometry(sdf, NULL), class)
  if(any(x != "units")){
   stop("All emissions must have units. Check ?units::set_units") # nocov
  }
  sdf$id <- NULL
  # if(length(unique(sapply(sf::st_geometry(sdf), length))) > 1){
  #   sdf <- sfx_explode(sdf)
  # }

  dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf, crs)))
  lista <- split(x = dft, f = dft$L1)
  df <- do.call("rbind",(lapply(1:length(lista), function(i){
    cbind(names(lista)[i], lista[[i]][1,], lista[[i]][2,])
  })))
  names(df) <- c("i", "xa", "ya", "borrar1", "xb", "yb", "borrar2")
  if(missing(idbrin)) idbrin <- df$i
  if(missing(typo)) typo <- rep(0, nrow(df))
  if(missing(width)) width <- rep(0, nrow(df))
  if(missing(height)) height <- rep(30, nrow(df))
  dfa <- data.frame(i = df$i,
                    idbrin = idbrin,
                    typo = typo,
                    xa = df$xa,
                    ya = df$ya,
                    xb = df$xb,
                    yb = df$yb)
  dfb <- sf::st_set_geometry(sdf, NULL)
  dfr <- cbind(dfa, dfb)
  dfr2 <- data.frame(i = dfa$i,
                     length = sf::st_length(sdf),
                     width,
                     height)
  dfl <- list(Emissions = dfr,
              Street = dfr2)
  return(dfl)
}

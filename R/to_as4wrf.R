#' Generates emissions dataframe to generate WRF-Chem inputs
#'
#' \code{to_as4wrf} returns a dataframes with columns lat, long, id, pollutants,
#' local time and GMT time. This dataframe has the proper format to be used with WRF
#' assimilation system: "Another Asimilation System 4 WRF (AAS4WRF)" as published
#' by Vera-Vala et al (2016)
#'
#' @param sdf Gridded emissions, which can be a SpatialPolygonsDataFrame, or a list
#' of SpatialPolygonsDataFrame, or a sf object of "POLYGON". The user must enter
#' a list with 36 SpatialPolygonsDataFrame with emissions for the mechanism CBMZ.
#' When there  are no emissions available, the SpatialPolygonsDataFrame must contain
#' 0.
#' @param nr Number of repetitions of the emissions period
#' @param dmyhm String indicating Day Month Year Hour and Minute in the format
#' "d-m-Y H:M" e.g.: "01-05-2014 00:00" It represents the time of the first
#' hour of emissions in Local Time
#' @param tz Time zone as required in for function \code{\link{as.POSIXct}}
#' @param crs Coordinate reference system, e.g: "+init=crs:4326". Used to
#' transform the coordinates of the output
#' @param islist logical value to indicate if sdf is a list or not
#' @importFrom sf st_coordinates  st_transform st_set_geometry
#' @importFrom methods as
#' @return data-frame of gridded emissions  g/h
#' @export
#' @note The user must produce a text file with the data-frame resulting of this
#' function. Then, use this file with the NCL script AAS4WRF.ncl
#' @note The reference of the emissions assimilation system is Vara-Vela, A.,
#' Andrade, M. F., Kumar, P., Ynoue, R. Y., and Munoz, A. G.: Impact of
#' vehicular emissions on the formation of fine particles in the Sao Paulo
#' Metropolitan Area: a numerical study with the WRF-Chem model, Atmos. Chem.
#' Phys., 16, 777-797, doi:10.5194/acp-16-777-2016, 2016.
#' A good website with timezones is http://www.timezoneconverter.com/cgi-bin/tzc
#' The crs is the same as used by \code{\link{sf}} package
#' It returns a dataframe with id,, long, lat, pollutants, time_lt, time_utc
#' and day-UTC-hour (dutch)
#' The pollutants for the CBMZ are: e_so2, e_no, e_ald, e_hcho, e_ora2, e_nh3
#' e_hc3, e_hc5, e_hc8, e_eth, e_co, e_ol2, e_olt, e_oli, e_tol, e_xyl, e_ket
#' e_csl, e_iso, e_no2, e_ch3oh, e_c2h5oh, e_pm25i, e_pm25j, e_so4i, e_so4j
#' e_no3i, e_no3j, e_orgi, e_orgj, e_eci, e_ecj, e_so4c, e_no3c, e_orgc, e_ecc
#' @seealso \code{\link{wrf_create}} \code{\link{to_wrf}}
#' @examples {
#' data(gCO)
#' df <- to_as4wrf(sdf = gCO,
#'                 dmyhm = "29-04-2018 00:00",
#'                 tz = "America/Sao_Paulo")
#' head(df)
#' df2 <- to_as4wrf(sdf = list(co = gCO, pm = gCO),
#'                  dmyhm = "29-04-2018 00:00",
#'                  tz = "America/Sao_Paulo")
#' head(df2)
#' }
to_as4wrf <- function(sdf,
                      nr = 1,
                      dmyhm,
                      tz,
                      crs = 4326,
                      islist){
  if(nr <= 0){
    stop("The argument 'nr' must be positive") # nocov
  } else if (class(sdf)[1] != "list") {
    sdf <- sf::st_as_sf(sdf)
    # if(class(sdf)[1] == "sf"){
    #   sdf <- as(sdf, "Spatial")
    # }
    dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf, crs)))
    dft$N <- paste0(dft[, 3], "_", dft[, 4])
    dft = dft[!duplicated(dft$N),]
    dft <- dft[, 1:2]

    dftid <- data.frame(id = 1:nrow(dft))

    dft <- data.frame(cbind(dftid, dft))

    sdf <- sf::st_set_geometry(sdf, NULL)

    dft <- do.call("rbind", replicate(ncol(sdf), dft, simplify = FALSE))
    dft$pol <-  unlist(lapply(1:ncol(sdf), function(i) {
      as.numeric(sdf[, i])
    })
    )
    names(dft) <- c("id", "long", "lat", "pollutant")
    dft <- do.call("rbind", replicate(nr, dft, simplify = FALSE))
    # tzz <- ifelse(utc!=0,(-utc)*3600, 0)
    time_lt <- as.POSIXct(x = dmyhm, format="%d-%m-%Y %H:%M", tz=tz)
    dft$time_lt = rep(seq.POSIXt(from = time_lt,
                                 by = "1 hour",
                                 length.out = ncol(sdf)*nr),
                      each=nrow(sdf))
    dft$time_utc <- dft$time_lt
    attr(dft$time_utc, "tzone") <- "Etc/UTC"
    dft$dutch <- as.numeric(
      paste0(
        strftime(dft$time_utc, timezone = tz, format = "%d"),
        strftime(dft$time_utc, timezone = tz, format = "%H")
      ))
  } else if (class(sdf)[1] == "list") {
    # if(class(sdf)[1] == "sf"){
    #   sdf <- lapply(sdf, methods::as, "Spatial")
    # }
    dft <- as.data.frame(sf::st_coordinates(sf::st_transform(sdf[[1]], crs)))
    dft$N <- paste0(dft[, 3], "_", dft[, 4])
    dft = dft[!duplicated(dft$N),]
    dft <- dft[, 1:2]

    dftid <- data.frame(id = 1:nrow(sdf[[1]]))

    dft <- as.data.frame(cbind(dftid, dft))

    sdf <- lapply(sdf, sf::st_set_geometry, NULL)

    # remove id and convert to matrix
    dft2 <- sapply(1:length(sdf), function(i){
      sdf[[i]]$id <- NULL
      unlist(sdf[[i]])
    })
    # convert to data.frame
    dft2 <- data.frame(dft2, row.names = NULL)
    names(dft2) <- names(sdf)
    # Create data.frame with id = 0
    dft1 <- data.frame(id = rep(0, nrow(dft2)), row.names = NULL)
    dft1$id <- dft$id
    dft1$long <- dft$X
    dft1$lat <- dft$Y
    # cbind both data.frames
    dft <- cbind(dft1, dft2)
    # replicate data.frames
    dft <- do.call("rbind", replicate(nr, dft, simplify = FALSE))
    time_lt <- as.POSIXct(x = dmyhm, format="%d-%m-%Y %H:%M", tz=tz)
    sdf1 <- sdf[[1]]
    sdf1$id <- NULL
    dft$time_lt <- rep(seq.POSIXt(from = time_lt,
                                  by = "1 hour",
                                  length.out = ncol(sdf1)*nr),
                       each=nrow(sdf[[1]]))
    dft$time_utc <- dft$time_lt
    attr(dft$time_utc, "tzone") <- "Etc/UTC"
    dft$dutch <- as.numeric(
      paste0(
        strftime(dft$time_utc, timezone = tz, format = "%d"),
        strftime(dft$time_utc, timezone = tz, format = "%H")
      ))
  }
  return(dft)
}

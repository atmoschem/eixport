#' Agregate EDGAR emissions NetCDF files into a RasterStack by
#'
#' @description The Emissions Database for Global Atmospheric Research (EDGAR) is a
#' project from the Joint Research Centre. This function reads the NetCDF and
#' merge/aggregate into diferent chemical mechanisms
#'
#'
#' @param path Character; path to the NetCDF files from EDGAR. The directory
#' **must** have one file for each of the following pollutants:
#' "voc" from 1 to 25, "co", "nox", "nmvoc","so2", "nh3",
#' "pm10", "pm2.5", "bc" and "oc"
#' @param chem Character; chemical mechanism: "edgar", "radm", "radmsorg",
#' "cbmz_mosaic", "cptec" and "ecb05_opt1".
#'
#' - When chem is "edgar" units are: "g km-2 h-1"
#' - Other mechanisms: gases "mol km-2 h-1" and aerosols: "ug m-2 s-1"
#' @param merge Logical; in the case that tehre are more than one NetCDF per pollutant,
#' merge = TRUE will merge them with sum. Default is FALSE.
#' @param k, Numeric; Value to factorize each pollutant.
#' @return RasterStack
#' @importFrom raster stack raster rotate
#' @importFrom ncdf4 nc_open nc_close
#' @note Molecular weights were obtained from Development of
#' Improved Chemical Speciation Database for Processing Emissions of
#' Volatile Organic Compounds for Air Quality Models
#' https://intra.engr.ucr.edu/~carter/emitdb/
#'
#' Some mappings were obtained from:
#'
#' Lopez-Norena, Ana and Fernandez, Rafael & Puliafito, SALVADOR. (2019).
#' ESPECIACION DE INVENTARIOS DE EMISIONES DE AEROSOLES Y COMPUESTOS ORGANICOS
#' VOLATILES PARA EL MODELO WRF-CHEM, APLICADO A LOS ESQUEMAS RADM-2,
#' CBM-Z Y MOZART-4.
#'
#' @export
#' @examples \dontrun{
#' # do not run
#' # Get EDGAR ####
#' get_edgar(dataset = "v432_VOC_spec",
#'           destpath = "V50_432_AP/TOT/",
#'           sector = c( "TOTALS"),
#'           type = "nc",
#'           year = 2012)
#'
#' get_edgar(dataset = "v50_AP",
#'           destpath = "V50_432_AP/TOT",
#'           sector = c( "TOTALS"),
#'           type = "nc",
#'           year = 2014)
#'
#' get_edgar(dataset = "v432_VOC_spec",
#'           destpath = "V50_432_AP/TRO/",
#'           sector = c( "TRO"),
#'           type = "nc",
#'           year = 2012, ask = F)
#'
#' get_edgar(dataset = "v50_AP",
#'           destpath = "V50_432_AP/TRO",
#'           sector = c( "TRO_RES", "TRO_noRES"),
#'           type = "nc",
#'           year = 2014)
#'
#' totals <- list.files(path = "V50_432_AP/TOT/",
#'                      full.names = TRUE,
#'                      pattern = ".zip")
#' lapply(totals, unzip, exdir = "V50_432_AP/TOT//")
#'
#'
#' tros <- list.files(path = "V50_432_AP/TRO",
#'                      full.names = TRUE,
#'                      pattern = ".zip")
#' lapply(tros, unzip, exdir = "V50_432_AP/TRO/")
#' edgar_chem("V50_432_AP/TOT", "radm")
#' }
chem_edgar <- function(path,
                       chem,
                       merge = FALSE,
                       k = rep(1, 34)){

  dte <- sysdata$dte
  emis_opt <- sysdata$emis_opt

  if (length(k) < 34) stop("k must have 34 elements")

  #no covr, becuase it would take too much time
  ncs <- list.files(path = path, #nocov start
                    full.names = TRUE,
                    pattern = ".nc")
  # just checking units again
  a <- ncdf4::nc_open(ncs[1])
  cat("units: ", a$var[[1]]$units, "\n")
  ncdf4::nc_close(a)

  # unidades  kg m-2 s-1

  lncs <- list(
    voc1 = grep("voc1_", ncs, value = T),
    voc2 = grep("voc2_", ncs, value = T),
    voc3 = grep("voc3_", ncs, value = T),
    voc4 = grep("voc4_", ncs, value = T),
    voc5 = grep("voc5_", ncs, value = T),
    voc6 = grep("voc6_", ncs, value = T),
    voc7 = grep("voc7_", ncs, value = T),
    voc8 = grep("voc8_", ncs, value = T),
    voc9 = grep("voc9_", ncs, value = T),
    voc10 = grep("voc10_", ncs, value = T),
    voc11 = grep("voc11_", ncs, value = T),
    voc12 = grep("voc12_", ncs, value = T),
    voc13 = grep("voc13_", ncs, value = T),
    voc14 = grep("voc14_", ncs, value = T),
    voc15 = grep("voc15_", ncs, value = T),
    voc16 = grep("voc16_", ncs, value = T),
    voc17 = grep("voc17_", ncs, value = T),
    voc18 = grep("voc18_", ncs, value = T),
    voc19 = grep("voc19_", ncs, value = T),
    voc20 = grep("voc20_", ncs, value = T),
    vpc21 = grep("voc21_", ncs, value = T),
    voc22 = grep("voc22_", ncs, value = T),
    voc23 = grep("voc23_", ncs, value = T),
    voc24 = grep("voc24_", ncs, value = T),
    voc25 = grep("voc25_", ncs, value = T),
    co = grep("CO_", ncs, value = T),
    nox = grep("NOx_", ncs, value = T),
    nmvoc = grep("NMVOC_", ncs, value = T),
    so2 = grep("SO2_", ncs, value = T),
    nh3 = grep("NH3_", ncs, value = T),
    pm10 = grep("PM10_", ncs, value = T),
    pm25 = grep("PM2.5_", ncs, value = T),
    bc = grep("BC_", ncs, value = T),
    oc = grep("_OC_", ncs, value = T))

  cat("Detecting the following files:\n")

  print(as.data.frame(cbind(lncs)))

  la <- unique(unlist(lapply(lncs, length)))

  if(length(la) > 1 & !merge) stop("There should be 1 NetCDF per pollutantt")

  fr <- function(x) raster::rotate(raster::raster(x))*1000*3600*1000*1000


  #EDGAR ####

  cat("EDGAR data: ")
  NCS_EDGAR <- data.frame(
    GEIA_id = c(paste0("voc", 1:25),
                c("co", "nox", "nmvoc","so2", "nh3",
                  "pm10", "pm2.5", "bc", "oc")))


  dte <- cbind(dte, NCS_EDGAR)

  lp <- lapply(1:length(lncs), function(i) {
    cat(dte$GEIA_id[i], " ")

    if(merge & length(lncs[[i]]) > 1) {
      cat("merging ", lncs[[i]])
      cat("\n")
      a <- do.call("+", lapply(lncs[[i]], fr))*k[i]
    } else {
      a <- fr(lncs[[i]])*k[i]
    }
    a
  })
  # lp <- lapply(1:nrow(dte), function(i){
  #   cat(dte$GEIA_id[i], " ")
  #   raster::rotate(
  #     raster::raster(
  #       dte$ncs[i]))*1000*3600*1000*1000
  # })

  # ug/m2/s
  bp <- raster::stack(lp)
  bp@history <- list("units: g km-2 h-1")
  names(bp) <- NCS_EDGAR$GEIA_id

  if(missing(chem)) {
    mech <- c("edgar", "radm", "radmsorg", "cbmz_mosaic", "cptec", "ecb05_opt1")
    choice <- utils::menu(mech, title="Choose:")
    chem <- mech[choice]
  }
  # chemical mechanisms
  if(chem == "edgar") return(bp)

  # radm ####
  if(chem == "radm"){
    E_ISO <- bp$voc10/dte[dte$GEIA_id == "voc10", ]$g_mol      # emis_opt$eradm[1]
    E_SO2 <- bp$so2/(64+16*2)                                # emis_opt$eradm[2]
    E_NO <- bp$nox*0.9/(14+16)                               # emis_opt$eradm[3]
    E_CO <- bp$co/(12+18)                                    # emis_opt$eradm[4]
    E_ETH <- bp$voc7/dte[dte$GEIA_id == "voc7", ]$g_mol        # emis_opt$eradm[5]
    E_HC3 <- bp$voc1/dte[dte$GEIA_id == "voc1", ]$g_mol +
      bp$voc2/dte[dte$GEIA_id == "voc2", ]$g_mol               # emis_opt$eradm[6]
    E_HC5 <- bp$voc3/dte[dte$GEIA_id == "voc3", ]$g_mol +
      bp$voc4/dte[dte$GEIA_id == "voc4", ]$g_mol               # emis_opt$eradm[7]
    E_HC8 <- bp$voc5/dte[dte$GEIA_id == "voc5", ]$g_mol +
      bp$voc6/dte[dte$GEIA_id == "voc6", ]$g_mol               # emis_opt$eradm[8]
    E_XYL <- bp$voc13/dte[dte$GEIA_id == "voc13", ]$g_mol +
      bp$voc14/dte[dte$GEIA_id == "voc14", ]$g_mol             # emis_opt$eradm[9]
    E_OL2 <- bp$voc9/dte[dte$GEIA_id == "voc9", ]$g_mol +
      bp$voc11/dte[dte$GEIA_id == "voc11", ]$g_mol            # emis_opt$eradm[10]
    E_OLT <- bp$voc8/dte[dte$GEIA_id == "voc8", ]$g_mol       # emis_opt$eradm[11]
    E_OLI <- bp$voc12/dte[dte$GEIA_id == "voc12", ]$g_mol     # emis_opt$eradm[12]
    E_TOL <- bp$voc15/dte[dte$GEIA_id == "voc15", ]$g_mol +
      bp$voc16/dte[dte$GEIA_id == "voc16", ]$g_mol +
      bp$voc17/dte[dte$GEIA_id == "voc17", ]$g_mol            # emis_opt$eradm[13]
    E_CSL <- bp$voc18/dte[dte$GEIA_id == "voc18", ]$g_mol +
      bp$voc19/dte[dte$GEIA_id == "voc19", ]$g_mol            # emis_opt$eradm[14]
    E_HCHO <- bp$voc21/dte[dte$GEIA_id == "voc21", ]$g_mol    # emis_opt$eradm[15]
    E_ALD <- bp$voc22/dte[dte$GEIA_id == "voc22", ]$g_mol     # emis_opt$eradm[16]
    E_KET <- bp$voc23/dte[dte$GEIA_id == "voc23", ]$g_mol     # emis_opt$eradm[17]
    E_ORA2 <- bp$voc24/dte[dte$GEIA_id == "voc24", ]$g_mol    # emis_opt$eradm[18]
    E_NH3 <- bp$nh3/(14+3)                                  # emis_opt$eradm[19]

    RADM <- raster::brick(list(
      E_ISO, E_SO2, E_NO, E_CO, E_ETH, E_HC3, E_HC5, E_HC8, E_XYL,
      E_OL2, E_OLT, E_OLI, E_TOL, E_CSL, E_HCHO, E_ALD,
      E_KET, E_ORA2, E_NH3
    ))
    names(RADM) <- emis_opt$eradm
    cat("units of gases: mol/km2/h\n")
    return(RADM)
    # radmsorg ####
  } else if(chem == "radmsorg"){
    E_ISO <- bp$voc10/dte[dte$GEIA_id == "voc10", ]$g_mol            # emis_opt$eradmsorg[1]
    E_SO2 <- bp$so2/(64+16*2)                                      # emis_opt$eradmsorg[2]
    E_NO <- bp$nox*0.9/(14+16)                                     # emis_opt$eradmsorg[3]
    E_NO2 <- bp$nox*0.1/(14+16*1)                                  # emis_opt$eradmsorg[4]
    E_CO <- bp$co/(12+18)                                          # emis_opt$eradmsorg[5]
    E_CH4 <- bp$nmvoc*(1/100)/(12+4)                               # emis_opt$eradmsorg[6]
    E_ETH <- bp$voc7/dte[dte$GEIA_id == "voc7", ]$g_mol              # emis_opt$eradmsorg[7]
    E_HC3 <- bp$voc1/dte[dte$GEIA_id == "voc1", ]$g_mol +
      bp$voc2/dte[dte$GEIA_id == "voc2", ]$g_mol                     # emis_opt$eradmsorg[8]
    E_HC5 <- bp$voc3/dte[dte$GEIA_id == "voc3", ]$g_mol +
      bp$voc4/dte[dte$GEIA_id == "voc4", ]$g_mol                     # emis_opt$eradmsorg[9]
    dte[dte$MECH == "E_HC8", ]$GEIA_id
    E_HC8 <- bp$voc5/dte[dte$GEIA_id == "voc5", ]$g_mol +
      bp$voc6/dte[dte$GEIA_id == "voc6", ]$g_mol                    # emis_opt$eradmsorg[10]
    E_XYL <- bp$voc13/dte[dte$GEIA_id == "voc13", ]$g_mol +
      bp$voc14/dte[dte$GEIA_id == "voc14", ]$g_mol                  # emis_opt$eradmsorg[11]
    E_OL2 <- bp$voc9/dte[dte$GEIA_id == "voc9", ]$g_mol +
      bp$voc11/dte[dte$GEIA_id == "voc11", ]$g_mol                  # emis_opt$eradmsorg[12]
    E_OLT <- bp$voc8/dte[dte$GEIA_id == "voc8", ]$g_mol             # emis_opt$eradmsorg[13]
    E_OLI <- bp$voc12/dte[dte$GEIA_id == "voc12", ]$g_mol           # emis_opt$eradmsorg[14]
    E_TOL <- bp$voc15/dte[dte$GEIA_id == "voc15", ]$g_mol +
      bp$voc16/dte[dte$GEIA_id == "voc16", ]$g_mol +
      bp$voc17/dte[dte$GEIA_id == "voc17", ]$g_mol                  # emis_opt$eradmsorg[15]
    E_CSL <- bp$voc18/dte[dte$GEIA_id == "voc18", ]$g_mol +
      bp$voc19/dte[dte$GEIA_id == "voc19", ]$g_mol                  # emis_opt$eradmsorg[16]
    E_HCHO <- bp$voc21/dte[dte$GEIA_id == "voc21", ]$g_mol          # emis_opt$eradmsorg[17]
    E_ALD <- bp$voc22/dte[dte$GEIA_id == "voc22", ]$g_mol           # emis_opt$eradmsorg[18]
    E_KET <- bp$voc23/dte[dte$GEIA_id == "voc23", ]$g_mol           # emis_opt$eradmsorg[19]
    E_ORA2 <- bp$voc24/dte[dte$GEIA_id == "voc24", ]$g_mol          # emis_opt$eradmsorg[20]
    E_NH3 <- bp$nh3/(14+3)                                        # emis_opt$eradmsorg[21]
    E_PM25I <- bp$pm2.5*0.2/3600                                  # emis_opt$eradmsorg[22]
    E_PM25J <- bp$pm2.5*0.8/3600                                  # emis_opt$eradmsorg[23]
    E_PM_10 <- bp$pm10/3600                                       # emis_opt$eradmsorg[24]
    E_ECI <- bp$bc*0.2/3600                                       # emis_opt$eradmsorg[25]
    E_ECJ <- bp$bc*0.8/3600                                       # emis_opt$eradmsorg[26]
    E_ORGI <- bp$oc*0.2/3600                                      # emis_opt$eradmsorg[27]
    E_ORGJ <- bp$oc*0.8/3600                                      # emis_opt$eradmsorg[28]
    E_SO4I <- bp$pm2.5*0.0077/3600                                # emis_opt$eradmsorg[29]
    E_SO4J <- bp$pm2.5*0.0623/3600                                # emis_opt$eradmsorg[30]
    E_NO3I <- bp$pm2.5*0.00247/3600                               # emis_opt$eradmsorg[31]
    E_NO3J <- bp$pm2.5*0.01053/3600                               # emis_opt$eradmsorg[32]
    E_NAAJ <- bp$pm2.5*0/3600                                     # emis_opt$eradmsorg[33]
    E_NAAI <- bp$pm2.5*0/3600                                     # emis_opt$eradmsorg[34]
    E_ORGI_A <- bp$pm2.5*0/3600                                   # emis_opt$eradmsorg[35]
    E_ORGJ_A <- bp$pm2.5*0/3600                                   # emis_opt$eradmsorg[36]
    E_ORGI_BB <- bp$pm2.5*0/3600                                  # emis_opt$eradmsorg[37]
    E_ORGJ_BB <- bp$pm2.5*0/3600                                  # emis_opt$eradmsorg[38]
    E_HCL <- bp$voc20*0.45/dte[dte$GEIA_id == "voc20", ]$g_mol[1]   # emis_opt$eradmsorg[39]
    E_CLI <- bp$voc20*0.02/dte[dte$GEIA_id == "voc20", ]$g_mol[1]   # emis_opt$eradmsorg[40]
    E_CLJ <- bp$voc20*0.08/dte[dte$GEIA_id == "voc20", ]$g_mol[1]   # emis_opt$eradmsorg[41]
    E_CH3CL <- bp$voc20*0.45/dte[dte$GEIA_id == "voc20", ]$g_mol[1] # emis_opt$eradmsorg[42]

    RADMSORG <- raster::brick(list(
      E_ISO, E_SO2, E_NO, E_NO2, E_CO, E_CH4, E_ETH, E_HC3, E_HC5,
      E_HC8, E_XYL, E_OL2, E_OLT, E_OLI, E_TOL, E_CSL, E_HCHO, E_ALD,
      E_KET, E_ORA2, E_NH3, E_PM25I, E_PM25J, E_PM_10, E_ECI, E_ECJ,
      E_ORGI, E_ORGJ, E_SO4I, E_SO4J, E_NO3I, E_NO3J, E_NAAJ, E_NAAI,
      E_ORGI_A, E_ORGJ_A, E_ORGI_BB,
      E_ORGJ_BB, E_HCL, E_CLI, E_CLJ, E_CH3CL
    ))
    names(RADMSORG) <- emis_opt$eradmsorg
    cat("units of gases: mol/km2/h\n")
    cat("units of aerosols: ug/m2/s\n")
    return(RADMSORG)
    # cbmz_mosaic ####
  } else if (chem == "cbmz_mosaic") {
    E_ISO <- bp$voc10/dte[dte$GEIA_id == "voc10", ]$g_mol   # emis_opt$ecbmz_mosaic[1]

    E_SO2 <- bp$so2/(64+16*2)                             # emis_opt$ecbmz_mosaic[2]
    E_NO <- bp$nox*0.9/(14+16)                            # emis_opt$ecbmz_mosaic[3]
    E_CO <- bp$co/(12+18)                                 # emis_opt$ecbmz_mosaic[4]
    E_ETH <- bp$voc7/dte[dte$GEIA_id == "voc7", ]$g_mol     # emis_opt$ecbmz_mosaic[5]
    E_HC3 <- bp$voc1/dte[dte$GEIA_id == "voc1", ]$g_mol +
      bp$voc2/dte[dte$GEIA_id == "voc2", ]$g_mol            # emis_opt$ecbmz_mosaic[6]
    E_HC5 <- bp$voc3/dte[dte$GEIA_id == "voc3", ]$g_mol +
      bp$voc4/dte[dte$GEIA_id == "voc4", ]$g_mol            # emis_opt$ecbmz_mosaic[7]
    E_HC8 <- bp$voc5/dte[dte$GEIA_id == "voc5", ]$g_mol +
      bp$voc6/dte[dte$GEIA_id == "voc6", ]$g_mol            # emis_opt$ecbmz_mosaic[8]
    E_XYL <- bp$voc13/dte[dte$GEIA_id == "voc13", ]$g_mol +
      bp$voc14/dte[dte$GEIA_id == "voc14", ]$g_mol          # emis_opt$ecbmz_mosaic[9]
    E_OL2 <- bp$voc9/dte[dte$GEIA_id == "voc9", ]$g_mol +
      bp$voc11/dte[dte$GEIA_id == "voc11", ]$g_mol         # emis_opt$ecbmz_mosaic[10]
    E_OLT <- bp$voc8/dte[dte$GEIA_id == "voc8", ]$g_mol    # emis_opt$ecbmz_mosaic[11]
    E_OLI <- bp$voc12/dte[dte$GEIA_id == "voc12", ]$g_mol  # emis_opt$ecbmz_mosaic[12]
    E_TOL <- bp$voc15/dte[dte$GEIA_id == "voc15", ]$g_mol +
      bp$voc16/dte[dte$GEIA_id == "voc16", ]$g_mol +
      bp$voc17/dte[dte$GEIA_id == "voc17", ]$g_mol         # emis_opt$ecbmz_mosaic[13]
    E_CSL <- bp$voc18/dte[dte$GEIA_id == "voc18", ]$g_mol +
      bp$voc19/dte[dte$GEIA_id == "voc19", ]$g_mol         # emis_opt$ecbmz_mosaic[14]
    E_HCHO <- bp$voc21/dte[dte$GEIA_id == "voc21", ]$g_mol # emis_opt$ecbmz_mosaic[15]
    E_ALD <- bp$voc22/dte[dte$GEIA_id == "voc22", ]$g_mol  # emis_opt$ecbmz_mosaic[16]
    E_KET <- bp$voc23/dte[dte$GEIA_id == "voc23", ]$g_mol  # emis_opt$ecbmz_mosaic[17]
    E_ORA2 <- bp$voc24/dte[dte$GEIA_id == "voc24", ]$g_mol # emis_opt$ecbmz_mosaic[18]
    E_NH3 <- bp$nh3/(14+3)                               # emis_opt$ecbmz_mosaic[19]
    E_NO2 <- bp$nox*0.1/(14+16*1)                        # emis_opt$ecbmz_mosaic[20]
    E_CH3OH <- bp$nmvoc*(1/100)/(12+3+16+1)              # emis_opt$ecbmz_mosaic[21]
    E_C2H5OH <- bp$nmvoc*(5/100)/(12*2+5+16+1)           # emis_opt$ecbmz_mosaic[22]
    E_PM25I <- bp$pm2.5*0.2/3600                         # emis_opt$ecbmz_mosaic[23]
    E_PM25J <- bp$pm2.5*0.8/3600                         # emis_opt$ecbmz_mosaic[24]
    E_ECI <- bp$bc*0.2/3600                              # emis_opt$ecbmz_mosaic[25]
    E_ECJ <- bp$bc*0.8/3600                              # emis_opt$ecbmz_mosaic[26]
    E_ORGI <- bp$oc*0.2/3600                             # emis_opt$ecbmz_mosaic[27]
    E_ORGJ <- bp$oc*0.8/3600                             # emis_opt$ecbmz_mosaic[28]
    E_SO4I <- bp$pm2.5*0.0077/3600                       # emis_opt$ecbmz_mosaic[29]
    E_SO4J <- bp$pm2.5*0.0623/3600                       # emis_opt$ecbmz_mosaic[30]
    E_NO3I <- bp$pm2.5*0.00247/3600                      # emis_opt$ecbmz_mosaic[31]
    E_NO3J <- bp$pm2.5*0.01053/3600                      # emis_opt$ecbmz_mosaic[32]
    E_SO4C <- bp$pm2.5*0/3600                            # emis_opt$ecbmz_mosaic[33]
    E_NO3C <- bp$pm2.5*0/3600                            # emis_opt$ecbmz_mosaic[34]
    E_ORGC <- bp$pm2.5*0/3600                            # emis_opt$ecbmz_mosaic[35]
    E_ECC <- bp$pm2.5*0/3600                             # emis_opt$ecbmz_mosaic[36]

    CBMZ_MOSAIC <- raster::brick(list(
      E_ISO, E_SO2, E_NO, E_CO, E_ETH, E_HC3, E_HC5, E_HC8, E_XYL, E_OL2,
      E_OLT, E_OLI, E_TOL, E_CSL, E_HCHO, E_ALD, E_KET, E_ORA2, E_NH3, E_NO2,
      E_CH3OH, E_C2H5OH, E_PM25I, E_PM25J, E_ECI, E_ECJ, E_ORGI, E_ORGJ, E_SO4I, E_SO4J,
      E_NO3I, E_NO3J, E_SO4C, E_NO3C, E_ORGC, E_ECC
    ))
    names(CBMZ_MOSAIC) <- emis_opt$ecbmz_mosaic
    cat("units of gases: mol/km2/h\n")
    cat("units of aerosols: ug/m2/s\n")
    return(CBMZ_MOSAIC)
    # cptec ####
  } else if (chem == "cptec") {
    E_ISO <- bp$voc10/dte[dte$GEIA_id == "voc10", ]$g_mol     # emis_opt$ecptec[1]
    E_SO2 <- bp$so2/(64+16*2)                               # emis_opt$ecptec[2]
    E_NO <- bp$nox*0.9/(14+16)                              # emis_opt$ecptec[3]
    E_NO2 <- bp$nox*0.1/(14+16*2)                           # emis_opt$ecptec[4]
    E_CO <- bp$co/(12+18)                                   # emis_opt$ecptec[5]
    E_ETH <- bp$voc7/dte[dte$GEIA_id == "voc7", ]$g_mol       # emis_opt$ecptec[6]
    E_HC3 <- bp$voc1/dte[dte$GEIA_id == "voc1", ]$g_mol +
      bp$voc2/dte[dte$GEIA_id == "voc2", ]$g_mol              # emis_opt$ecptec[7]
    E_HC5 <- bp$voc3/dte[dte$GEIA_id == "voc3", ]$g_mol +
      bp$voc4/dte[dte$GEIA_id == "voc4", ]$g_mol              # emis_opt$ecptec[8]
    E_HC8 <- bp$voc5/dte[dte$GEIA_id == "voc5", ]$g_mol +
      bp$voc6/dte[dte$GEIA_id == "voc6", ]$g_mol              # emis_opt$ecptec[9]
    E_XYL <- bp$voc13/dte[dte$GEIA_id == "voc13", ]$g_mol +
      bp$voc14/dte[dte$GEIA_id == "voc14", ]$g_mol           # emis_opt$ecptec[10]
    E_OL2 <- bp$voc9/dte[dte$GEIA_id == "voc9", ]$g_mol +
      bp$voc11/dte[dte$GEIA_id == "voc11", ]$g_mol           # emis_opt$ecptec[11]
    E_OLT <- bp$voc8/dte[dte$GEIA_id == "voc8", ]$g_mol      # emis_opt$ecptec[12]
    E_OLI <- bp$voc12/dte[dte$GEIA_id == "voc12", ]$g_mol    # emis_opt$ecptec[13]
    E_TOL <- bp$voc15/dte[dte$GEIA_id == "voc15", ]$g_mol +
      bp$voc16/dte[dte$GEIA_id == "voc16", ]$g_mol +
      bp$voc17/dte[dte$GEIA_id == "voc17", ]$g_mol           # emis_opt$ecptec[14]
    E_CSL <- bp$voc18/dte[dte$GEIA_id == "voc18", ]$g_mol +
      bp$voc19/dte[dte$GEIA_id == "voc19", ]$g_mol           # emis_opt$ecptec[15]
    E_HCHO <- bp$voc21/dte[dte$GEIA_id == "voc21", ]$g_mol   # emis_opt$ecptec[16]
    E_ALD <- bp$voc22/dte[dte$GEIA_id == "voc22", ]$g_mol    # emis_opt$ecptec[17]
    E_KET <- bp$voc23/dte[dte$GEIA_id == "voc23", ]$g_mol    # emis_opt$ecptec[18]
    E_ORA2 <- bp$voc24/dte[dte$GEIA_id == "voc24", ]$g_mol   # emis_opt$ecptec[19]
    E_NH3 <- bp$nh3/(14+3)                                 # emis_opt$ecptec[20]
    E_PM_25 <- bp$pm2.5/3600                               # emis_opt$ecptec[21]
    E_PM_10 <- bp$pm10/3600                                # emis_opt$ecptec[22]
    E_OC <- bp$oc/3600                                     # emis_opt$ecptec[23]
    E_SULF <- bp$pm2.5*0.27/3600                           # emis_opt$ecptec[24]
    E_BC <- bp$bc/3600                                     # emis_opt$ecptec[25]

    CPTEC <- raster::brick(list(
      E_ISO, E_SO2, E_NO, E_NO2, E_CO, E_ETH, E_HC3, E_HC5, E_HC8, E_XYL, E_OL2,
      E_OLT, E_OLI, E_TOL, E_CSL, E_HCHO, E_ALD, E_KET, E_ORA2, E_NH3, E_PM_25, E_PM_10,
      E_OC, E_SULF, E_BC
    ))
    names(CPTEC) <- emis_opt$ecptec
    cat("units of gases: mol/km2/h\n")
    cat("units of aerosols: ug/m2/s\n")
    return(CPTEC)
    # ecb05_opt1 ####
  } else if (chem  == "ecb05_opt1") {
    E_NO2 <- bp$nox*0.1/(14+16*2)                            # emis_opt$ecb05_opt1[1]
    E_XYL <- bp$voc13/dte[dte$GEIA_id == "voc13", ]$g_mol +
      bp$voc14/dte[dte$GEIA_id == "voc14", ]$g_mol              # emis_opt$ecb05_opt1[2]
    E_TOL <- bp$voc15/dte[dte$GEIA_id == "voc15", ]$g_mol +
      bp$voc16/dte[dte$GEIA_id == "voc16", ]$g_mol +
      bp$voc17/dte[dte$GEIA_id == "voc17", ]$g_mol              # emis_opt$ecb05_opt1[3]
    E_TERP <- bp$voc10/dte[dte$GEIA_id == "voc10", ]$g_mol*0.1  # emis_opt$ecb05_opt1[4]
    E_SO2 <- bp$so2/(64+16*2)                                 # emis_opt$ecb05_opt1[5]
    E_ORA2 <- bp$voc24/dte[dte$GEIA_id == "voc24", ]$g_mol      # emis_opt$ecb05_opt1[6]
    E_OLT <- bp$voc8/dte[dte$GEIA_id == "voc8", ]$g_mol         # emis_opt$ecb05_opt1[7]
    E_OLI <- bp$voc12/dte[dte$GEIA_id == "voc12", ]$g_mol       # emis_opt$ecb05_opt1[8]
    E_OL2 <- bp$voc9/dte[dte$GEIA_id == "voc9", ]$g_mol +
      bp$voc11/dte[dte$GEIA_id == "voc11", ]$g_mol              # emis_opt$ecb05_opt1[9]
    E_NO <- bp$nox*0.9/(14+16)                               # emis_opt$ecb05_opt1[10]
    E_NH3 <- bp$nh3/(14+3)                                   # emis_opt$ecb05_opt1[11]
    E_ISO <- bp$voc10/dte[dte$GEIA_id == "voc10", ]$g_mol*0.9  # emis_opt$ecb05_opt1[12]
    E_HCL <- bp$voc20*1/dte[dte$GEIA_id == "voc20", ]$g_mol[1] # emis_opt$ecb05_opt1[13]
    E_HCHO <- bp$voc21/dte[dte$GEIA_id == "voc21", ]$g_mol     # emis_opt$ecb05_opt1[14]
    E_ETH <- bp$voc7/dte[dte$GEIA_id == "voc7", ]$g_mol        # emis_opt$ecb05_opt1[15]
    E_CSL <- bp$voc18/dte[dte$GEIA_id == "voc18", ]$g_mol +
      bp$voc19/dte[dte$GEIA_id == "voc19", ]$g_mol             # emis_opt$ecb05_opt1[16]
    E_CO <- bp$co/(12+16)                                    # emis_opt$ecb05_opt1[17]
    E_CH3OH <- bp$nmvoc*(1/100)/(12+3+16+1)                  # emis_opt$ecb05_opt1[18]
    E_C2H5OH <- bp$nmvoc*(5/100)/(12*2+5+16+1)               # emis_opt$ecb05_opt1[19]
    #Acetaldehyde?
    E_ALD <- bp$voc22/dte[dte$GEIA_id == "voc22", ]$g_mol*0.3  # emis_opt$ecb05_opt1[20]
    #C3+ Aldehydes
    E_ALDX <- bp$voc22/dte[dte$GEIA_id == "voc22", ]$g_mol*0.7 # emis_opt$ecb05_opt1[21]
    E_HC3 <- bp$voc1/dte[dte$GEIA_id == "voc1", ]$g_mol +
      bp$voc2/dte[dte$GEIA_id == "voc2", ]$g_mol               # emis_opt$ecb05_opt1[22]
    E_HC5 <- bp$voc3/dte[dte$GEIA_id == "voc3", ]$g_mol +
      bp$voc4/dte[dte$GEIA_id == "voc4", ]$g_mol               # emis_opt$ecb05_opt1[23]
    E_HC8 <- bp$voc5/dte[dte$GEIA_id == "voc5", ]$g_mol +
      bp$voc6/dte[dte$GEIA_id == "voc6", ]$g_mol               # emis_opt$ecb05_opt1[24]
    E_KET <- bp$voc23/dte[dte$GEIA_id == "voc23", ]$g_mol      # emis_opt$ecb05_opt1[25]
    E_PM25I <- bp$pm2.5*0.2/3600                             # emis_opt$ecb05_opt1[26]
    E_PM25J <- bp$pm2.5*0.8/3600                             # emis_opt$ecb05_opt1[27]
    E_ECI <- bp$bc*0.2/3600                                  # emis_opt$ecb05_opt1[28]
    E_ECJ <- bp$bc*0.8/3600                                  # emis_opt$ecb05_opt1[29]
    E_ORGI <- bp$oc*0.2/3600                                 # emis_opt$ecb05_opt1[30]
    E_ORGJ <- bp$oc*0.8/3600                                 # emis_opt$ecb05_opt1[31]
    E_SO4I <- bp$pm2.5*0.0077/3600                           # emis_opt$ecb05_opt1[32]
    E_SO4J <- bp$pm2.5*0.0623/3600                           # emis_opt$ecb05_opt1[33]
    E_NO3I <- bp$pm2.5*0.00247/3600                          # emis_opt$ecb05_opt1[34]
    E_NO3J <- bp$pm2.5*0.01053/3600                          # emis_opt$ecb05_opt1[35]
    E_SO4C <- bp$pm2.5*0/3600                                # emis_opt$ecb05_opt1[36]
    E_NO3C <- bp$pm2.5*0/3600                                # emis_opt$ecb05_opt1[37]
    E_ORGC <- bp$pm2.5*0/3600                                # emis_opt$ecb05_opt1[38]
    E_ECC <- bp$pm2.5*0/3600                                 # emis_opt$ecb05_opt1[39]
    E_PM10 <- bp$pm10*1/3600                                 # emis_opt$ecb05_opt1[40]

    CB05 <- raster::brick(list(
      E_NO2, E_XYL, E_TOL, E_TERP, E_SO2, E_ORA2, E_OLT, E_OLI, E_OL2, E_NO,
      E_NH3, E_ISO, E_HCL, E_HCHO, E_ETH, E_CSL, E_CO, E_CH3OH, E_C2H5OH, E_ALD,
      E_ALDX, E_HC3, E_HC5, E_HC8, E_KET, E_PM25I, E_PM25J, E_ECI, E_ECJ, E_ORGI,
      E_ORGJ, E_SO4I, E_SO4J, E_NO3I, E_NO3J, E_SO4C, E_NO3C, E_ORGC, E_ECC, E_PM10
    ))
    names(CB05) <- emis_opt$ecb05_opt1
    cat("units of gases: mol/km2/h\n")
    cat("units of aerosols: ug/m2/s\n")
    return(CB05) #nocov end

  } else {
    stop("other mechanisms not implemented")
  }

}

#' Download datasets of EDGAR emissions
#'
#' @description The Emissions Database for Global Atmospheric Research (EDGAR) is a
#' project from the Joint Research Centre. They provide provides global past and
#' present day anthropogenic emissions of greenhouse gases and air
#' pollutants by country and on spatial grid. \code{\link{get_edgar}} provide
#' functions to download any of the EDGAR datasets.
#'
#'
#' @param dataset Character; name of the datasets:  "v50_AP",  "v432_AP", "v432_VOC_spec",
#'  "htap_v2_2", "v50_GHG
#' @param pol Character; one of the pollutants shown on note.
#' @param sector Character; one of the sectors shown on note.
#' @param year Integer;  years.
#' @param n Integer; Number of cores in the machine, by default the half.
#' @param destpath Character: Path to create the directory for downloads datasets
#' @param type Extension, character/ Indicates if the file should be "txt", "nc" or "other".
#' txt with untis t/year and nc with units are ug/m2/s
#' @param ask Logical; Are these URL ok?
#' @param copyright Logical; to show copyright information.
#' @param verbose Logical, to print more information.
#' @return Downloads data
#' @note
#' **I recommend 2 ways:**
#'
#' **1. include 'sector' and do not include 'pol', which download all pollutants as default**
#'
#'   get_edgar(dataset = "v432_AP",
#'             destpath = tempdir(),
#'             sector = c("TRO", "TOTALS"),
#'             year = 2012)
#'
#' **2. include 'pol' and do not include 'sector', which download all sectors as default**
#'
#'   get_edgar(dataset = "v432_AP",
#'             destpath = tempdir(),
#'             pol = c("CO", "NOx"),
#'             year = 2012)
#'
#' \tabular{llll}{
#'   dataset  \tab pollutant \tab            sector  \tab years \cr
#'   v50_AP  \tab   BC, CO,  NH3, NMVOC, NOx, OC, PM10,
#'    PM2.5, SO2  \tab "AWB" "CHE" "ENE" "FFF" "FOO_PAP" "IND" "IRO"
#'    "NFE" "NMM" "PRO" "RCO" "REF_TRF" "SWD_INC" "TNR_Aviation_CDS"
#'    "TNR_Aviation_CRS" "TNR_Aviation_LTO" "TNR_Aviation_SPS" "TNR_Other"
#'    "TNR_Ship" "TOTALS" "TRO_RES" "TRO_noRES" "AGS" "MNM" "PRU_SOL"
#'    "SWD_LDF" "WWT" "NEU"  \tab 1970-2015 \cr
#'   v50_GHG  \tab  CH4,CO2_excl_short-cycle_org_C, CO2_org_short-cycle_C, N2O
#'   \tab AGS AWB CHE ENE ENF FFF IND IRO MNM PRO PRO_COAL PRO_GAS PRO_OIL RCO
#'   REF_TRF SWD_INC SWD_LDF TNR_Aviation_CDS TNR_Aviation_CRS TNR_Aviation_LTO
#'   TNR_Aviation_SPS TNR_Other TNR_Ship TOTALS TRO WWT NEU NFE NMM PRU_SOL IDE N2O
#'   \tab 1970-2018 \cr
#'   v432  \tab  CH4,CO2_excl_short-cycle_org_C, CO2_org_short-cycle_C, N2O
#'   \tab AGS AWB CHE ENE ENF FFF IND IRO MNM PRO RCO REF_TRF SWD_INC SWD_LDF
#'   TNR_Aviation_CDS TNR_Aviation_CRS TNR_Aviation_LTO TNR_Aviation_SPS
#'   TNR_Other TNR_Ship TOTALS TRO WWT NEU NFE NMM PRU_SOL IDE N2O
#'   \tab 1970-2012 \cr
#'   v432_AP  \tab   BC, CO,  NH3, NMVOC, NOx, OC, PM10,
#'   PM2.5_bio, PM2.5_fossil, SO2  \tab AWB CHE ENE FFF FOO_PAP IND IRO NFE NMM
#'   PRO RCO REF_TRF SWD_INC TNR_Aviation_CDS TNR_Aviation_CRS TNR_Aviation_LTO
#'   TNR_Aviation_SPS TNR_Other TNR_Ship TOTALS TRO AGS MNM PRU_SOL SWD_LDF
#'   NEU \tab 1970-2012 \cr
#'   v432_VOC_spec \tab   voc1, voc2, voc3, voc3, voc5, voc6, voc7, voc8, bvoc9, voc10.
#'                       voc11, voc12, voc13, voc14, voc15, voc16, voc17, voc18, voc19, voc20,
#'                       voc21, voc22, voc23, voc24, voc25, TOTALS
#'                          \tab  ENE, IND, PPA, RCO, REF
#'                          TNR_Aviation_CDS, TNR_Aviation_CRS,
#'                TNR_Aviation_LTO, TNR_Aviation_SPS, TNR_Other, TNR_Ship, TOTALS,
#'                TRO, TRF \tab 1970-2012 \cr
#'      htap_v2_2  \tab BC, CO, NH3, NMVOC, NOx, OCm, PM10, PM2.5, SO2 \tab
#'      AIR, ENERGY, INDUSTRY, RESIDENTIAL, SHIPS TRANSPORT \tab 2008 and 2010 \\cr
#' }
#' voc11 only 2008
#'
#'
#' @references
#'
#'- **v50_AP** Crippa, M., Solazzo, E., Huang, G., Guizzardi, D., Koffi, E.,
#'Muntean, M., Schieberle, C., Friedrich, R., Janssens-Maenhout, G.:
#'High resolution temporal profiles in the Emissions Database for
#'Global Atmospheric Research, Nature Scientific Data, 2020, in press.
#'- **v432**:  Muntean, M., Guizzardi, D., Schaaf, E., Crippa, M., Solazzo, E.,
#'Olivier, J.G.J., Vignati, E. Fossil CO2 emissions of all world countries - 2018
#'Report, EUR 29433 EN, Publications Office  of the European Union, Luxembourg,
#'2018, ISBN 978-92-79-97240-9, doi:10.2760/30158, JRC113738.
#'- **v432_AP**:Crippa, M., Guizzardi, D., Muntean, M., Schaaf, E., Dentener, F.,
#'van Aardenne, J. A., Monni, S., Doering, U., Olivier, J. G. J., Pagliari, V.,
#'and Janssens-Maenhout, G.: Gridded emissions of air pollutants for the period
#'1970–2012 within EDGAR v4.3.2, Earth Syst. Sci. Data, 10, 1987–2013,
#'https://doi.org/10.5194/essd-10-1987-2018, 2018.
#'- **v432_VOC_spec**: Huang, G., Brook, R., Crippa, M., Janssens-Maenhout, G.,
#'Schieberle, C., Dore, C., Guizzardi, D., Muntean, M., Schaaf, E., and Friedrich, R.:
#'Speciation of anthropogenic emissions of non-methane volatile organic compounds: a
#'global gridded data set for 1970–2012, Atmos. Chem. Phys., 17, 7683-7701,
#'https://doi.org/10.5194/acp-17-7683-2017, 2017.
#'- **htap_v2_2**: Janssens-Maenhout, G., Crippa, M., Guizzardi, D.,
#'Dentener, F., Muntean, M., Pouliot, G., Keating, T., Zhang, Q., Kurokawa, J.,
#'Wankmüller, R., Denier van der Gon, H., Kuenen, J. J. P., Klimont, Z.,
#'Frost, G., Darras, S., Koffi, B., and Li, M.: HTAP_v2.2: a mosaic of
#'regional and global emission grid maps for 2008 and 2010 to study
#'hemispheric transport of air pollution, Atmos. Chem. Phys., 15,
#'11411–11432, https://doi.org/10.5194/acp-15-11411-2015, 2015.
#'
#' MNM is MNN for NOx v432_AP
#'
#' @importFrom utils download.file askYesNo data
#' @importFrom parallel detectCores mclapply
#' @export
#' @examples \donttest{
#' # see all the links:
#' data(edgar)
#' head(edgar)
#' # Download all pollutants for sector
#' get_edgar(dataset = "v50_AP",
#'           pol = "CO",
#'           sector = "TOTALS",
#'           year = 2014,
#'           destpath = tempdir(),
#'           type = "nc",
#'           ask = FALSE)
#' }
get_edgar <- function(dataset = "v50_AP",
                      pol,
                      sector,
                      year,
                      n = parallel::detectCores()/2,
                      destpath = tempdir(),
                      type = "nc",
                      ask = TRUE,
                      copyright = TRUE,
                      verbose = TRUE){
  if(copyright) message(
    paste0(c(
      " Copyright notice\n",
      "------------------\n",
      " (c) European Union, 1995-2019\n\n",
      " Reuse is authorised, provided the source is acknowledged. The reuse policy \n",
      " of the European Commission is implemented by a Decision of 12 December 2011, see\n",
      " http://eur-lex.europa.eu/LexUriServ/LexUriServ.do?uri=OJ:L:2011:330:0039:0042:EN:PDF\n\n"
    ))
  )
  ed <- sysdata$edgar

  eda <- ed
  ed <- ed[ed$data %in% dataset, ]
  if(nrow(ed) == 0) {
    cat("Please, choose one of the following datasets:\n", unique(eda$data), "\n") # nocov
    stop("No dataset")                                                             # nocov
  }

  eda <- ed
  if(missing(year)){
    warning("Downloading all available years")                                    # nocov
  } else {
    ed <- ed[ed$year %in% year, ]
    if(nrow(ed) == 0) {
      cat("Please, choose one of the following years:\n", unique(eda$year), "\n") # nocov
      stop("No Year")                                                             # nocov
    }
  }

  eda <- ed
  if(missing(sector)){
    warning("Downloading all available sector")                                   # nocov
  } else {
    ed <- ed[ed$sector %in% sector, ]
    if(nrow(ed) == 0) {
      cat("Please, choose one of the following sector:\n", unique(eda$sector), "\n") # nocov
      stop("No sector")                                                              # nocov
    }
  }

  eda <- ed
  if(missing(pol)){
    warning("Downloading all available pollutants") # nocov
  } else {
    ed <- ed[ed$pol %in% pol, ]
    if(nrow(ed) == 0) {
      cat("Please, choose one of the following pollutants:\n", unique(eda$pol), "\n")
      stop("No pollutants")
    }
  }

  # nocov start

  eda <- ed
  if(missing(type)){
    warning("Downloading all available pollutants")
  } else {
    ed <- ed[ed$type %in% type, ]
    if(nrow(ed) == 0) {
      cat("Please, choose one of the following pollutants:\n", unique(eda$type), "\n")
      stop("No type")
    }
  }

  if(verbose)  cat("Downloading the following data:\n")
  if(verbose)  cat(ed$url)

  if(ask){  # nocov start
    a <- utils::askYesNo("Are these links ok?")
    if(!a) stop("Make a selection again. Remember, you can see the urls with data(edgar)")
  } # nocov end


  # paths
  if(missing(n)) {
    for (i in 1:length(ed$url)){
      utils::download.file(url = ed$url[i],
                           destfile =  paste0(destpath, "/", ed$links[i]))
      message(paste0(
        "Files at ", destpath,  "\n"))
    }
  } else {
    parallel::mclapply(seq_along(ed$url), function(i) {
      utils::download.file(paste0(url = ed$url[i]),
                           destfile =  paste0(destpath, "/", ed$links[i]))

    }, mc.cores = n)
  }
  # nocov end
}

#' Download datasets of EDGAR emissions
#'
#' @description The Emissions Database for Global Atmospheric Research (EDGAR) is a
#' project from the Joint Research Centre. They provide provides global past and
#' present day anthropogenic emissions of greenhouse gases and air
#' pollutants by country and on spatial grid. \code{\link{get_edgar}} provide
#' functions to download any of the EDGAR datasets.
#'
#'
#' @param dataset Character; name of the datasets: "v432", "v432_AP", "v432_VOC_spec",
#' "v4tox2", "htap_v2_2"
#' @param pol Character; one of the pollutants shown on note.
#' @param sector Character; one of the sectors shown on note.
#' @param year Integer; on of the years shown on note.
#' @param destpath Character: Path to create the directory for downloads datasets
#' @param txt Logical; if TRUE, download data as .txt and untis t/year,
#' if FALSE units are ug/m2/s
#' @param return_url Logical; return url?
#' @param copyright Logical; to show copyright information.
#' @return Downloads data
#' @note
#'
#' **I recommend 2 ways:**
#'
#' **1. include 'sector' and dont include 'pol', which download all pollutants as default**
#'
#'   get_edgar(dataset = "v432_AP",
#'             destpath = tempdir(),
#'             sector = c("TRO", "TOTALS"),
#'             year = 2012)
#'
#' **2. include 'pol' and dont include 'sector', which download all sectors as default**
#'
#'   get_edgar(dataset = "v432_AP",
#'             destpath = tempdir(),
#'             pol = c("CO", "NOx"),
#'             year = 2012)
#'
#' \tabular{llll}{
#'   dataset  \tab pollutant \tab            sector  \tab years \cr
#'      v432  \tab    CH4, N2O, CO2_excl_short-cycle_org_C, CO2_org_short-cycle_C
#'            \tab     AGS, CHE, ENE, ENF, FFF, IND, IRO, MNM, PRO, REF_TRF, SWD_INC,
#'             SWD_LDF, TNR_Aviation_CDS, TNR_Aviation_CRS,
#'             TNR_Aviation_LTO, TNR_Aviation_SPS, TNR_Other, TNR_Ship,
#'             TOTALS, TRO, WWT \tab 1970-2012 \cr
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
#'      v4tox2  \tab    HG, HG_D, HG_G, HG_P \tab Iro,  cement, chlor, comb_power_ind,
#'      comb_res_oth, glass, gold_A, gold_L, nfe_oth, shipping, totals, tro_roa,
#'      waste \tab 1970-2008  \cr
#'      htap_v2_2  \tab BC, CO, NH3, NMVOC, NOx, OCm, PM10, PM2.5, SO2 \tab
#'      AIR, ENERGY, INDUSTRY, RESIDENTIAL, SHIPS TRANSPORT \tab 2008 and 2010 \\cr
#' }
#' voc11 only 2008
#'
#'
#' @references
#'
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
#'- **v4tox2**: Muntean, M., Janssens-Maenhout, G., Song, S., Giang, A., Selin, N. E.,
#'Zhong, H., ... & Schaaf, E. (2018). Evaluating EDGARv4. tox2 speciated mercury
#'emissions ex-post scenarios and their impacts on modelled global and regional
#'wet deposition patterns. Atmospheric Environment, 184, 56-68.
#'- **htap_v2_2**: Janssens-Maenhout, G., Crippa, M., Guizzardi, D.,
#'Dentener, F., Muntean, M., Pouliot, G., Keating, T., Zhang, Q., Kurokawa, J.,
#'Wankmüller, R., Denier van der Gon, H., Kuenen, J. J. P., Klimont, Z.,
#'Frost, G., Darras, S., Koffi, B., and Li, M.: HTAP_v2.2: a mosaic of
#'regional and global emission grid maps for 2008 and 2010 to study
#'hemispheric transport of air pollution, Atmos. Chem. Phys., 15,
#'11411–11432, https://doi.org/10.5194/acp-15-11411-2015, 2015.
#'
#'MNM is MNN for NOx v432_AP
#'
#' @importFrom utils download.file
#' @export
#' @examples \dontrun{
#' # Download all pollutants for sector TRO
#' get_edgar(dataset = "v432_AP", destpath = tempdir(),
#' sector = c("TOTALS"),
#' year = 2012)
#' # Download all sectors for pollutant CO
#' get_edgar(dataset = "v432_AP", destpath = tempdir(),
#' pol = c("CO"),
#' year = 2012)
#' }
get_edgar <- function(dataset = "v432_AP",
                      pol,
                      sector,
                      year,
                      destpath = tempdir(),
                      txt = TRUE,
                      return_url = TRUE,
                      copyright = TRUE){
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


  # Check
  if(length(dataset) > 1) stop("Only one dataset per time")      # nocov start
  if(dataset == "v432_AP") {
    ed <- sysdata$v432_AP
  } else if(dataset == "v432_VOC_spec"){
    ed <- sysdata$v432_VOC
  } else {
    stop("Sorry, we are updating other datasets")
  }

  # links
  df <- ed[ed$dataset == dataset, ]

  if(missing(year)){
    stop("missing year")
  } else if(length(year) > 1){
    stop("One year per time please")
  }

  if(missing(sector)){
    if(missing(pol)) stop("Include pollutants")
    df <- ed[ed$dataset %in% dataset &
               ed$pol %in% pol, ]

  } else if(missing(pol)){
    if(missing(sector))stop("Include sector")
    df <- ed[ed$dataset %in% dataset &
               ed$sector %in% sector, ]

  } else {
    df <- ed[ed$dataset %in% dataset &
               ed$pol %in% pol &
               ed$sector %in% sector, ]

  }
  # more checks
  if(dataset == "htap_v2_2") {
    if(!year %in% c(2008, 2010)) {
      stop("When dataset is htap_v2_2, years can be 2008 or 2010 only")
    }
  } else {
    if(!year %in% 1970:2012){
      stop("For this datasets, years go from 1970 to 2012 only")
    }
  }                                                                      # nocov end

  links = unlist(lapply(1:length(df$URL), function(i) {
    year <- eval(parse(text = df$years[i]))
    eval(parse(text = df$URL[i]))
  }))

  txzz <- paste0(eval(parse(text = df$years)),
                 "_", df$pol, "_",df$sector,
                 ".zip")
  if(txt){
    links <- gsub(pattern = ".0.1x0.1", replacement = "", x = links)
    txzz <- gsub(pattern = ".0.1x0.1", replacement = "", x = txzz)
  }
  df$links <- links
  df$txzz <- txzz
  # paths
  for (i in 1:length(links)){
    utils::download.file(paste0(url = df$links[i]),
                         destfile =  paste0(destpath, "/", df$txzz[i]))
    message(paste0(
      "Files at ", destpath,  "\n"))
  }
  if(return_url) return(links)
}

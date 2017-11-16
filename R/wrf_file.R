#' Create emission files to the WRF-Chem model
#'
#' @description Create an emission file from wrfinput
#'
#' @param wrfinput_dir folder with the wrfinput file(s)
#' @param wrfchemi_dir output folder
#' @param domains domain or domains to processed
#' @param frames_per_auxinput5 value from wrf &time_control namelist.input, numer of times in a single emission file
#' @param auxinput5_interval_m value from wrf &time_control namelist.input, interval in minutes betwen diferent times
#' @param day_offset number of days (can be an fracion) to crate multiple files
#' @param io_style_emissions from wrf &chem namelist.input
#' @param kemit from wrf &chem namelist.input, numer of levels of the emission file
#' @param variaveis emission species
#' @param n_aero number of aerosol species
#' @param COMPRESS integer between 1 (least compr) and 9 (most compr) or NA for no compression
#' @param verbose print file info
#' @param force_ncdf4 force NetCDF4 format
#' @note works only with io_style_emissions = 2
#'
#' @export
#'
#' @examples \dontrun{
#' # Do not run
#'
#' # emissions for a 1 day forecast for domains 1 and 2
#' # the folder WRF_input has to contain wrfinput_d01 and wrfinput_d02
#'
#' wrf_emisson(wrfinput_dir         = "WRF_INPUT",
#'             wrfchemi_dir         = "EMISS",
#'             domains              = 1:2,
#'             frames_per_auxinput5 = 24,
#'             auxinput5_interval_m = 60,
#'             day_offset           = 0)
#'
#' # emission for the last timestep
#'
#' wrf_emisson(wrfinput_dir         = "WRF_INPUT",
#'             wrfchemi_dir         = "EMISS",
#'             domains              = 1:2,
#'             frames_per_auxinput5 = 1,
#'             auxinput5_interval_m = 60,
#'             day_offset           = 1)
#'}

wrf_emisson <- function(wrfinput_dir         = "",
                        wrfchemi_dir         = "",
                        domains              = 1,
                        frames_per_auxinput5 = 1,
                        auxinput5_interval_m = 60,
                        day_offset           = 0,
                        io_style_emissions   = 2,
                        kemit                = 1,
                        variaveis = c("E_NO2","E_NO","E_TOL","E_XYL","E_ALD","E_ALDX","E_SO2","E_CO",
                                      "E_OLT","E_OLI","E_OL2","E_NH3","E_ISO","E_HCL","E_HCHO","E_ETH",
                                      "E_CH3OH","E_C2H5OH","E_HC3","E_HC5","E_HC8","E_KET","E_ORA2",
                                      "E_CSL","E_TERP","E_PM25I","E_PM25J","E_ECI","E_ECJ","E_ORGI",
                                      "E_ORGJ","E_SO4I","E_SO4J","E_NO3J","E_NO3I","E_SO4C","E_NO3C",
                                      "E_ORGC","E_ECC","E_PM10"),
                        n_aero               = 15,
                        COMPRESS             = NA,
                        verbose              = FALSE,
                        force_ncdf4          = FALSE)
{
  a <- Sys.info()["sysname"]
  if(a[[1]] == "Windows") linux = F else linux = T # to avoid special chacacteres in the filename

  for(domain in domains){
    # basic information from wrfinput
    wrfinput     <- paste(wrfinput_dir,"/wrfinput_d0",domain,sep="")
    wrfinput     <- nc_open(wrfinput,write = F)
    input_time   <- ncvar_get(wrfinput,"Times")

    date <- as.character(paste(substr(input_time,1,10),substr(input_time,12,19)))
    date <- as.POSIXct(strptime(date, "%Y-%m-%d %H:%M:%S"))
    date <- date + 86400 * day_offset
    hora         <- format(date,"%H")
    minuto       <- format(date,"%M")
    segundo      <- format(date,"%S")
    hora         <- paste(formatC(hora,    width = 2, format = "d", flag = "0"))
    minuto       <- paste(formatC(minuto,  width = 2, format = "d", flag = "0"))
    segundo      <- paste(formatC(segundo, width = 2, format = "d", flag = "0"))

    # future work
    # if(io_style_emissions == 1){
    #   frames_per_auxinput5 <- 12
    # }
    if(io_style_emissions ==2){
      if(linux){
        file_name         <- paste(wrfchemi_dir,"/wrfchemi_d0",domain,"_",format(date,"%Y-%m-%d"),"_",hora,":",minuto,":",segundo,sep="")
      } else  file_name   <- paste(wrfchemi_dir,"/wrfchemi_d0",domain,"_",format(date,"%Y-%m-%d"),"_",hora,"_",minuto,"_",segundo,sep="")
    }

    if(frames_per_auxinput5 == 1){
      file_time  <- paste(date,"_",hora,":",minuto,":",segundo,sep="")
    }
    else{
      file_time    <- character(frames_per_auxinput5)
      for(i in 1:frames_per_auxinput5){
        file_time[i] <- paste(format(date,"%Y-%m-%d"),"_",hora,":",minuto,":",segundo,sep="")
        date         <- date + 60 * auxinput5_interval_m
        hora         <- format(date,"%H")
        minuto       <- format(date,"%M")
        segundo      <- format(date,"%S")
        hora         <- paste(formatC(hora,    width = 2, format = "d", flag = "0"))
        minuto       <- paste(formatC(minuto,  width = 2, format = "d", flag = "0"))
        segundo      <- paste(formatC(segundo, width = 2, format = "d", flag = "0"))
      }
    }

    input_lat    <- ncvar_get(wrfinput,"XLAT")
    input_lon    <- ncvar_get(wrfinput,"XLONG")
    g_atributos  <- ncatt_get(wrfinput,0)
    g_atributos  <- c( list(Title = "Anthropogenic emissions",
                            History = paste("created on",format(Sys.time(),"%Y-%m-%d at %H:%M")),
                            Author = "function by Schuch (2017)"),
                       g_atributos[4:length(g_atributos)])
    # definition of dimensions
    west_east           <- ncdim_def("west_east",units = "",longname = "",vals = 1:g_atributos$`WEST-EAST_PATCH_END_UNSTAG`)
    south_north         <- ncdim_def("south_north",units = "",longname = "",vals = 1:g_atributos$`SOUTH-NORTH_PATCH_END_UNSTAG`)
    emissions_zdim_stag <- ncdim_def("emissions_zdim_stag",units = "",longname = "",vals = 1:kemit)
    DateStrLen          <- ncdim_def("DateStrLen",units = "",longname = "",vals = 1:19)
    Time                <- ncdim_def("Time",units = "",longname = "",vals = 1:frames_per_auxinput5, unlim=TRUE)
    # definition of variables
    Times               <- ncvar_def(name = "Times",dim = list(DateStrLen,Time),units = "",      prec = "char", compression = COMPRESS)
    XLONG               <- ncvar_def(name = "XLONG",units = "",dim = list(west_east,south_north),prec = "float",compression = COMPRESS)
    XLAT                <- ncvar_def(name = "XLAT" ,units = "",dim = list(west_east,south_north),prec = "float",compression = COMPRESS)
    # GAS fase emissions
    for(i in 1:(length(variaveis) - n_aero)){
      assign(variaveis[i], ncvar_def(name = variaveis[i],units = "",dim = list(west_east,south_north,emissions_zdim_stag,Time),
                                     prec="float",compression = COMPRESS))
    }
    # AEROSOL emissions
    for(i in (1+length(variaveis) - n_aero):length(variaveis)){
      assign(variaveis[i], ncvar_def(name = variaveis[i],units = "",dim = list(west_east,south_north,emissions_zdim_stag,Time),
                                     prec="float",compression = COMPRESS))
    }
    emiss_file <- nc_create(filename=file_name,vars=c(list('Times'=Times,'XLAT'=XLAT,'XLONG'=XLONG),mget(ls(pattern = "E_"))),force_v4 = force_ncdf4)
    for(i in 1:length(g_atributos)){
      ncatt_put(emiss_file,varid = 0,attname = names(g_atributos)[i],attval = g_atributos[[i]])
    }
    # values for the basic variables
    ncvar_put(emiss_file,"Times",file_time)
    ncvar_put(emiss_file,"XLONG",input_lon)
    ncatt_put(emiss_file,varid = "XLONG",attname = "MemoryOrder",attval = "XY")
    ncatt_put(emiss_file,varid = "XLONG",attname = "description",attval = "LONGITUDE, WEST IS NEGATIVE")
    ncatt_put(emiss_file,varid = "XLONG",attname = "units",attval = "degree east")
    ncatt_put(emiss_file,varid = "XLONG",attname = "stagger",attval = "")
    ncatt_put(emiss_file,varid = "XLONG",attname = "FieldType",attval = 104)
    ncvar_put(emiss_file,"XLAT",input_lat)
    ncatt_put(emiss_file,varid = "XLAT",attname = "MemoryOrder",attval = "XY")
    ncatt_put(emiss_file,varid = "XLAT",attname = "description",attval = "LATITUDE, SOUTH IS NEGATIVE")
    ncatt_put(emiss_file,varid = "XLAT",attname = "units",attval = "degree north")
    ncatt_put(emiss_file,varid = "XLAT",attname = "stagger",attval = "")
    ncatt_put(emiss_file,varid = "XLAT",attname = "FieldType",attval = 104)
    # vetor of zeros
    zero <- array(0,c(west_east$len,south_north$len,emissions_zdim_stag$len,Time$len))
    # GASES inicializat5ion with zeros
    for(i in 1:(length(variaveis) - n_aero)){
      ncvar_put(emiss_file,varid = variaveis[i],zero)
      ncatt_put(emiss_file,varid = variaveis[i],attname = "MemoryOrder",attval = "XYZ")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "description",attval = "EMISSIONS")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "units",attval = "mol km^-2 hr^-1")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "stagger",attval = "Z")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "FieldType",attval = 104)
    }
    # AEROSOIS inicializat5ion with zeros
    for(i in (1+length(variaveis) - n_aero):length(variaveis)){
      ncvar_put(emiss_file,varid = variaveis[i],zero)
      ncatt_put(emiss_file,varid = variaveis[i],attname = "MemoryOrder",attval = "XYZ")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "description",attval = "EMISSIONS")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "units",attval = "ug m^-2 s^-1")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "stagger",attval = "Z")
      ncatt_put(emiss_file,varid = variaveis[i],attname = "FieldType",attval = 104)
    }
    if(verbose){
      print(emiss_file)
    }
    nc_close(emiss_file)
  }
}

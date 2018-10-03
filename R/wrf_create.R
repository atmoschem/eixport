#' Create emission files to the WRF-Chem
#'
#' @description Create an emission file from wrfinput file(s),
#'
#' @param wrfinput_dir folder with the wrfinput file(s)
#' @param wrfchemi_dir output folder
#' @param domains domain or domains to process
#' @param frames_per_auxinput5 value from wrf &time_control namelist.input,
#' number of times in a single emission file
#' @param auxinput5_interval_m value from wrf &time_control namelist.input,
#' interval in minutes between different times
#' @param day_offset number of days (can be a fraction) to create multiple files
#' @param io_style_emissions from wrf &chem namelist.input
#' @param kemit from wrf &chem namelist.input, number of levels of the emission file
#' @param variaveis emission species, can be used data(emis_opt)
#' @param n_aero number of aerosol species
#' @param COMPRESS integer between 1 (least compr) and 9 (most compr) or NA for
#' no compression
#' @param force_ncdf4 force NetCDF4 format
#' @param verbose print file info
#' @note using io_style_emissions = 1, the 00z will be generated with day_offset = 0 and
#' 12z with day_offset = 0.5
#'
#' @note Windows users need to rename the emission files
#' from 'wrfchemi_d01_2011-08-01_00_00_00' to 'wrfchemi_d01_2011-08-01_00:00:00'
#' or change in namelist the defoult filename before run wrf.exe with these files
#'
#' @author Daniel Schuch
#'
#' @import ncdf4
#'
#' @export
#'
#' @seealso \code{\link{to_wrf}} and \code{\link{emis_opt}}
#'
#' @examples \dontrun{
#' # Do not run
#'
#' # emissions for a 1 day forecast for domains 1 and 2
#'
#' dir.create(file.path(tempdir(), "EMISS"))
#'
#' # emissions on date_hour style
#' wrf_create(wrfinput_dir         = system.file("extdata", package = "eixport"),
#'            wrfchemi_dir         = file.path(tempdir(), "EMISS"),
#'            domains              = 1:2,
#'            frames_per_auxinput5 = 25,
#'            auxinput5_interval_m = 60,
#'            verbose              = TRUE)
#'
#' # emissions on 00z / 12z style, create the 00z
#' wrf_create(wrfinput_dir         = system.file("extdata", package = "eixport"),
#'            wrfchemi_dir         = file.path(tempdir(), "EMISS"),
#'            domains              = 1:2,
#'            io_style_emissions   = 1,
#'            day_offset           = 0,
#'            verbose              = TRUE,
#'            )
#' # emissions on 00z / 12z style, create the 12z
#' wrf_create(wrfinput_dir         = system.file("extdata", package = "eixport"),
#'            wrfchemi_dir         = file.path(tempdir(), "EMISS"),
#'            domains              = 1:2,
#'            io_style_emissions   = 1,
#'            day_offset           = 0.5,
#'            verbose              = TRUE)
#'}

wrf_create  <- function(wrfinput_dir         = "",
                        wrfchemi_dir         = "",
                        domains              = 1,
                        frames_per_auxinput5 = 1,
                        auxinput5_interval_m = 60,
                        day_offset           = 0,
                        io_style_emissions   = 2,
                        kemit                = 1,
                        variaveis            = "ecbmz_mosaic",
                        n_aero               = 15,
                        COMPRESS             = NA,
                        force_ncdf4          = FALSE,
                        verbose              = FALSE)
{
  a <- Sys.info()["sysname"]
  # to avoid special chacacteres in the filename
  if(a[[1]] == "Windows") linux = F else linux = T # nocov

  if(length(variaveis) == 1){
    emis_opt <- NULL
    load(system.file("data/emis_opt.rda", package = "eixport"))
    if(variaveis %in% names(emis_opt)){
      variaveis <- emis_opt[[variaveis]]
    }else{
      cat(paste(variaveis,"is not valid, use one of:\n"))           # nocov
      cat(names(emis_opt))                                          # nocov
      stop("name, numeric value or a set of variable names")        # nocov
    }

  }

  for(domain in domains){
    # basic information from wrfinput
    wrfinput     <- paste(wrfinput_dir, "/wrfinput_d0", domain, sep = "")
    wrfinput     <- ncdf4::nc_open(wrfinput,write = F)
    input_time   <- ncdf4::ncvar_get(wrfinput,"Times")

    date <- as.character(paste(substr(input_time, 1, 10),
                               substr(input_time, 12, 19)))
    date <- as.POSIXct(strptime(date, "%Y-%m-%d %H:%M:%S"))

    if(io_style_emissions == 1){
      if(day_offset != 0){
        day_offset <- 0.5
      }
    }

    date <- date + 86400 * day_offset
    hora         <- format(date,"%H")
    minuto       <- format(date,"%M")
    segundo      <- format(date,"%S")
    hora         <- paste(formatC(hora,    width = 2, format = "d", flag = "0"))
    minuto       <- paste(formatC(minuto,  width = 2, format = "d", flag = "0"))
    segundo      <- paste(formatC(segundo, width = 2, format = "d", flag = "0"))

    if(io_style_emissions == 1){
      frames_per_auxinput5 <- 12  # nocov start
      if(day_offset == 0){
        h <- "00z"
      }else{
        h <- "12z"
      }
      file_name <- paste(wrfchemi_dir, "/wrfchemi_", h, "_", "d0",domain, sep = "") # nocov end
    }
    if(io_style_emissions ==2){                                                     # nocov start
      if(linux){
        file_name <- paste(wrfchemi_dir, "/wrfchemi_d0", domain, "_",
                           format(date,"%Y-%m-%d"),
                                   "_",hora,":", minuto,":", segundo, sep = "")
      } else  file_name <- paste(wrfchemi_dir, "/wrfchemi_d0", domain, "_",
                                 format(date,"%Y-%m-%d"),
                                 "_", hora, "_", minuto, "_", segundo, sep = "")   # nocov end
    }

    if(frames_per_auxinput5 == 1){
      file_time  <- paste(format(date,"%Y-%m-%d"),
                          "_", hora, ":", minuto, ":", segundo, sep = "")
    }
    else{                                                                   # nocov start
      file_time    <- character(frames_per_auxinput5)
      for(i in 1:frames_per_auxinput5){
        file_time[i] <- paste(format(date,"%Y-%m-%d"),
                              "_", hora, ":", minuto, ":", segundo, sep = "")
        date         <- date + 60 * auxinput5_interval_m
        hora         <- format(date,"%H")
        minuto       <- format(date,"%M")
        segundo      <- format(date,"%S")
        hora         <- paste(formatC(hora,
                                      width = 2,
                                      format = "d",
                                      flag = "0"))
        minuto       <- paste(formatC(minuto,
                                      width = 2,
                                      format = "d",
                                      flag = "0"))
        segundo      <- paste(formatC(segundo,
                                      width = 2,
                                      format = "d",
                                      flag = "0"))
      }                                                                      # nocov end
    }

    input_lat    <- ncdf4::ncvar_get(wrfinput, "XLAT")
    input_lon    <- ncdf4::ncvar_get(wrfinput, "XLONG")
    g_atributos  <- ncdf4::ncatt_get(wrfinput, 0)
    g_atributos  <- c( list(Title = "Anthropogenic emissions",
                            History = paste("created on",
                                            format(Sys.time(),
                                                   "%Y-%m-%d at %H:%M")),
                            Author = paste0("R package eixport v",
                                            utils::packageVersion("eixport"))),
                       g_atributos[4:length(g_atributos)])
    # definition of dimensions
    west_east <- ncdf4::ncdim_def("west_east",
                                  units = "",
                                  longname = "",
                                  vals = 1:g_atributos$`WEST-EAST_PATCH_END_UNSTAG`)
    south_north <- ncdf4::ncdim_def("south_north",
                                    units = "",
                                    longname = "",
                                    vals = 1:g_atributos$`SOUTH-NORTH_PATCH_END_UNSTAG`)
    emissions_zdim_stag <- ncdf4::ncdim_def("emissions_zdim_stag",
                                            units = "",
                                            longname = "",
                                            vals = 1:kemit)
    DateStrLen          <- ncdf4::ncdim_def("DateStrLen",
                                            units = "",
                                            longname = "",
                                            vals = 1:19)
    Time                <- ncdf4::ncdim_def("Time",
                                            units = "",
                                            longname = "",
                                            vals = 1:frames_per_auxinput5,
                                            unlim = TRUE)
    # definition of variables
    Times <- ncdf4::ncvar_def(name = "Times",
                              dim = list(DateStrLen,Time),
                              units = "",
                              prec = "char",
                              compression = COMPRESS)
    XLONG <- ncdf4::ncvar_def(name = "XLONG",
                              units = "",
                              dim = list(west_east,south_north),
                              prec = "float",
                              compression = COMPRESS)
    XLAT <- ncdf4::ncvar_def(name = "XLAT" ,
                             units = "",
                             dim = list(west_east, south_north),
                             prec = "float",
                             compression = COMPRESS)
    # GAS phase emissions
    for(i in 1:(length(variaveis) - n_aero)){
      assign(variaveis[i],
             ncdf4::ncvar_def(name = variaveis[i],
                              units = "",
                              dim = list(west_east,
                                         south_north,
                                         emissions_zdim_stag,
                                         Time),
                              prec="float",
                              compression = COMPRESS))
    }
    # AEROSOLS emissions
    for(i in (1+length(variaveis) - n_aero):length(variaveis)){
      assign(variaveis[i],
             ncdf4::ncvar_def(name = variaveis[i],
                              units = "",
                              dim = list(west_east,
                                         south_north,
                                         emissions_zdim_stag,
                                         Time),prec="float",
                              compression = COMPRESS))
    }
    emiss_file <- nc_create(filename = file_name,
                            vars = c(list('Times' = Times,
                                          'XLAT' = XLAT,
                                          'XLONG' = XLONG),
                                     mget(ls(pattern = "E_"))),
                            force_v4 = force_ncdf4)
    for(i in 1:length(g_atributos)){
      ncdf4::ncatt_put(emiss_file,
                       varid = 0,
                       attname = names(g_atributos)[i],
                       attval = g_atributos[[i]])
    }
    # values for the basic variables
    ncdf4::ncvar_put(emiss_file,
                     "Times",
                     file_time)
    ncdf4::ncvar_put(emiss_file,
                     "XLONG",
                     input_lon)
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLONG",
                     attname = "MemoryOrder",
                     attval = "XY")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLONG",
                     attname = "description",
                     attval = "LONGITUDE, WEST IS NEGATIVE")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLONG",
                     attname = "units",
                     attval = "degree east")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLONG",
                     attname = "stagger",
                     attval = "")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLONG",
                     attname = "FieldType",
                     attval = 104)
    ncdf4::ncvar_put(emiss_file,
                     "XLAT",
                     input_lat)
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLAT",
                     attname = "MemoryOrder",
                     attval = "XY")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLAT",
                     attname = "description",
                     attval = "LATITUDE, SOUTH IS NEGATIVE")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLAT",
                     attname = "units",
                     attval = "degree north")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLAT",
                     attname = "stagger",
                     attval = "")
    ncdf4::ncatt_put(emiss_file,
                     varid = "XLAT",
                     attname = "FieldType",
                     attval = 104)
    # vetor of zeros
    zero <- array(0, c(west_east$len,
                       south_north$len,
                       emissions_zdim_stag$len,
                       Time$len))
    # GASES initialization with zeros
    for(i in 1:(length(variaveis) - n_aero)){
      ncdf4::ncvar_put(emiss_file,
                       varid = variaveis[i],
                       zero)
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "MemoryOrder",
                       attval = "XYZ")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "description",
                       attval = "EMISSIONS")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "units",
                       attval = "mol km^-2 hr^-1")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "stagger",
                       attval = "Z")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "FieldType",
                       attval = 104)
    }
    # AEROSOIS initialization with zeros
    for(i in (1 + length(variaveis) - n_aero):length(variaveis)){
      ncdf4::ncvar_put(emiss_file,
                       varid = variaveis[i],
                       zero)
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "MemoryOrder",
                       attval = "XYZ")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "description",
                       attval = "EMISSIONS")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "units",
                       attval = "ug m^-2 s^-1")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "stagger",
                       attval = "Z")
      ncdf4::ncatt_put(emiss_file,
                       varid = variaveis[i],
                       attname = "FieldType",
                       attval = 104)
    }
    if(verbose){
      print(emiss_file)
    }
    ncdf4::nc_close(emiss_file)
  }
}

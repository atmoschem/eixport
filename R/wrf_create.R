#' Create emission files for the WRF-Chem model
#'
#' @description Create WRF-chem emission files using information from the WRF initial condictions (wrfinput) file(s). The wrfinput file of the corresponding domain is read from the current folder or from the wrfinput_dir.
#'
#' There are two emission styles available: the 12 hour pair of emissions (that will be recycled by the model) using io_style_emissions = 1 and the date_hour format using io_style_emissions = 2 (default), see notes for more detail.
#'
#' The initial time is the original (wrfinput file) adjusted by the day_offset argument, this argument can be useful for split the emissions into several files or for a restarted simulation. The emissions are recorded at the interval of 60 minutes (or the auxinput5_interval_m argument) for 1 time (or frames_per_auxinput5 argument times).
#'
#' The variables created on output file is based on emis_opt data or a character vector contains the species, any change in variables need to be followed by a change in the n_aero for the correspondent number of aerosol species in the emission file (the n_aero last variables).
#'
#' Title argument will be written on global attribute TITLE, from the version 4.0 the model checks if the TITLE version contains "V4.", this can be disabled setting 'force_use_old_data = .true.' on WRF namelist.input.
#'
#' @param wrfinput_dir input folder with the wrfinput file(s)
#' @param wrfchemi_dir output folder
#' @param domains domain / domains to be process
#' @param frames_per_auxinput5 value from wrf &time_control namelist.input,
#' number of times (frames) in a single emission file
#' @param auxinput5_interval_m value from wrf &time_control namelist.input,
#' interval in minutes between different times (frames) see Details
#' @param day_offset number of days (can be a fraction) see Details
#' @param io_style_emissions from wrf &chem namelist.input, 1 for 12z/00z style and 2 to date_hour style, see Details
#' @param kemit from wrf &chem namelist.input number of vertical levels of the emission file
#' @param variables emission species, can be used `emis_opt`
#' @param n_aero number of aerosol species
#' @param COMPRESS integer between 1 (least comp.) and 9 (most comp.) or NA for
#' no compression
#' @param force_ncdf4 force NetCDF4 format
#' @param title TITLE attribute for the NetCDF
#' @param separator filename alternative separator for hour:minutes:seconds with io_style_emission=2
#' @param prefix file name prefix, default is wrfchemi (wrf default)
#' @param overwrite logical, defoult is true, if FALSE check if the file exist
#' @param return_fn logical, return the name of the last file created
#' @param verbose print file info
#'
#' @note Using io_style_emissions = 1, the wrfchemi_00z will be generated with day_offset = 0 and
#' wrfchemi_12z with day_offset = 0.5 (frames_per_auxinput5 and auxinput5_interval_m will have no effect).
#'
#' @note Windows users may need to rename the emission files or change in namelist the defoult filename before run wrf.exe with these emission files.
#'
#' @note The separator argument can be useful for write in NTSF format discs on linux systems, for 'default' the separator is ':' for linux-like systems and '%3A' for windowns.
#'
#' @author Daniel Schuch
#'
#' @importFrom  ncdf4 nc_open nc_close ncvar_get ncvar_put ncdim_def ncvar_def ncatt_put
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

wrf_create  <- function(wrfinput_dir         = getwd(),
                        wrfchemi_dir         = wrfinput_dir,
                        domains              = 1,
                        frames_per_auxinput5 = 1,
                        auxinput5_interval_m = 60,
                        day_offset           = 0,
                        io_style_emissions   = 2,
                        kemit                = 1,
                        variables            = "ecb05_opt2",
                        n_aero               = 15,
                        COMPRESS             = NA,
                        force_ncdf4          = FALSE,
                        title                = "Anthropogenic emissions for WRF V4.0",
                        separator            = 'default',
                        prefix               = 'wrfchemi',
                        overwrite            = TRUE,
                        return_fn            = FALSE,
                        verbose              = FALSE)
{
  a <- Sys.info()["sysname"]
  # to avoid special chacacteres in the filename
  if(a[[1]] == "Windows") linux = FALSE else linux = TRUE           # nocov
  if(a[[1]] == "Windows")
    if(io_style_emissions == 2)                                     # nocov
      cat("NOTE: see wrf_create domumentation notes before run\n")  # nocov

  if(length(variables) == 1 & !is.na(variables)[1]){
    emis_opt <- sysdata$emis_opt
    if(variables %in% names(emis_opt)){
      variables <- emis_opt[[variables]]
    }else{
      cat(paste(variables,"is not valid, use one of:\n"))           # nocov
      cat(names(emis_opt))                                          # nocov
      stop("name, numeric value or a set of variable names")        # nocov
    }
  }
  if(is.na(variables)[1]){
    cat("writing the emission file without emission variables\n")   # nocov
  }else{
    if(n_aero > length(variables)){
      stop("n_aero cannot be greater than the number of variables ") # nocov
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
      if(day_offset != 0){                    # nocov
        day_offset <- 0.5                     # nocov
      }
    }

    date         <- date + 86400 * day_offset
    hora         <- format(date,"%H")
    minuto       <- format(date,"%M")
    segundo      <- format(date,"%S")
    hora         <- paste(formatC(hora,    width = 2, format = "d", flag = "0"))
    minuto       <- paste(formatC(minuto,  width = 2, format = "d", flag = "0"))
    segundo      <- paste(formatC(segundo, width = 2, format = "d", flag = "0"))

    if(separator == 'default'){
      separator <- "%3A"           # nocov
    }else{
      linux <- FALSE               # nocov
    }

    if(io_style_emissions == 1){
      frames_per_auxinput5 <- 12  # nocov start
      if(day_offset == 0){
        h <- "00z"
      }else{
        h <- "12z"
      }
      file_name <- paste(wrfchemi_dir, "/",prefix,"_", h, "_", "d0",domain, sep = "") # nocov end
    }
    if(io_style_emissions ==2){                                                     # nocov start
      if(linux){
        file_name <- paste(wrfchemi_dir, "/",prefix,"_d0", domain, "_",
                           format(date,"%Y-%m-%d"),
                                   "_",hora,":", minuto,":", segundo, sep = "")
      } else  file_name <- paste(wrfchemi_dir, "/",prefix,"_d0", domain, "_",
                                 format(date,"%Y-%m-%d"),
                                 "_", hora, separator, minuto, separator, segundo, sep = "")# nocov end
    }
    if(!overwrite & file.exists(file_name)){
      cat('using current file for domain',domain,'...\n') # nocov
      next                                                # nocov
    }else{
      cat('creating emission for domain',domain,'...\n')
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
    g_atributos  <- c( list(TITLE = title,
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

    if(!is.na(variables)[1]){
      # GAS phase emissions
      if(n_aero != length(variables)){
        for(i in 1:(length(variables) - n_aero)){
          assign(variables[i],
                 ncdf4::ncvar_def(name = variables[i],
                                  units = "",
                                  dim = list(west_east,
                                             south_north,
                                             emissions_zdim_stag,
                                             Time),
                                  prec="float",
                                  compression = COMPRESS))
        }
      }
      # AEROSOLS emissions
      if(n_aero > 0){
        for(i in (1+length(variables) - n_aero):length(variables)){
          assign(variables[i],
                 ncdf4::ncvar_def(name = variables[i],
                                  units = "",
                                  dim = list(west_east,
                                             south_north,
                                             emissions_zdim_stag,
                                             Time),prec="float",
                                  compression = COMPRESS))
        }
      }
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
    if(!is.na(variables)[1]){
      # GASES initialization with zeros
      if(n_aero != length(variables)){
        for(i in 1:(length(variables) - n_aero)){
          ncdf4::ncvar_put(emiss_file,
                           varid = variables[i],
                           zero)
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "MemoryOrder",
                           attval = "XYZ")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "description",
                           attval = "EMISSIONS")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "units",
                           attval = "mol km^-2 hr^-1")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "stagger",
                           attval = "Z")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "FieldType",
                           attval = 104)
        }
      }
      # AEROSOIS initialization with zeros
      if(n_aero > 0){
        for(i in (1 + length(variables) - n_aero):length(variables)){
          ncdf4::ncvar_put(emiss_file,
                           varid = variables[i],
                           zero)
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "MemoryOrder",
                           attval = "XYZ")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "description",
                           attval = "EMISSIONS")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "units",
                           attval = "ug m^-2 s^-1")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "stagger",
                           attval = "Z")
          ncdf4::ncatt_put(emiss_file,
                           varid = variables[i],
                           attname = "FieldType",
                           attval = 104)
        }
      }
    }

    if(verbose){
      print(emiss_file, "\n")
    }
    cat('output file:',emiss_file$filename,'\n')
    ncdf4::nc_close(emiss_file)
    ncdf4::nc_close(wrfinput)
  }
  if(return_fn){
    return(emiss_file$filename) # nocov
  }
}

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# how to use
# Rscript rwrf1.R wrfout_d01_RADM2_BASE 
# Edit and change variables

library(raster)
library(wrftools)
data(cetesb)
cetesb <- cetesb[!is.na(cetesb$Station), "Station"]

t2 = c("T2", "o3", "co", "no", "no2", "U10", "V10", "PSFC", "PM10", "PM2_5_DRY")

# time series ####

wrfi <- args[1]

newdir <- substr(wrfi, start = 12, stop = 100)

newdir <- gsub(":", "_", newdir)

newfir <- gsub(".nc", "",  newdir)

dir.create(newdir)

df <- xtractor(atmos = wrfi,
 i                vars = t2,
                 points = cetesb,
#                 stations = cetesb$Station,
                 return_list = FALSE)
df$dir <- newdir

saveRDS(df, paste0(newdir, "/ROUT.rds"), compress = "xz")

# NetCDF ####
df <- xtractor(atmos = wrfi,
               vars = t2,
               points = cetesb,
               return_list = TRUE)

 for (i in seq_along(t2)) {
        print(names(df$raster)[i])
        writeRaster(
          df$raster[[i]],
          paste0(newdir, "/", t2[i], ".nc"),
          overwrite = TRUE
        )
     print(
     paste0(newdir, t2[i], ".nc")
  )
   }



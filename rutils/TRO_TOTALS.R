library(eixport)
# v432_AP ####
pols <- c("BC", "CO",  "NH3", "NMVOC", "NOx", "OC", 
          "PM10", "PM2.5_fossil",
          "SO2")

si <- c("TOTALS", "TRO")

for(j in 1:length(pols)){
  for(i in 1:length(si)){
    print(paste(pols[j], si[i]))
    
    get_edgar(dataset = "v432_AP", 
              pol = pols[j],
              sector = si[i], 
              year = 2012, 
              destpath = "~/Documentos/edgar", txt = TRUE)
    
  }
}

# v432_VOC_spec ####
pols <- c(#"voc1", "voc2", "voc3", "voc3", "voc5", "voc6", 
          # "voc7", "voc8", "voc9", "voc10", 
          "voc11" #, "voc12", 
          # "voc13", "voc14", "voc15", "voc16", "voc17", "voc18",
          # "voc19", "voc20", "voc21", "voc22", "voc23", "voc24", "voc25"
          )

si <- c("TRO", "TOTALS")

for(j in 1:length(pols)){
  for(i in 1:length(si)){
    print(paste(pols[j], si[i]))
    
    get_edgar(dataset = "v432_VOC_spec", 
              pol = pols[j],
              sector = si[i], 
              year = 2012, 
              destpath = "~/Documentos/edgar", txt = TRUE)
    
  }
}

library(eixport)
# a <- read.csv("~/MEGA/PhDgrive/models/links_edgar.csv")
# head(a)

pols <- c("BC", "CO", "NH3", "NMVOC", "NOx", "OC",
          "PM10", "PM2.5_bio", "PM2.5_fossil", "SO2")
years <- 1970:2012
for(j in 1:length(years)){
  for(i in 1:length(pols)){
    urls <- get_edgar(dataset = "v432_AP",
                      pol = pols[i],
                      year = years[j],
                      copyright = F)
  }
}

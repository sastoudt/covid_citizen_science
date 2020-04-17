setwd("~/Desktop/covid_citizen_science")
require(readr)
inat <- read_tsv("0043558-200221144449610.csv") 
##  GBIF.org (16 April 2020) GBIF Occurrence Download https://doi.org/10.15468/dl.35fcyr


max(inat$eventDate, na.rm=T) ## "2020-04-10 17:15:52 UTC"

inat$eventDate=as.Date(inat$eventDate)
inat2 <- subset(inat, eventDate>=as.Date("2010-01-01") )
rm(inat)
inat3 <- subset(inat2, countryCode=="US")
rm(inat2)
inat4 = inat3[-which(is.na(inat3$stateProvince)),]
rm(inat3)

inat5 = inat4[,c(1:10,12:14,16,18,22,23,24,25,26,27,28,29,30:35, 41:42, 45)]
rm(inat4)

library(data.table)
fwrite(inat5, "iNatNice.csv")



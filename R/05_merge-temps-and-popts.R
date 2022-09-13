################################################################################################
## make array of overlapping population trend and temperature time series for each population ##
################################################################################################
library(rlist)
library(lubridate)
library(tidyverse)

## read in population time series data for acclitherm species
lpi_ol <- read.csv("data-processed/population-ts/lpi_acclitherm-spp.csv")
biotime_ol <- read.csv("data-processed/population-ts/biotime-with-absences_acclitherm-spp.csv")
gpdd_ol <- read.csv("data-processed/population-ts/gpdd_acclitherm-spp.csv")

## filter out populations we don't have temps for 
pops_key <- read.csv("data-processed/populations-with-temps.csv")

lpi_ol <- filter(lpi_ol, popts_id %in% pops_key$popts_id)
biotime_ol <- filter(biotime_ol, popts_id %in% pops_key$popts_id)
gpdd_ol <- filter(gpdd_ol, popts_id %in% pops_key$popts_id)
  
## save 
write.csv(lpi_ol, "data-processed/population-ts/lpi_acclitherm-spp-with-temps.csv", row.names = FALSE)
write.csv(biotime_ol, "data-processed/population-ts/biotime_acclitherm-spp-with-temps.csv", row.names = FALSE)
write.csv(gpdd_ol, "data-processed/population-ts/gpdd_acclitherm-spp-with-temps.csv", row.names = FALSE)






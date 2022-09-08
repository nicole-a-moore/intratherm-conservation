library(tidyverse)
library(ncdf4)
library(rerddap)


##########################################################################################
## getting temperature data for popultions with known population dynamics to predict CTmax
##########################################################################################
acclitherm <- read.csv("data-processed/acclitherm.csv", stringsAsFactors = FALSE) %>%
  select(genus_species, realm_general, class)

lpi_ol <- read.csv("data-processed/population-ts/lpi_acclitherm-spp.csv", stringsAsFactors = FALSE)
gpdd_ol <- read.csv("data-processed/population-ts/gpdd_acclitherm-spp.csv", stringsAsFactors = FALSE) 
biotime_ol <- read.csv("data-processed/population-ts/biotime-with-absences_acclitherm-spp.csv",  stringsAsFactors = FALSE) 

## get locations of populations
locs_lpi <- select(lpi_ol, popts_id, population_id, genus_species, Latitude, Longitude, System) %>% 
  rename("latitude" = Latitude, "longitude" = Longitude, "realm_of_population" = System) %>% unique()
locs_gpdd <- select(gpdd_ol, popts_id, population_id, genus_species, LatDD, LongDD, Ocean) %>% unique()
locs_gpdd$realm_of_population = ifelse(locs_gpdd$Ocean == "Not applicable",
                                   "Terrestrial/Freshwater", 
                                   "Marine") 
locs_gpdd <- select(locs_gpdd, -Ocean) %>%
  rename("latitude" = LatDD, "longitude" = LongDD)
locs_biotime <- select(biotime_ol, popts_id, population_id, genus_species, LATITUDE, LONGITUDE, REALM) %>%
  rename("latitude" = LATITUDE, "longitude" = LONGITUDE, "realm_of_population" = REALM) %>% unique()

## figure out realm of populations 
## check and make sure amphibians are always terrestrial 
locs_lpi <- left_join(locs_lpi, acclitherm) %>%
  filter(class == "Amphibia") %>%
  mutate(realm_of_population = "Terrestrial") %>%
  rbind(., filter(left_join(locs_lpi, acclitherm), class != "Amphibia"))

locs_gpdd <- left_join(locs_gpdd, acclitherm) %>%
  filter(class == "Amphibia") %>%
  mutate(realm_of_population = "Terrestrial") %>%
  rbind(., filter(left_join(locs_gpdd, acclitherm), class != "Amphibia"))

locs_biotime <- left_join(locs_biotime, acclitherm) %>%
  filter(class == "Amphibia") %>%
  mutate(realm_of_population = "Terrestrial") %>%
  rbind(., filter(left_join(locs_biotime, acclitherm), class != "Amphibia"))

## make sure fish are freshwater in gpdd 
locs_gpdd <- locs_gpdd %>%
  filter(realm_of_population == "Terrestrial/Freshwater") %>%
  mutate(realm_of_population = ifelse(class %in% c("Actinopteri", "Bivalvia"),
                            "Freshwater",
                            "Terrestrial")) %>%
  rbind(., filter(locs_gpdd, realm_of_population != "Terrestrial/Freshwater"))

## make sure realms match
all_locs <- rbind(locs_gpdd, locs_lpi, locs_biotime)

realms <- all_locs %>%
  select(genus_species, population_id, realm_of_population, realm_general)

no_match <- realms %>%
  filter(realm_general != realm_of_population) %>%
  select(genus_species, population_id) %>%
  unique() %>%
  left_join(., realms) %>% unique()
## 435 don't match - mean population was sampled from different realm than acclimation response ratio/thermal limit
## flag for now and decide what to do later 
all_locs$sampled_in_other_realm <- ifelse(all_locs$population_id %in% no_match$population_id,
                                          "Yes",
                                          "No")

## create temperature id = some pops will have the same temperature data so no use extracting twice
all_locs$temp_id <- paste(all_locs$latitude, all_locs$longitude, sep = "_")

#########################################
##      GETTING TERRESTRIAL TEMPS      ##
#########################################
temperature_data <- data.frame(matrix(nrow = 32873))
unique_pairs <- all_locs %>%
  filter(realm_of_population == "Terrestrial") %>%
  select(latitude, longitude, temp_id) %>%
  unique()

unique_loc <- 1
while (unique_loc < nrow(unique_pairs) + 1) {
  
  loc_cumulative <- data.frame(matrix(ncol=2))
  colnames(loc_cumulative) <- c("date", "temp_value")
  rep = 1930
  
  while (rep < 2020) {
    print(paste("On population number ", unique_loc, ", getting temp data from ", rep, sep = ""))
    
    ## read in gridded data in nc file for the file_index from berkeley earth and store data in R workspace 
    filename <- paste("data-raw/temperature-data/Complete_TAVG_Daily_LatLong1_", rep, ".nc", sep = "")
    ncfile <- nc_open(filename)
    
    ## create variables for things needed to use data
    date <- ncvar_get(ncfile, "date_number")
    arr.anom <-ncvar_get(ncfile, "temperature")
    arr.clim <- ncvar_get(ncfile, "climatology")
    
    if (rep == 1930 & unique_loc == 1) {
      lat <- ncfile$dim$latitude$vals
      lon <- ncfile$dim$longitude$vals
    }
    
    nc_close(ncfile)
    
    ## get clim and anom data for collection location of species 
    ## NaN here if location does not have data 
    loc_long_index <- which.min(abs(lon - unique_pairs$longitude[unique_loc]))
    loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[unique_loc]))
    loc.anom <- arr.anom[loc_long_index,loc_lat_index,]
    loc.clim.365d <- arr.clim[loc_long_index,loc_lat_index,]
    
    ## account for leap year - duplicate day index added on feb 28 in clim array (this seems to be how they dealt with it when calculating anom)
    index_59 <- loc.clim.365d[59]
    loc.clim.366d <- append(loc.clim.365d, index_59, after = 59)
    
    ## repeat day list loc.clim.365d on normal years + loc.clim.366d on leap years 
    last_year = (rep + 10)
    loc.clim <- c()
    
    while (rep < last_year) {
      if (rep == 2020) {
        loc.clim <- append(loc.clim, loc.clim.365d[1:151], after = length(loc.clim))
      }
      else if (rep %% 4 == 0){
        if (rep == 1900) { 
          loc.clim <- append(loc.clim, loc.clim.365d, after = length(loc.clim))
        }
        else {
          loc.clim <- append(loc.clim, loc.clim.366d, after = length(loc.clim))
        }
      }
      else {
        loc.clim <- append(loc.clim, loc.clim.365d, after = length(loc.clim))
      }
      rep = rep + 1
    }
    
    ## create dataframe of actual temp values at location by adding anomaly and climatology values over the 10 years 
    temp_list <- c()
    max <- rep
    rep <- rep - 10
    d <- 1
    
    while (rep < max) {
      if (rep == 2020) {
        temps <- loc.anom[d:(d+150)] + loc.clim[d:(d+150)]
        temp_list <- append(temp_list, temps, after = length(temp_list))
        d = d + 150
      }
      else if (rep %% 4 == 0) {
        if (rep == 1900) {
          temps <- loc.anom[d:(d+364)] + loc.clim[d:(d+364)]
          temp_list <- append(temp_list, temps, after = length(temp_list))
          d = d + 365
        }
        else { 
          temps <- loc.anom[d:(d+365)] + loc.clim[d:(d+365)]
          temp_list <- append(temp_list, temps, after = length(temp_list))
          d = d + 366
        }
      }
      else {
        temps <- loc.anom[d:(d+364)] + loc.clim[d:(d+364)]
        temp_list <- append(temp_list, temps, after = length(temp_list))
        d = d + 365
      }
      rep = rep + 1
    }
    
    ## make dataframe of date and corresponding temp values
    loc <- data.frame(date[], temp_list[])
    colnames(loc) <- c("date", "temp_value")
    
    ## add loc to loc_cumulative so one data frame contains data from all 10 year datasets
    loc_cumulative <- rbind(loc_cumulative, loc)
  }
  
  pop_id <- unique_pairs$temp_id[unique_loc]
  
  ## add column for population to temperature data 
  if (unique_loc == 1){
    temperature_data <- cbind(temperature_data, loc_cumulative)
    temperature_data <- temperature_data[,-1]
    colnames(temperature_data)[unique_loc+1] <- pop_id
  }
  else {
    temperature_data <- cbind(temperature_data, loc_cumulative[,2])
    colnames(temperature_data)[unique_loc+1] <- pop_id
  }
  
  unique_loc = unique_loc + 1;
  
}


terrestrial_temps <- temperature_data[-1,]
#saveRDS(terrestrial_temps, "data-processed/intermediate-data/terrestrial_tavg.rds")
terrestrial_temps <- readRDS("data-processed/intermediate-data/terrestrial_tavg.rds")


####################################
##      GETTING MARINE TEMPS      ##
####################################
unique_pairs <- all_locs %>%
  filter(realm_of_population == "Marine") %>%
  select(latitude, longitude, temp_id) %>%
  unique()

info <- info("ncdcOisst21Agg_LonPM180")


## make latitude and longitude vectors based on NOAA format
## longitude: Uniform grid with centers from -179.875 to 179.875 by 0.25 degrees.
lon <- rep(-179.875, times = 1439)
n = 2
while (n < 1441) {
  lon[n] <- lon[n -1] + 0.25
  n = n+1
}
## latitude: Uniform grid with centers from -89.875 to 89.875 by 0.25 degrees.
lat <- rep(-89.875, times = 719)
n = 2
while (n < 721) {
  lat[n] <- lat[n -1] + 0.25
  n = n+1
}


grid_lat <- c()
grid_lon <- c()

## find closest lat lon grid cell to each population collection location 
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
  loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
  loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
  
  grid_lon <- append(grid_lon, lon[loc_lon_index])
  grid_lat <- append(grid_lat, lat[loc_lat_index])
  
  num_unique = num_unique + 1
}

unique_pairs <- unique_pairs %>%
  mutate(grid_lon = grid_lon) %>%
  mutate(grid_lat = grid_lat) 


## create dataframe for temp data
## nValues for time attribute = 14096
temperature_data <- data.frame(matrix(nrow = 14096))
colnames(temperature_data) = c("date")

## loop through each population getting temp data for its grid cell and adding to temp data
unique_loc <- 1
while (unique_loc < nrow(unique_pairs) + 1) {
  print(paste("On population number", unique_loc))
  time_series <- griddap(info,
                         time = c("1981-09-01", "2020-04-04"), 
                         latitude = c(unique_pairs$grid_lat[unique_loc],unique_pairs$grid_lat[unique_loc]),
                         longitude = c(unique_pairs$grid_lon[unique_loc], unique_pairs$grid_lon[unique_loc]),
                         url = "https://upwell.pfeg.noaa.gov/erddap/")
  temps <- time_series$data$sst
  print(paste("Successfully got time series of length", length(temps)))
  
  if (unique_loc == 1) {
    times <- time_series$data$time
    temperature_data$date <- times
  }
  
  temperature_data$temp <- temps
  pop_id <- unique_pairs$temp_id[unique_loc]
  colnames(temperature_data)[unique_loc+1]<- pop_id
  
  print("Stored data in temperature_data and moving on to next population!")
  
  unique_loc <- unique_loc + 1
}

## save
marine_temps <- temperature_data
marine_temps <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_marine_temps_popdynam.rds")




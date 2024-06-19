library(tidyverse)
library(ncdf4)
library(rerddap)


##########################################################################################
## getting temperature data for populations with known population dynamics to predict CTmax
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

#########################################################
##      GETTING ELEVATION FOR POPDYNAM POPULATIONS     ##
#########################################################
latitude_of_raster <- c() 
longitude_of_raster <- c()
## "raster_of_latitude" and "raster_of_longitude" represent the centre coordinates of 1 degree lat x 1 degree long grid cells 

unique_locs <- all_locs %>%
  filter(realm_of_population == "Terrestrial") %>%
  select(latitude, longitude, temp_id) %>%
  unique()

filename <- paste("data-raw/temperature-data/Complete_TAVG_Daily_LatLong1_1930.nc", sep = "")
ncfile <- nc_open(filename)

lat <- ncvar_get(ncfile, "latitude")
long <- ncvar_get(ncfile, "longitude")

nc_close(ncfile)

## get grid square coordinates for each population
num_unique <- 1
while (num_unique < nrow(unique_locs)+1) {
  loc_long_index <- which.min(abs(long - unique_locs$longitude[num_unique]))
  loc_lat_index <- which.min(abs(lat - unique_locs$latitude[num_unique]))
  
  latitude_of_raster <- append(latitude_of_raster, lat[loc_lat_index])
  longitude_of_raster <- append(longitude_of_raster, long[loc_long_index])
  
  num_unique <- num_unique + 1
}

unique_locs$latitude_of_raster <- latitude_of_raster
unique_locs$longitude_of_raster <- longitude_of_raster

########################################
##      GETTING RASTER ELEVATION      ##
########################################
## first, get the average elevation across each raster we took Berekely Earth temperature data from 
library(elevatr)
library(raster)
library(rgbif)
library(sp)
library(conflicted)
library(rlist)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "raster")

latitude <- unique_locs$latitude_of_raster
longitude <- unique_locs$longitude_of_raster

## 1. create a SpatialPolygonsDataFrame of 1 degree lat x 1 degree long rectangles representing each grid cell we need elevation for

## set projection to lat long WGS84
myProj <- "+proj=longlat +datum=WGS84 +ellps=WGS84"

raster_means <- c()

## for each grid cell we took temp data from:
i <- 1
while (i < nrow(unique_locs)+1) {
  
  ## 1. create a rectangle representing grid cell:
  ## draw square with coords corresponding to corners 
  ybottom = unique_locs$latitude_of_raster[i] - 0.5
  xleft = unique_locs$longitude_of_raster[i] - 0.5
  ytop = unique_locs$latitude_of_raster[i] + 0.5
  xright = unique_locs$longitude_of_raster[i] + 0.5
  
  print("Drawing grid cell...")
  rectangle <- Polygon(cbind(c(xleft,xleft,xright,xright,xleft),c(ybottom, ytop, ytop, ybottom, ybottom)))
  
  poly <- Polygons(list(rectangle), ID = "A")
  
  ## create spatial object of polygon 
  spPolygon = SpatialPolygons(list(poly))
  ## assign projection
  proj4string(spPolygon) <- myProj
  
  ## create dataframe: 
  df = matrix(data = c(0))
  rownames(df) = "A"
  spp = SpatialPolygonsDataFrame(spPolygon, data = as.data.frame(df))
  
  
  ## 2. get GDEM raster object with data for every cell:
  lat_og <- unique_locs$latitude_of_raster[i]
  long_og <- unique_locs$longitude_of_raster[i]
  
  y <- c(lat_og) ## latitudes of points to get GDEM data for 
  x <- c(long_og) ## longitudes of points to get GDEM data for 
  
  ## add 9 points in and around edge of grid cell to query to ensure whole grid cell GDEM data is downloaded
  n = 1
  while (n < 3) {
    if(n < 2) {
      latitude <- lat_og + 0.5
      longitude <- long_og + 0.5
      
      y <- append(y, latitude)
      x <- append(x, longitude)
    }
    if (n >= 2) {
      latitude <- lat_og - 0.5
      longitude <- long_og - 0.5
      
      y <- append(y, latitude)
      x <- append(x, longitude)
    }
    n = n + 1
  }
  loc_df <- data.frame(crossing(x, y)) ## data frame with the 9 query coordinates 
  
  
  ## get GDEM data for loc_df
  print(paste("Getting GDEM data for grid cell ", i, "...", sep = ""))
  cell_raster <- get_elev_raster(locations = loc_df, prj = myProj, z=10)
  ##plot(cell_raster)
  
  ## check that dimensions overlap and projections are the same:
  ##show(cell_raster)
  ##show(spp)
  ##plot(cell_raster)
  ##plot(spp, add=TRUE)
  
  
  ## 3. calculate average elevation in each grid cell and record: 
  print(paste("Done! Extracting pixels under grid cell ", i, "...", sep = ""))
  under_cell <- data.frame(extract(cell_raster, spp)) ## extract elevation of all GDEM pixels under the grid cell rectangle
  colnames(under_cell) <- c("pixel_elevation")
  print("Done! Calculating mean... ")
  under_cell <- subset(under_cell, subset = under_cell$pixel_elevation >= 0) ## remove all negative elevations corresponding to ocean 
  raster_mean <- lapply(under_cell, FUN=mean) ## calculate the mean of the pixels to get mean elevation of the grid cell
  raster_means <- append(raster_means, raster_mean)
  
  
  ## 4. move on to the next grid cell 
  print(paste("Finished grid cell number ", i, ", moving on to next cell...", sep = ""))
  i <- i + 1
}

unique_locs$raster_mean = unlist(raster_means, use.names=FALSE)
#saveRDS(unique_locs, "data-processed/elevation_popdynam.rds")
unique_pairs <- readRDS("data-processed/elevation_popdynam.rds")


########################################
##       GETTING POINT ELEVATION      ##
########################################
elev_input <- unique_pairs %>%
  rename(decimalLatitude = latitude, decimalLongitude = longitude) 

point_elev <- elevation(input = elev_input, elevation_model = "astergdem", username = "nicole_a_moore")

point_elev <- point_elev %>%
  rename("elevation_of_collection" = elevation_geonames) 

unique_pairs <- left_join(unique_pairs, point_elev)
all_locs <- left_join(all_locs, unique_pairs) 

all_locs <- select(all_locs, -latitude_of_raster, -longitude_of_raster)



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
#saveRDS(terrestrial_temps, "data-processed/temperature-data/terrestrial_tavg.rds")
terrestrial_temps <- readRDS("data-processed/temperature-data/terrestrial_tavg.rds")

## adjust for elevation
## check NA:
isNA <- terrestrial_temps %>%
  select(as.vector(which(colSums(is.na(terrestrial_temps)) == nrow(terrestrial_temps))))
colnames(isNA)
## 19.5_-154.93333 - all are on Hawaii or bermuda

## add elevational correction
i = 2
while (i < length(colnames(terrestrial_temps)) + 1) {
  temp_id <- colnames(terrestrial_temps)[i]
  pop <- unique_pairs[which(unique_pairs$temp_id == temp_id),]
  terrestrial_temps[i] <- terrestrial_temps[i] + 5.5*(pop$raster_mean - pop$elevation_of_collection)/1000
  i = i+1
}

write.csv(terrestrial_temps, "data-processed/temperature-data/terrestrial_tavg_elevation-corrected.csv", row.names = FALSE)



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

## loop through each population getting temp data for its grid cell and adding to temp data
unique_loc <- 1
while (unique_loc < nrow(unique_pairs) + 1) {
  print(paste("On population number", unique_loc))
  time_series <- griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                         url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                         time =  c("1981-09-01T12:00:00Z", "2020-04-04"), 
                         zlev = c(0, 0),
                         latitude = c(unique_pairs$grid_lat[unique_loc],unique_pairs$grid_lat[unique_loc]),
                         longitude = c(unique_pairs$grid_lon[unique_loc], unique_pairs$grid_lon[unique_loc]))
  temps <- time_series$data$sst
  print(paste("Successfully got time series of length", length(temps)))
  
  if (unique_loc == 1) {
    times <- time_series$data$time
    temperature_data[,1] <- times
    colnames(temperature_data) <- c("date")
  }
  
  temperature_data$temp <- temps
  pop_id <- unique_pairs$temp_id[unique_loc]
  colnames(temperature_data)[unique_loc+1]<- pop_id
  
  print("Stored data in temperature_data and moving on to next population!")
  
  unique_loc <- unique_loc + 1
}

## save
marine_temps <- temperature_data
#saveRDS(marine_temps, "data-processed/temperature-data/marine_tavg.rds")
marine_temps <- readRDS("data-processed/temperature-data/marine_tavg.rds")
colnames(marine_temps)[1] = "date"

## get rid of missing temps 
marine_missing <- unique_pairs[which(unique_pairs$temp_id %in% colnames(marine_temps)[which(is.na(marine_temps[1,]))]),] %>%
  select(-grid_lat,-grid_lon)

## could not find 64 locations
marine_temps <- marine_temps[,-which(colnames(marine_temps) %in% marine_missing$temp_id)]

write.csv(marine_temps, "data-processed/temperature-data/marine_tavg.csv", row.names = FALSE)


########################################
##      GETTING FRESHWATER TEMPS      ##
########################################
unique_pairs <- all_locs %>%
  filter(realm_of_population == "Freshwater")%>%
  #rbind(., marine_missing) %>%
  select(latitude, longitude, temp_id) %>%
  unique()

## open nc file and get lat lon and time vectors
filename <- paste("data-raw/temperature-data/watertemperature_wfd_historical_1958-2001.nc", sep = "")
ncfile <- nc_open(filename)

lon <- ncvar_get(ncfile, "longitude") ## units: degrees - intervals of 0.5 (30')
lat <- ncvar_get(ncfile, "latitude") ## units: degrees - intervals of 0.5 (30')
time <- ncvar_get(ncfile, "time") ## units: hours since 1901-01-01 (first time is 1958-01-01) 

## close the file
nc_close(ncfile)

## temps:
temperature_data <- data.frame(matrix(nrow = length(time)))
colnames(temperature_data) = c("date")
temperature_data$date <- time

## for each population:
unique_loc <- 1
while (unique_loc < nrow(unique_pairs) + 1) {
  print(paste("On: ", unique_loc, sep = ""))
  
  ## find closest lat lon coordinates to population collection location
  loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[unique_loc]))
  loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[unique_loc]))
  
  ## get waterTemp time series for closest lat lon coordinates 
  ncfile <- nc_open(filename)
  waterTemp <- ncvar_get(ncfile, "waterTemperature", start = c(loc_lon_index, loc_lat_index, 1), 
                         count = c(1, 1, -1))
  nc_close(ncfile)
  
  ## add to column in temperature_data and rename after column's population_id with longitude added onto the end
  temperature_data$temp <- waterTemp
  colnames(temperature_data)[unique_loc+1] <- unique_pairs$temp_id[unique_loc]
  
  unique_loc <- unique_loc + 1
}

freshwater_temps <- temperature_data
#saveRDS(freshwater_temps, "data-processed/temperature-data/freshwater_tavg.rds")
freshwater_temps <- readRDS("data-processed/temperature-data/freshwater_tavg.rds")

# ## change realm_of_population for missing marine temps to freshwater:
# ol_locs_all <- ol_locs_all %>%
#   mutate(realm_of_population = ifelse(temp_id %in% colnames(freshwater_temps), "Freshwater", realm_of_population))
# 
# population_overlap <- population_overlap %>%
#   mutate(realm_of_population = ifelse(temp_id %in% colnames(freshwater_temps), "Freshwater", realm_of_population))
# 

## convert from degrees K to degrees C
converted <- freshwater_temps
converted[, 2:508] <- converted[, 2:508] - 273.15

## convert date 
## starts at 1958-01-01
year <- c(round(0.5/365, digits = 3))
leap_year <- c(round(0.5/366, digits = 3))

i = 1
while (i < 366) {
  if (i < 365) {
    year = append(year, round((i+0.5)/365, digits = 3))
  }
  leap_year = append(leap_year, round((i+0.5)/366, digits = 3))
  i = i+1
}

rep = 1958
last_year = 2002
date <- c()

while (rep < last_year) {
  if (rep %% 4 == 0){
    if (rep == 1900) { 
      date <- append(date, rep+year, after = length(date))
    }
    else {
      date <- append(date, rep+leap_year, after = length(date))
    }
  }
  else {
    date <- append(date, rep+year, after = length(date))
  }
  rep = rep + 1
}

## replace column for date
converted$date <- as.vector(date)
freshwater_temps <- converted

##  check na:
na_locs <- colnames(freshwater_temps)[which(is.na(freshwater_temps[1,]))]
#na_locs	<- na_locs[!na_locs %in% marine_missing$temp_id]

# marine:
## 65_-53 David strait
## 49.66666667_-64.75, 49.66667_-64.75 - gulf of St Lawence
## 52.5_-130, 53_-131, 52.5_-132.5, 52.5_-132.5,  - coast of BC
## 52.5_5.25 netherlands coast
## 52.782831_5.327911 - on coast of netherlands
## 65.5_-66 - Arctic Ocean
## 63.75_-166.88333 - Bering Sea
## -33.545_27.05139 - off coast of South Africa
##  52.5_5.25, 52.782831_5.327911 - on coast of netherlands
## -50.6_127.4 - Indian Ocean 
## 53_144 Asia
## 59.25_-152.8333333, 59.48333333_-152.75, 58.96666667_-151.3833333, 57.56666667_-154.5333333 inlet in Alaska
## 35_124 - yellow sea 
## 58_-159 bristol bay
# -50.6_127.4 indian ocean
# 36_-122, 50.326447_-128.062496 north pacific
# 65.5_-66 artic ocean
# 63.75_-166.88333 bering sea 
# -33.545_27.05139 coast of SA
## -40.492606_172.715575 New Zealand

# probably not marine:
## 47.21666667_143.0166667
# 47.21667_143.01667

## remove two non-freshwater spp 
na_locs <- na_locs[-which(na_locs %in% c("47.21666667_143.0166667", "47.21667_143.01667"))]

## remove all na from freshwater temps, label realm_of_population as marine
freshwater_temps <- freshwater_temps[,-which(is.na(freshwater_temps[1,]))]
write.csv(freshwater_temps, "data-processed/temperature-data/freshwater_tavg.csv", row.names = FALSE)

all_locs$realm_of_population[all_locs$temp_id %in% na_locs] <- "Marine"


## get marine temp data for these species:
unique_pairs <- all_locs %>%
  filter(realm_of_population == "Marine" & temp_id %in% na_locs) 

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
  mutate(grid_lat = grid_lat) %>%
  select(grid_lat, grid_lon, temp_id) %>%
  unique() 

temperature_data <- data.frame(matrix(nrow = 14096))

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
    temperature_data[,1] <- times
    colnames(temperature_data) <- c("date")
  }
  
  temperature_data$temp <- temps
  pop_id <- unique_pairs$temp_id[unique_loc]
  colnames(temperature_data)[unique_loc+1]<- pop_id
  
  print("Stored data in temperature_data and moving on to next population!")
  
  unique_loc <- unique_loc + 1
}

new_marine_spp <- temperature_data
# saveRDS(new_marine_spp, "data-processed/temperature-data/marine_tavg_newspp.rds")
new_marine_spp <- readRDS("data-processed/temperature-data/marine_tavg_newspp.rds") %>%
  select(-date)

## get rid of missing ones
new_marine_spp <- new_marine_spp[,-which(is.na(new_marine_spp[1,]))]

## combine with other marine data
marine_temps <- read_csv("data-processed/temperature-data/marine_tavg.csv") %>%
  cbind(., new_marine_spp) 

write.csv(marine_temps, "data-processed/temperature-data/marine_tavg_all.csv", row.names = FALSE)

## write out file of all unique populations that we could find temperature data for 
locs_found <- append(colnames(marine_temps)[-1], colnames(freshwater_temps)[-1]) %>%
  append(., colnames(terrestrial_temps[-1]))

with_temps <- filter(all_locs, temp_id %in% locs_found) 
length(unique(with_temps$population_id)) # 1557
length(unique(all_locs$population_id)) # 2052

with_temps <- with_temps %>%
  unique(.)

write.csv(with_temps, "data-processed/populations-with-temps.csv", row.names = FALSE)

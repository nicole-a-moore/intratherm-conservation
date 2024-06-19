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

lpi_ol <- filter(lpi_ol, population_id %in% pops_key$population_id)
biotime_ol <- filter(biotime_ol, population_id %in% pops_key$population_id)
gpdd_ol <- filter(gpdd_ol, population_id %in% pops_key$population_id)
  
## save 
write.csv(lpi_ol, "data-processed/population-ts/lpi_acclitherm-spp-with-temps.csv", row.names = FALSE)
write.csv(biotime_ol, "data-processed/population-ts/biotime_acclitherm-spp-with-temps.csv", row.names = FALSE)
write.csv(gpdd_ol, "data-processed/population-ts/gpdd_acclitherm-spp-with-temps.csv", row.names = FALSE)

## start with LPI
lpi_ol <- arrange(lpi_ol, Binomial, ID, Location, year) %>% ## each goes from 1950-2018 and has NA for missing years 
  mutate(temp_id = paste(Latitude, Longitude, sep = "_")) %>%
  left_join(., pops_key)

## NOTE: units vary greatly
unique(lpi_ol$Units)

## start with lpi:
pops_by_realm <- lpi_ol %>%
  select(genus_species, population_id, temp_id, year, abundance, realm_of_population, Units, popts_id,
         sampled_in_other_realm) %>%
  distinct() %>%
  split(., lpi_ol$realm_of_population) 

pops_new <- list()
i = 1 
while (i <= length(pops_by_realm)) {
  
  pops <- pops_by_realm[[i]] %>%
    split(., .$population_id) 
  
  if(i == 1) {
    temps <- read_csv("data-processed/temperature-data/freshwater_tavg.csv") %>%
      separate(date, sep = 4, into = c("year"), remove = FALSE)  %>%
      mutate(year = as.integer(year))
    
    first <- 1958
    last <- 2001
  }
  else if (i == 2) {
    temps <- read_csv("data-processed/temperature-data/marine_tavg_all.csv")%>%
      separate(date, sep = " ", into = c("nondec_date"), remove = FALSE) %>%
      separate(date, sep = "-", into = c("year"), remove = FALSE) %>%
      mutate(nondec_date = as_date(nondec_date)) %>%
      mutate(date = decimal_date(nondec_date)) %>%
      select(-nondec_date) %>%
      mutate(year = as.integer(year)) %>%
      mutate(date = as.numeric(as.character(date)))
    
    first <- 1981
    last <- 2020
  }
  else if (i == 3) {
    temps <- read_csv("data-processed/temperature-data/terrestrial_tavg_elevation-corrected.csv") %>%
      separate(date, sep = 4, into = c("year"), remove = FALSE) %>%
      mutate(year = as.integer(year))
    
    first <- 1950
    last <- 2019
  }
  
  z = 1 + length(pops_new)
  x = 1
  while (x <= length(pops)) {
    ## format population data
    pop <- as.data.frame(pops[x]) 
    colnames(pop) <- c("genus_species", "population_id", "temp_id", "year", "abundance", "realm_of_population", "units",
                       "popts_id", "sampled_in_other_realm")
    
    ## merge with dates
    ## if sample date is only a year, assign to last day of that year 
    pop <- left_join(temps[,1:2], pop, by = c("year"))%>%
      group_by(year) %>%
      arrange(-date) %>%
      mutate(abundance = replace(abundance, duplicated(abundance), NA)) %>%
      arrange(date) %>%
      ungroup() %>%
      fill(c(genus_species, population_id, temp_id, realm_of_population, units, popts_id, sampled_in_other_realm), 
           .direction = "updown")
    
    ## add temps
    loc_temps <- temps[1:nrow(temps),
                          which(colnames(temps) == unique(pop$temp_id))]
    colnames(loc_temps)[1] = "temperature"
    loc_temps$date = temps$date
    
    pop <- left_join(pop, loc_temps)
    
    ## check if no abundance data within period we have temperature data for:
    if(length(which(is.na(pop$abundance))) == nrow(pop)) {
      x = x+1
    }
    ## check if no temperature data for that species:
    else if(is.null(pop$temperature)) {
      x = x+1
    }
    
    else {
      pops_new <- list.append(pops_new, pop)
      names(pops_new)[z] <- pop$population_id[1]
      
      z = z+1
      x = x+1	
    }
  }
  
  i = i+1
}

## now add populations from gpdd
gpdd_ol <- gpdd_ol %>%
  mutate(temp_id = paste(LatDD, LongDD, sep = "_")) %>%
  arrange(genus_species, MainID, LocationID, SampleYear) %>%
  left_join(., pops_key)

unique(gpdd_ol$SamplingUnits)

## add sampling date column for each point based on Decimal Year End/Begin and Sampling Frequency
unique(gpdd_ol$SamplingFrequency)

pops_by_realm <- gpdd_ol %>%
  select(genus_species, population_id, temp_id, SampleYear, DecimalYearEnd, Population, realm_of_population, SamplingUnits,
         popts_id, sampled_in_other_realm) %>%
  distinct() %>%
  split(., gpdd_ol$realm_of_population) 

i = 1 
while(i < length(pops_by_realm)+1) {
  pops <- pops_by_realm[[i]] %>%
    split(., .$population_id) 
  
  if(i == 1) {
    temps <- read_csv("data-processed/temperature-data/freshwater_tavg.csv") %>%
      separate(date, sep = 4, into = c("year"), remove = FALSE)  %>%
      mutate(year = as.integer(year))
    
    first <- 1958
    last <- 2001
  }
  else if (i == 2) {
    temps <- read_csv("data-processed/temperature-data/marine_tavg_all.csv")%>%
      separate(date, sep = " ", into = c("nondec_date"), remove = FALSE) %>%
      separate(date, sep = "-", into = c("year"), remove = FALSE) %>%
      mutate(nondec_date = as_date(nondec_date)) %>%
      mutate(date = decimal_date(nondec_date)) %>%
      select(-nondec_date) %>%
      mutate(year = as.integer(year)) %>%
      mutate(date = as.numeric(as.character(date)))
    
    first <- 1981
    last <- 2020
  }
  else if (i == 3) {
    temps <- read_csv("data-processed/temperature-data/terrestrial_tavg_elevation-corrected.csv") %>%
      separate(date, sep = 4, into = c("year"), remove = FALSE) %>%
      mutate(year = as.integer(year))
    
    first <- 1950
    last <- 2019
  }
  
  z = 1 + length(pops_new)
  x = 1
  while (x < length(pops) + 1) {
    pop <- as.data.frame(pops[x]) 
    colnames(pop) <- c("genus_species", "population_id", "temp_id", "year", "sampling_date", "abundance", 
                       "realm_of_population", "units", "popts_id", "sampled_in_other_realm")
    
    start <- min(pop$year)
    end <- max(pop$year)
    
    ## check if abundance time series ends before temp time series begins
    if (end < first) {
      ## move on to next population
      x = x+1
    }
    ## check if temp time series ends before abundance time series begins
    else if (last < start) {
      ## move on to next population
      x = x+1
    }
    
    else {
      
      ## check if more than one sample per year:
      if (length(unique(pop$year)) != nrow(pop)) {
        
        ## merge with temps 
        loc_temps <- temps[1:nrow(temps), which(colnames(temps) == unique(pop$temp_id))]
        colnames(loc_temps)[1] = "temperature"
        loc_temps$date <- temps$date
        
        loc_temps <- separate(loc_temps, date, sep = 4, into = c("year"), remove = FALSE) %>%
          mutate(year = as.integer(year))
        
        vec <- c()
        for (l in pop$sampling_date) {
          ## if temp data not available for sampling date, assign NA
          if(l < min(loc_temps$date) | l > max(loc_temps$date)) {
            vec = append(vec, NA)
          }
          
          else {
            vec = append(vec, loc_temps$date[which.min(abs(l - loc_temps$date))])
          }
        }
        pop$date <- vec
      
        pop <- left_join(loc_temps, pop, by = c("date", "year")) %>%
          group_by(year) %>%
          arrange(-date) %>%
          mutate(abundance = replace(abundance, duplicated(abundance), NA)) %>%
          arrange(date) %>%
          ungroup() %>%
          fill(c(genus_species, population_id, temp_id, realm_of_population, units, popts_id, sampled_in_other_realm), 
               .direction = "updown") %>%
          select(-sampling_date)
        
      }
      else {
        ## check if gaps between years of sampling:
        if(length(start:end) != length(unique(pop$year))) {
          years <- tibble(year = start:end)
          pop <- left_join(years, pop, by = "year")
        }
        
        loc_temps <- temps[1:nrow(temps), which(colnames(temps) == unique(pop$temp_id))]
        colnames(loc_temps)[1] = "temperature"
        loc_temps$date <- temps$date
        
        loc_temps <- separate(loc_temps, date, sep = 4, into = c("year"), remove = FALSE) %>%
          mutate(year = as.integer(year))
        
        pop <- left_join(loc_temps, pop, by = c("year")) %>%
          group_by(year) %>%
          arrange(-date) %>%
          mutate(abundance = replace(abundance, duplicated(abundance), NA)) %>%
          arrange(date) %>%
          ungroup() %>%
          fill(c(genus_species, population_id, temp_id, realm_of_population, units, popts_id, sampled_in_other_realm), 
               .direction = "updown") %>%
          select(-sampling_date)
      }
      
      pops_new <- list.append(pops_new, pop)
      names(pops_new)[z] <- pop$population_id[1]
      
      z = z+1
      x = x+1	
    }
  }
  
  i = i+1
}

saveRDS(pops_new, "data-processed/intermediate-data/gpdd_lpi_temp.rds")

## now biotime!
biotime_ol <- biotime_ol %>%
  mutate(temp_id = paste(LATITUDE, LONGITUDE, sep = "_")) %>%
  arrange(genus_species, STUDY_ID, decimal_date) %>%
  left_join(., pops_key) %>%
  mutate(year = str_split_fixed(decimal_date, "\\.", 2)[,1])

pops_by_realm <- biotime_ol %>%
  select(genus_species, population_id, temp_id, year, decimal_date, sum.allrawdata.ABUNDANCE, realm_of_population, ABUNDANCE_TYPE,
         popts_id, sampled_in_other_realm) %>%
  distinct() %>%
  split(., biotime_ol$realm_of_population) 

i = 1 
while (i <= length(pops_by_realm)) {
  
  pops <- pops_by_realm[[i]] %>%
    split(., .$population_id) 
  
  if(i == 1) {
    temps <- read_csv("data-processed/temperature-data/freshwater_tavg.csv") %>%
      separate(date, sep = 4, into = c("year"), remove = FALSE)  %>%
      mutate(year = as.integer(year))
    
    first <- 1958
    last <- 2001
  }
  else if (i == 2) {
    temps <- read_csv("data-processed/temperature-data/marine_tavg_all.csv")%>%
      separate(date, sep = " ", into = c("nondec_date"), remove = FALSE) %>%
      separate(date, sep = "-", into = c("year"), remove = FALSE) %>%
      mutate(nondec_date = as_date(nondec_date)) %>%
      mutate(date = decimal_date(nondec_date)) %>%
      select(-nondec_date) %>%
      mutate(year = as.integer(year)) %>%
      mutate(date = as.numeric(as.character(date)))
    
    first <- 1981
    last <- 2020
  }
  else if (i == 3) {
    temps <- read_csv("data-processed/temperature-data/terrestrial_tavg_elevation-corrected.csv") %>%
      separate(date, sep = 4, into = c("year"), remove = FALSE) %>%
      mutate(year = as.integer(year))
    
    first <- 1950
    last <- 2019
  }
  
  z = 1 + length(pops_new)
  x = 1
  while (x <= length(pops)) {
    ## format population data
    pop <- as.data.frame(pops[x]) 
    colnames(pop) <- c("genus_species", "population_id", "temp_id", "year", "sampling_date", "abundance", "realm_of_population", "units",
                       "popts_id", "sampled_in_other_realm")
    
    start <- min(pop$year)
    end <- max(pop$year)
    
    ## check if abundance time series ends before temp time series begins
    if (end < first) {
      ## move on to next population
      x = x+1
    }
    ## check if temp time series ends before abundance time series begins
    else if (last < start) {
      ## move on to next population
      x = x+1
    }
    
    else {
      ## merge with temps 
      loc_temps <- temps[1:nrow(temps), which(colnames(temps) == unique(pop$temp_id))]
      colnames(loc_temps)[1] = "temperature"
      loc_temps$date <- temps$date
      
      loc_temps <- separate(loc_temps, date, sep = 4, into = c("year"), remove = FALSE) %>%
        mutate(year = as.integer(year))
      
      vec <- c()
      for (l in pop$sampling_date) {
        ## if temp data not available for sampling date, assign NA
        if(l < min(loc_temps$date) | l > max(loc_temps$date)) {
          vec = append(vec, NA)
        }
        
        else {
          vec = append(vec, loc_temps$date[which.min(abs(l - loc_temps$date))])
        }
      }
      pop$date <- vec
      pop$year = as.numeric(as.character(pop$year))
      
      pop <- left_join(loc_temps, pop, by = c("date", "year")) %>%
        group_by(year) %>%
        arrange(-date) %>%
        mutate(abundance = replace(abundance, duplicated(abundance), NA)) %>%
        arrange(date) %>%
        ungroup() %>%
        fill(c(genus_species, population_id, temp_id, realm_of_population, units, popts_id, sampled_in_other_realm), 
             .direction = "updown") %>%
        select(-sampling_date)
      
      pops_new <- list.append(pops_new, pop)
      names(pops_new)[z] <- pop$population_id[1]
      
      z = z+1
      x = x+1	
    }
  }
  
  i = i+1
}

## save:
saveRDS(pops_new, "data-processed/population-time-series-with-temps.rds")

## get rid of populations whose time series didn't overlap the temperature time series:
pops_key <- filter(pops_key, population_id %in% names(pops_new))
write.csv(pops_key, "data-processed/populations-with-temps_after-matching.csv")

lpi_ol <- read.csv("data-processed/population-ts/lpi_acclitherm-spp-with-temps.csv")
biotime_ol <- read.csv("data-processed/population-ts/biotime_acclitherm-spp-with-temps.csv")
gpdd_ol <- read.csv("data-processed/population-ts/gpdd_acclitherm-spp-with-temps.csv")

lpi_ol <- filter(lpi_ol, population_id %in% pops_key$population_id)
biotime_ol <- filter(biotime_ol, population_id %in% pops_key$population_id)
gpdd_ol <- filter(gpdd_ol, population_id %in% pops_key$population_id)

## save 
write.csv(lpi_ol, "data-processed/population-ts/lpi_acclitherm-spp-with-temps_after-matching.csv", row.names = FALSE)
write.csv(biotime_ol, "data-processed/population-ts/biotime_acclitherm-spp-with-temps_after-matching.csv", row.names = FALSE)
write.csv(gpdd_ol, "data-processed/population-ts/gpdd_acclitherm-spp-with-temps_after-matching.csv", row.names = FALSE)

## filter version of acclitherm:
acclitherm <- read.csv("data-processed/acclitherm.csv")

acclitherm <- filter(acclitherm, genus_species %in% c(lpi_ol$genus_species, biotime_ol$genus_species,
                                                      gpdd_ol$genus_species))
length(unique(acclitherm$genus_species)) # 170
length(unique(names(pops_new))) # 1406

write.csv(acclitherm,"data-processed/acclitherm_after-matching.csv", row.names = FALSE)

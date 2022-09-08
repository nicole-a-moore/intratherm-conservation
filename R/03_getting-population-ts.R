## extract population time series for species in acclitherm 
library(tidyverse)
library(readr)

## GPDD: 41 species, 235 populations
## LPI: 176 species, 1290 populations
## BIOTIME: 169 species, 527 populations

acclitherm <- read_csv("data-processed/acclitherm.csv")

########################################################
## figure out which species in acclitherm are in GPDD ##
########################################################
path = "data-raw/gpdd/urn_uuid_bced9df4_1398_4768_a0f3_6d5007446ad4/data/"

taxa <- read_csv(paste(path, "df35b.236.1-DATA.csv", sep = ""))
main <- read_csv(paste(path, "df35b.234.1-DATA.csv", sep = ""))
gdata <- read_csv(paste(path, "df35b.233.1-DATA.csv", sep = ""))
location <- read_csv(paste(path, "df35b.239.1-DATA.csv", sep = ""))
names(main)

## search list of TaxonNames for list of synonyms for our species
syns_df <- read_csv("data-processed/acclitherm_synonyms-key.csv")
syns = append(syns_df$binomial, syns_df$syn_name) %>%
  append(., syns_df$acc_name) %>%
  unique()

names <- taxa$TaxonName
syns[which(syns %in% names)] # 41 overlapping species 

## subset to overlapping data 
overlap <- intersect(names, syns)

## create genus_species column with name that matches acclitherm
overlap_taxa <- taxa %>% 
  filter(TaxonName %in% syns) %>%
  mutate(genus_species = ifelse(TaxonName %in% syns_df$acc_name,
                                TaxonName,
                                NA)) %>%
  left_join(., syns_df, by = c("TaxonName" = "syn_name")) %>%
  mutate(genus_species = ifelse(is.na(genus_species),
                               acc_name,
                               genus_species)) %>%
  select(c(1:13))
  
overlap_main <- main %>% 
  filter(TaxonID %in% overlap_taxa$TaxonID)

overlap_gdata <- gdata %>% 
  filter(MainID %in% overlap_main$MainID)

## join them all together 
ol_gpdd <- left_join(overlap_gdata, overlap_main) %>% 
  left_join(., overlap_taxa, by  = "TaxonID") %>% 
  left_join(., location, by = "LocationID") %>%
  distinct()

## which species are in overlap but not acclitherm?
ol_gpdd$genus_species[which(!ol_gpdd$genus_species %in% acclitherm$genus_species)] ## should be empty

## how many population time series?
length(unique(paste(ol_gpdd$TaxonName, ol_gpdd$MainID, sep = "_"))) #235

## fix some locations
unique(ol_gpdd$LatDD)
unique(ol_gpdd$LongDD)

## 0_0 - from Klicava Reservoir, coordinates should be 50.08, 13.9
ol_gpdd$LatDD[which(ol_gpdd$LocationID == 10323)]  <- 50.08
ol_gpdd$LongDD[which(ol_gpdd$LocationID == 10323)]  <- 13.9

location$LatDD[which(location$LocationID == 10323)]  <- 50.08
location$LongDD[which(location$LocationID == 10323)]   <- 13.9

ol_gpdd %>% 
  mutate(unique_population = paste(TaxonName, MainID, sep = "_")) %>% 
  select(unique_population, everything()) %>% 
  ggplot(aes(x = SeriesStep, y = Population, group = unique_population, color = TaxonName)) + 
  geom_line() +
  facet_wrap( ~ unique_population, scales = "free") + 
  theme(legend.position = "none")

ggsave("figures/gpdd-time-series.pdf", width = 20, height = 12)

## add pop ts identifier (database_rowID)
ol_gpdd$popts_id <- paste("GPDD", 1:nrow(ol_gpdd), sep = "_")
ol_gpdd$population_id <- paste(ol_gpdd$genus_species, ol_gpdd$LatDD, ol_gpdd$LongDD, ol_gpdd$MainID, sep = "_")
write_csv(ol_gpdd, "data-processed/population-ts/gpdd_acclitherm-spp.csv")


########################################################
## figure out which species in acclitherm are in LPI  ##
########################################################
library(devtools)
#install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
library(rlpi)

lpi <- read_csv("data-raw/lpi/Public data set/LPR2020data_public.csv") %>% 
  mutate(genus_species = str_replace(Binomial, "_", " "))

## look for acclitherm species, checking for synonyms
names <- unique(lpi$genus_species)
syns[which(syns %in% names)] # 176 overlapping species 

## subset to overlapping data 
overlap <- intersect(names, syns)

## create genus_species column with name that matches acclitherm
overlap_lpi <- lpi %>% 
  filter(genus_species %in% syns) %>%
  mutate(genus_species_new = ifelse(genus_species %in% syns_df$acc_name,
                                genus_species,
                                NA))  %>%
  select(genus_species, everything()) %>%
  left_join(., syns_df, by = c("genus_species" = "syn_name")) %>%
  mutate(genus_species = ifelse(is.na(genus_species_new),
                                acc_name,
                                genus_species_new)) %>%
  select(-genus_species_new)

overlap_lpi <- overlap_lpi[,-c(100:112)]

## how many species and populations?
length(unique(overlap_lpi$genus_species)) # 176 species
length(unique(overlap_lpi$ID)) ## 1290 populations

## reformat data to match gpdd
ol_lpi <- overlap_lpi %>% 
  mutate(population_id = paste(genus_species, Latitude, Longitude, ID, sep = "_")) %>%
  gather(key = year, value = abundance, "1950":"2018") %>% 
  mutate(abundance = ifelse(abundance == "NULL", NA, abundance)) %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  mutate(year = as.numeric(year))

## add pop ts identifier (database_rowID)
ol_lpi$popts_id <- paste("LPI", 1:nrow(ol_lpi), sep = "_")
write_csv(ol_lpi, "data-processed/population-ts/lpi_acclitherm-spp.csv")

############################################################
## figure out which species in acclitherm are in BioTIME  ##
############################################################
biotime <- read.csv("data-raw/biotime/BioTIMEQuery_24_06_2021.csv", stringsAsFactors = FALSE)

colnames(biotime)

## search list of TaxonNames for list of synonyms for our species
names <- biotime$GENUS_SPECIES
syns[which(syns %in% names)] # 178 overlapping species 

## subset to overlapping data 
overlap <- intersect(names, syns)

## create genus_species column with name that matches acclitherm
ol_biotime <- biotime %>% 
  filter(GENUS_SPECIES %in% syns) %>%
  mutate(genus_species = ifelse(GENUS_SPECIES %in% syns_df$acc_name,
                                GENUS_SPECIES,
                                NA)) %>%
  left_join(., syns_df, by = c("GENUS_SPECIES" = "syn_name")) %>%
  mutate(genus_species = ifelse(is.na(genus_species),
                                acc_name,
                                genus_species)) %>%
  select(genus_species, everything())
ol_biotime <- ol_biotime[,-c(17:29)]

## which species are in overlap but not acclitherm?
ol_biotime$genus_species[which(!ol_biotime$genus_species %in% acclitherm$genus_species)] ## should be empty

## attach meta data 
meta <- read.csv("data-raw/biotime/BioTIMEMetadata_24_06_2021.csv", stringsAsFactors = FALSE)

ol_biotime <- left_join(ol_biotime, meta, by = "STUDY_ID") %>%
  filter(!str_detect(DATA_SOURCE, "Global Population Dynamics Database")) %>%
  arrange(STUDY_ID, SAMPLE_DESC, GENUS_SPECIES, YEAR) %>%
  select(STUDY_ID, SAMPLE_DESC, GENUS_SPECIES, YEAR, sum.allrawdata.ABUNDANCE, 
         sum.allrawdata.BIOMASS, everything()) %>%
  select(-X) %>%
  mutate(decimal_date = ifelse(is.na(MONTH), 
                               YEAR, 
                               ifelse(is.na(DAY), 
                                      (MONTH*30)/365 + YEAR, 
                                      (MONTH*30+DAY)/365 + YEAR))
  ) ## create decimal_date column for each sampling time

## make data frame of all sampled dates for each location so can add 0 abundance for days sampled but not found
sample_dates <- biotime %>%
  filter(STUDY_ID %in% ol_biotime$STUDY_ID) %>% ## remove studies that we don't have populations from
  group_by(STUDY_ID) %>%
  arrange(STUDY_ID, YEAR, MONTH, DAY) %>%
  select(YEAR, MONTH, DAY, STUDY_ID) %>%
  distinct() %>%
  mutate(decimal_date = (MONTH*30+DAY)/365 + YEAR) %>% ## create decimal_date column for each sampling time
  mutate(decimal_date = ifelse(is.na(MONTH) & is.na(DAY), YEAR, decimal_date)) %>%
  filter(!is.na(decimal_date)) %>% 
  select(decimal_date, STUDY_ID) %>%
  ungroup()

ol_biotime <- ol_biotime %>%
  select(-SAMPLE_DESC, -decimal_date) %>%
  arrange(STUDY_ID)

## marine studies: treating whole study as one sampling location
## look into how to deal with grain size later
studyIDs <- ol_biotime %>% 
  mutate(LATITUDE = CENT_LAT) %>% ## change lat and lon to central lat and lon of study
  mutate(LONGITUDE = CENT_LONG) %>%
  mutate(decimal_date = (MONTH*30+DAY)/365 + YEAR) %>% ## create decimal_date column for each sampling time
  mutate(decimal_date = ifelse(is.na(MONTH) & is.na(DAY), YEAR, decimal_date)) %>%
  arrange(STUDY_ID, GENUS_SPECIES, decimal_date, YEAR, MONTH, DAY) %>%
  unique() %>%
  split(., .$STUDY_ID, .$GENUS_SPECIES) 

new_ol <- c()
## merge so each study has each combination of sampling time x species
i = 1
while(i < length(studyIDs) + 1) {
  species <- split(studyIDs[[i]], studyIDs[[i]]$GENUS_SPECIES)
  dates <- sample_dates %>%
    filter(STUDY_ID == unique(species[[1]]$STUDY_ID))
  
  z = 1
  while(z < length(species) + 1) {
    current_species <- left_join(dates, species[[z]], by = c("decimal_date", "STUDY_ID")) %>%
      fill(-DAY, -YEAR, -MONTH, -sum.allrawdata.ABUNDANCE, -sum.allrawdata.BIOMASS, .direction = "updown")
    
    new_ol <- rbind(new_ol, current_species)
    
    z = z + 1
  }
  i = i + 1
}

new_ol <- new_ol %>%
  mutate(population_id = paste(GENUS_SPECIES, LATITUDE, LONGITUDE, STUDY_ID, sep = "_")) ## add pop id

## add pop ts identifier (database_rowID)
new_ol$popts_id <- paste("BioTIME", 1:nrow(new_ol), sep = "_")

## write out biotime without absense data
write_csv(new_ol, "data-processed/population-ts/biotime_acclitherm-spp.csv")

## add absences 
new_ol <- new_ol %>%
  mutate(sum.allrawdata.ABUNDANCE = 
           ifelse(is.na(sum.allrawdata.ABUNDANCE), 0, sum.allrawdata.ABUNDANCE)) %>% ## replace missing abundance with 0
  mutate(sum.allrawdata.BIOMASS = 
           ifelse(is.na(sum.allrawdata.BIOMASS), 0, sum.allrawdata.BIOMASS)) %>%
  group_by(decimal_date, GENUS_SPECIES, STUDY_ID) %>%
  add_count() %>%
  mutate(sum.allrawdata.ABUNDANCE = sum(sum.allrawdata.ABUNDANCE)/n) %>% ## get average abundance of samples taken in same time point + at same loc + same species 
  mutate(sum.allrawdata.BIOMASS = sum(sum.allrawdata.BIOMASS)/n) %>%
  ungroup() %>%
  .[!duplicated(.[,c("decimal_date","sum.allrawdata.ABUNDANCE",
                     "sum.allrawdata.BIOMASS","GENUS_SPECIES", "STUDY_ID")]),] %>% ## get rid of duplicates 
  arrange(STUDY_ID, GENUS_SPECIES, decimal_date, YEAR, MONTH, DAY) %>%
  select(-n)

ol_biotime_new <- new_ol %>%
  select(decimal_date, everything()) %>%
  group_by(STUDY_ID) %>%
  mutate(sample_type = case_when(
    length(which(sum.allrawdata.ABUNDANCE == 0)) == length(sum.allrawdata.ABUNDANCE) ~ "biomass",
    length(which(sum.allrawdata.BIOMASS == 0)) == length(sum.allrawdata.BIOMASS) ~ "abundance",
    TRUE ~ "both"
  )) %>% ## add column for type of abundance
  ungroup() %>%
  mutate(sum.allrawdata.ABUNDANCE = ifelse(sample_type == "biomass", 
                                           NA, sum.allrawdata.ABUNDANCE)) %>%
  mutate(sum.allrawdata.BIOMASS = ifelse(sample_type == "abundance", 
                                         NA, sum.allrawdata.BIOMASS)) %>%
  select(-YEAR, -DAY, -MONTH) 

## add pop ts identifier (database_rowID)
ol_biotime_new$popts_id <- paste("BioTIME", 1:nrow(ol_biotime_new), sep = "_")

## plot them 
ol_biotime_new %>% 
  select(population_id, everything()) %>% 
  ggplot(aes(x = decimal_date, y = sum.allrawdata.BIOMASS, group = population_id, color = GENUS_SPECIES)) + 
  geom_line() +
  theme(legend.position = "none")

ol_biotime_new %>% 
  filter(sample_type == "abundance") %>%
  ggplot(aes(x = decimal_date, y = sum.allrawdata.ABUNDANCE, group = population_id, color = GENUS_SPECIES)) + 
  geom_line() +
  theme(legend.position = "none")

## write out
write_csv(ol_biotime_new, "data-processed/population-ts/biotime-with-absences_acclitherm-spp.csv")
## extract population time series for species in acclitherm 
library(tidyverse)
library(readr)

## GPDD: 41 species, 235 populations
## LPI: 176 species, 1290 populations

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
  mutate(populaton_id = paste(genus_species, Latitude, Longitude, ID, sep = "_")) %>%
  gather(key = year, value = abundance, "1950":"2018") %>% 
  mutate(abundance = ifelse(abundance == "NULL", NA, abundance)) %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  mutate(year = as.numeric(year))

write_csv(ol_lpi, "data-processed/population-ts/lpi_acclitherm-spp.csv")









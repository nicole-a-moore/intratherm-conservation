## example taxize script to get the accepted names and synonyms of acclitherm species 
library(taxize)
library(plyr)
library(tidyverse)

acclitherm <- read.csv("data-processed/intermediate-data/acclitherm_Rohr-Comte-Intra.csv")

## use taxize to get a list of species synonyms and the most taxonomically correct names
## first, get list of unique species names in acclitherm:
acc_species <- unique(acclitherm$genus_species)

## search for the "tsn" of each species
## tsn stands for taxonomic serial numbers and each species has a unique one
tsn_search_tt <- get_tsn(as.character(acc_species), accepted = FALSE)

## save the search results:
saveRDS(tsn_search_tt, "data-processed/intermediate-data/tsn_search_acclitherm.rds")

tsns_tt <- data.frame(readRDS("data-processed/intermediate-data/tsn_search_acclitherm.rds"))

## add column for binomial
tsns_tt$binomial <- acc_species

found <- tsns_tt %>%
  subset(match == "found")  ## get only species with tsns that were found 

## get synonyms for found species by searching for them using their tsns
syns <- synonyms(db = tsns_tt)

## save search results 
saveRDS(syns, "data-processed/intermediate-data/syns_acclitherm.rds")
syns <- readRDS("data-processed/intermediate-data/syns_acclitherm.rds")

## turn the data into a dataframe, get rid of some columns 
syns_df <- ldply(syns, data.frame) %>%
  select(-c(1,9))
## in this dataframe, if the binomial + tsn you entered is the accepted name (acc_name), then the acc_name column = NA
## if the name you gave is old/not accepted, then the acc_name column has the new, accepted name for that species 
## the syn_name column contains all synonyms for that binomial 

## left join the synonym df with the species we found tsns for 
syns_df <- left_join(syns_df, found, by = c("sub_tsn" = "ids")) %>%
  filter(!is.na(sub_tsn)) %>%
  select(binomial, everything())

## add back the species that were not found: 
not_found <- tsns_tt %>%
  subset(match == "not found") 

no_syn <- tsns_tt %>%
  subset(match == "found") %>%
  filter(!ids %in% syns_df$sub_tsn)

syns_df <- bind_rows(syns_df, no_syn) %>% 
  select(-class, -ids) %>%
  bind_rows(., not_found) %>%
  ## get rid of NAs in accepted names column by filling with binomial if NA 
  mutate(acc_name = ifelse(is.na(acc_name), as.character(binomial), as.character(acc_name)))

names <- syns_df %>%
  select(binomial, acc_name) %>%
  filter(!duplicated(.))

## save a key to reference the old versus corrected names later:
write.csv(names, "data-processed/acclitherm_taxize-key.csv", row.names = FALSE)

## save a key to reference the synonyms list later:
write.csv(syns_df, "data-processed/acclitherm_synonyms-key.csv", row.names = FALSE)

## correct namesin the database:
acclitherm <- acclitherm %>%
  ungroup() %>%
  left_join(., names, by = c("genus_species" = "binomial")) %>%
  select(-genus_species) %>%
  rename("genus_species" = acc_name) %>%
  mutate(genus = str_split_fixed(genus_species, pattern = " ", n = 2)[,1],
         species = str_split_fixed(genus_species, pattern = " ", n = 2)[,2]) %>%
  mutate(population_id = paste(genus_species, latitude, longitude, sep = "_")) %>% 
  ## make sure populations with no location are not grouped together
  mutate(population_id = ifelse(is.na(latitude) & is.na(longitude),
                                paste(genus_species, ref, sep = "_"),
                                population_id)) 

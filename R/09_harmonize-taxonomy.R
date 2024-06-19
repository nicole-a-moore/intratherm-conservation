## taxonomic harmonization between acclitherm and bioshifts 
library(tidyverse)
library(taxadb)

## function to harmonize a list of species names 
harmonize_taxnomy <- function(names) {
  
  ## first, search for GBIF accepted names
  ## GBIF
  gbif = filter_name(names, "gbif") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "gbif") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus)
  
  not_found <- names[which(!names %in% gbif$input)]
  
  ## if not found, search itis
  ## itis
  itis = filter_name(not_found, "itis") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "itis") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus)
  
  not_found <- not_found[which(!not_found %in% itis$input)]
  
  ## if not found, search col
  ## col
  col = filter_name(not_found, "col") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "col") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus)
  
  not_found <- not_found[which(!not_found %in% col$input)]
  
  ## if not found, search ncbi
  ## ncbi
  animal_classes <- c("Aves", "Mammalia", "Reptilia", "Insecta",  "Chondrichthyes", "Amphibia",
                      "Actinopterygii","Teleostei", "Bivalvia", "Elasmobranchii", "Holocephali")
  
  ncbi = filter_name(not_found, "ncbi") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "ncbi") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus) %>%
    mutate(kingdom = ifelse(kingdom == "Metazoa" & class %in% animal_classes,
                            "Animalia",
                            "Plantae")) ## change kingdom = metazoa to plantae or animalia
  
  not_found <- not_found[which(!not_found %in% ncbi$input)]
  
  names_db <- rbind(itis, ncbi) %>% rbind(., col) %>% rbind(., gbif) %>%
    unique(.) %>%
    rename("genus_species" = input,
           "genus_species_accepted" = scientificName) 
  
  ## return database of names and list of species not found
  
  return(list(names_db, not_found))
}

## read in acclitherm 
acclitherm <- read.csv("data-processed/intermediate-data/acclitherm_Rohr-Comte-Intra.csv")

## harmonize taxonomy 
harmonized_acc <- harmonize_taxnomy(acclitherm$genus_species)

acc_accsp <- harmonized_acc[[1]]
acc_notfsp <- harmonized_acc[[2]]

## read in list of bioshifts species from Brunno 
splist <- read.csv("data-raw/bioshifts/splist.csv")

## find out which overlap
length(which(acc_accsp$genus_species_accepted %in% splist$species)) ## 61
length(which(acc_notfsp %in% splist$species))

## generate list of the overlapping species
sp_overlap <- acc_accsp$genus_species[which(acc_accsp$genus_species_accepted %in% splist$species)]

write.csv(sp_overlap, "data-processed/bioshifts_sp_of_interest.csv", row.names = F)

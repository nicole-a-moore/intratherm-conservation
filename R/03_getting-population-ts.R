## extract population time series for species in acclitherm 
library(tidyverse)

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
syns_df <- read_csv("data-processed/acclitherm_synonyms.csv")
syns = append(syns_df$binomial, syns_df$syn_name) %>%
  append(., syns$acc_name) %>%
  unique()

names <- taxa$TaxonName
syns[which(syns %in% names)] # 41 overlapping species 

## subset to overlapping data 
overlap <- intersect(names, syns)

ol_syns <- filter(syns_df, syn_name %in% syns)

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
ol <- left_join(overlap_gdata, overlap_main) %>% 
  left_join(., overlap_taxa, by  = "TaxonID") %>% 
  left_join(., location, by = "LocationID") %>%
  distinct()

ol$genus_species[which(!ol$genus_species %in% acclitherm$genus_species)]

## join with acclitherm


## 0_0 - from Klicava Reservoir, coordinates should be 50.08, 13.9
ol$LatDD[which(ol$LocationID == 10323)]  <- 50.08
ol$LongDD[which(ol$LocationID == 10323)]  <- 13.9

location$LatDD[which(location$LocationID == 10323)]  <- 50.08
location$LongDD[which(location$LocationID == 10323)]   <- 13.9

ol %>% 
  mutate(unique_population = paste(TaxonName, MainID, sep = "_")) %>% 
  select(unique_population, everything()) %>% 
  ggplot(aes(x = SeriesStep, y = Population, group = unique_population, color = TaxonName)) + 
  geom_line() +
  facet_wrap( ~ unique_population, scales = "free") 

ggsave("figures/gpdd-taxon.pdf", width = 20, height = 12)

write_csv(ol, "data-processed/poppulation-ts/gpdd_acclitherm-spp.csv")


## Living Planet Index








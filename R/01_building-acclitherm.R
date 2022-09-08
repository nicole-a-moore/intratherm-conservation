## pulling data for species with at least 2 thermal tolerance estimates from existing databases 
library(tidyverse)
library(janitor)
library(viridis)
library(cowplot)
library(readxl)
library(taxize)
library(broom)
library(plyr)
rename = dplyr::rename

## prep Rohr -----------------------------------------------
amphib <- read_csv("data-raw/existing-data/amphib-rohr.csv") %>% 
  clean_names()

am2 <- amphib %>% 
  unite(col = genus_species, sep = " ", remove = FALSE, genus3, species3) %>% 
  unite(col = record_species, sep = "_", remove = FALSE, record, genus_species) 

## make sure all are unique populations
unique_pop <- am2 %>% 
  distinct(genus_species, latitude, longitude) %>% 
  group_by(genus_species) %>% 
  tally() %>%
  select(genus_species)

rohr_unique_pop <- am2 %>% 
  filter(genus_species %in% unique_pop$genus_species) 

write_csv(rohr_unique_pop, "data-processed/existing-data/rohr_amphib_unique_pop.csv")


## prep Comte & Olden -----------------------------------------------
comte <- read_csv("data-raw/existing-data/comte-all.csv")

unique_pop <- comte %>% 
  distinct(Species, Latitude, Longitude) %>% 
  group_by(Species) %>% 
  tally() %>%
  select(Species)

comte_unique_pop <- comte %>% 
  filter(Species %in% unique_pop$Species) 

write_csv(comte_unique_pop, "data-processed/existing-data/comte_fish_unique_pop.csv")


## merge together raw intratherm extractions -----------------------------------------------
intra <- read_csv("data-raw/intratherm-extractions/Globtherm2_within_species_2018_12_17.csv") %>% 
  clean_names() %>% 
  mutate(genus_species = paste(genus, species, sep = "_")) %>% 
  mutate_all(funs(as.character)) %>% 
  mutate(extractor = "group")

so <- read_excel("data-raw/intratherm-extractions/Globtherm2_within_species_SO.xlsx") %>% 
  clean_names() %>% 
  mutate(genus_species = paste(genus, species, sep = "_")) %>% 
  mutate_all(funs(as.character)) %>% 
  mutate(extractor = "SO")

jb <- read_excel("data-raw/intratherm-extractions/Globtherm2_within_species_JB.xlsx") %>% 
  clean_names() %>% 
  mutate(genus_species = paste(genus, species, sep = "_")) %>% 
  mutate_all(funs(as.character)) %>% 
  mutate(extractor = "JB")

fl <- read_excel("data-raw/intratherm-extractions/Globtherm2_within_species_FL.xlsx") %>% 
  clean_names() %>% 
  mutate(genus_species = paste(genus, species, sep = "_")) %>% 
  mutate_all(funs(as.character)) %>% 
  mutate(extractor = "FL")

ab <- read_excel("data-raw/intratherm-extractions/Globtherm2_within_species_AB.xlsx") %>% 
  clean_names() %>% 
  mutate(genus_species = paste(genus, species, sep = "_")) %>% 
  mutate_all(funs(as.character)) %>% 
  mutate(extractor = "AB")

fv <- read_excel("data-raw/intratherm-extractions/Globtherm2_FV_Test.xlsx") %>% 
  clean_names() %>% 
  mutate(genus_species = paste(genus, species, sep = "_")) %>% 
  mutate_all(funs(as.character)) %>% 
  mutate(extractor = "FV")

all_mult <- bind_rows(so, ab, fl, fv, intra)

write_csv(all_mult, "data-processed/intermediate-data/team-intratherm-extracted.csv")

## clean the data 
all_mult2 <- all_mult %>% 
  mutate(parameter_value = str_replace(parameter_value, "<", "")) %>% 
  mutate(parameter_value = as.numeric(parameter_value)) %>% 
  mutate(error_estimate = as.numeric(error_estimate)) %>% 
  mutate(lat_of_collection = as.numeric(lat_of_collection)) %>% 
  mutate(original_compilation = "intratherm_team") %>% 
  rename(latitude = lat_of_collection,
         longitude = long_of_collection) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>% 
  rename(acclim_temp = pretreatment_temp) %>% 
  rename(acclim_time = pretreatment_duration) %>% 
  mutate(acclim_temp = as.numeric(acclim_temp)) %>% 
  mutate(ramping_rate = as.numeric(ramping_rate)) %>% 
  mutate(n_cat = NA) %>% 
  mutate(n_cat = ifelse(grepl(">", sample_size), sample_size, n_cat)) %>%
  mutate(n_cat = ifelse(grepl("<", sample_size), sample_size, n_cat)) %>%
  mutate(n_cat = ifelse(grepl("-", sample_size), sample_size, n_cat)) %>% 
  mutate(sample_size = ifelse(!grepl("[^0-9]", sample_size), sample_size, NA)) %>% 
  mutate(sample_size = as.numeric(sample_size))

all_mult2 %>% 
  ggplot(aes(x = latitude, y = parameter_value, color = parameter_tmax_or_tmin)) + geom_point()


## create species inventory list -----------------------------------------------
rohr <- read_csv("data-processed/existing-data/rohr_amphib_unique_pop.csv")

rohr2 <- rohr %>% 
  mutate(parameter_tmax_or_tmin = "tmax") %>% 
  rename(parameter_value = raw_ctm1) %>% 
  rename(genus = genus1,
         species = species1,
         ramping_rate = heating_rate) %>% 
  mutate(original_compilation = "Rohr") %>% 
  mutate(acclim_time = as.character(acclim_time)) %>% 
  rename(life_stage = stage) %>% 
  rename(sample_size = n_numb) %>% 
  rename(realm_general = habitat) %>% 
  separate(record_species, into = c("record_number", "genus_species"), sep = "_") %>% 
  rename(metric_description = ep1_descrip) %>% 
  rename(ref = reference) %>% 
  rename(location_description = locality)

mult_species <- all_mult2 %>% 
  distinct(genus, species)

rohr_species <- rohr %>% 
  distinct(genus1, species1) %>% 
  rename(genus = genus1,
         species = species1)

comte_species <- read_csv("data-processed/existing-data/comte_fish_unique_pop.csv") %>% 
  clean_names() %>% 
  separate(species, into = c("genus", "species")) %>% 
  distinct(genus, species)

comte <- read_csv("data-processed/existing-data/comte_fish_unique_pop.csv") %>% 
  clean_names() %>% 
  separate(species, into = c("genus", "species")) %>% 
  mutate(parameter_tmax_or_tmin = "tmax") %>% 
  rename(acclim_temp = temperature_of_acclimation_c) %>% 
  rename(acclim_time = length_acclimation_period_days,
         ramping_rate = heating_rate_c_min) %>%
  rename(parameter_value = thermal_limit_c) %>% 
  mutate(original_compilation = "Comte") %>% 
  mutate(acclim_time = as.character(acclim_time)) %>% 
  mutate(reference = as.character(reference)) %>% 
  rename(realm_general = realm_affinity) %>% 
  rename(sample_size = nindividuals) %>% 
  mutate(error_type = "SD") %>% 
  rename(error_estimate = sd_thermal_limit) %>% 
  rename(metric_description = endpoint) %>% 
  rename(ref = source) %>% 
  rename(location_description = location)

all_species <- bind_rows(mult_species, rohr_species, comte_species) %>% 
  distinct(genus, species)

## merging and cleaning Intratherm extractions, Rohr and Comte data --------
combined_tmax <- bind_rows(all_mult2, rohr2, comte) %>% 
  filter(!is.na(parameter_value)) %>% ## get rid of ones with no parameter estimate
  mutate(realm_general2 = case_when(realm_general == "aquatic" ~ "Aquatic",
                                    realm_general == "marine" ~ "Marine",
                                    realm_general == "marine littoral" ~ "Marine",
                                    realm_general == "terrestrial" ~ "Terrestrial",
                                    realm_general == "Arboreal" ~ "Terrestrial",
                                    realm_general == "freshwater" ~ "Freshwater",
                                    realm_general == "freshwater native" ~ "Freshwater",
                                    TRUE ~ realm_general)) %>% 
  mutate(realm_general3 = case_when(realm_general2 %in% c("Aquatic", "Aquatic & terrestrial", "Freshwater", 'Marine') ~ "Aquatic",
                                    realm_general2 == "Terrestrial" ~ "Terrestrial")) %>% 
  mutate(species = ifelse(species == "sylvatica", "sylvaticus", species)) %>% 
  mutate(genus_species = ifelse(is.na(genus_species), paste(genus, species, sep = " "), genus_species)) %>% 
  mutate(genus_species = str_replace(genus_species, "_", " ")) %>% 
  mutate(genus_species = str_replace(genus_species, "sylvatica", "sylvaticus"))

unique(combined_tmax$acclim_time)

comb_tmax2 <- combined_tmax %>% 
  mutate(acclim_time_original = acclim_time) %>% ## save a version of the acclim time as originally entered
  mutate(acclim_time_units = case_when(grepl("d", acclim_time) ~ "days",
                                       grepl("w", acclim_time) ~ "weeks",
                                       grepl("m", acclim_time) ~ "minutes",
                                       grepl("h", acclim_time) ~ "hours",
                                       TRUE ~ "days")) %>%
  mutate(acclim_time = str_replace(acclim_time, "overnight", "0.5")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "15-17d", "15")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "7-10d", "7")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "12-14", "12")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "10-14", "10")) %>%
  mutate(acclim_time = str_replace(acclim_time, "8-10w", "8")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "57-63d", "57")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "30-51d", "51")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "58-70d", "57")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "2-4m", "2")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "w", "")) %>% 
  mutate(acclim_time = str_replace(acclim_time, "h", "")) %>%
  mutate(acclim_time = str_replace(acclim_time, "d", "")) %>%
  mutate(acclim_time = str_replace(acclim_time, "[A-Za-z]", "")) %>% 
  mutate(acclim_time = as.numeric(acclim_time)) %>% 
  mutate(acclim_time = ifelse(acclim_time_units == "weeks", acclim_time*7, acclim_time)) %>% 
  mutate(acclim_time = ifelse(acclim_time_units == "hours", acclim_time/24, acclim_time)) %>% 
  mutate(acclim_time = ifelse(acclim_time_units == "minutes", acclim_time/1440, acclim_time)) %>% 
  select(genus_species, genus, species, latitude, longitude, location_description, realm_general3, acclim_temp, acclim_time, parameter_value, parameter_tmax_or_tmin, everything())

unique(comb_tmax2$acclim_time)

write_csv(comb_tmax2, "data-processed/intermediate-data/combined-thermal-limits.csv")
comb_tmax2 <- read_csv("data-processed/intermediate-data/combined-thermal-limits.csv")

## keep species with multiple acclimation temperatures 
mult_acclim_comb <- comb_tmax2 %>% 
  distinct(genus_species, acclim_temp, .keep_all = TRUE) %>% 
  group_by(genus_species) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  select(genus_species)

acclitherm <- comb_tmax2 %>% 
  filter(genus_species %in% c(mult_acclim_comb$genus_species)) %>% 
  select(genus_species, latitude, longitude, everything()) %>% 
  mutate(population_id = paste(genus_species, latitude, longitude, sep = "_")) %>% 
  ## make sure populations with no location are not grouped together
  mutate(population_id = ifelse(is.na(latitude) & is.na(longitude),
                                paste(genus_species, ref, sep = "_"),
                                population_id)) %>%
  select(genus_species, population_id, acclim_temp, everything())

write_csv(acclitherm, "data-processed/intermediate-data/acclitherm-multi-acclim.csv")
acclitherm <- read_csv("data-processed/intermediate-data/acclitherm-multi-acclim.csv")

unique(acclitherm$genus_species) ## 450 species with at least 1 population & multiple acclimation temps

acclitherm_species <- acclitherm %>% 
  distinct(genus_species, .keep_all = TRUE) %>% 
  select(genus_species, phylum, class, order, family, life_stage)

## get Class of each species
classes <- data.frame()
for(species in unique(acclitherm$genus_species)) {
  #classes <- rbind(classes, tax_name(sci = species, get = "class", db = "ncbi"))
}
#write_csv(classes, "data-processed/intermediate-data/acclitherm-classes.csv")
classes <- read_csv("data-processed/intermediate-data/acclitherm-classes.csv")

## check other database
isna <- read_csv("data-processed/intermediate-data/acclitherm-classes.csv") %>% 
  filter(is.na(class)) 

notna <- read_csv("data-processed/intermediate-data/acclitherm-classes.csv") %>% 
  filter(!is.na(class)) 

na_classes <- data.frame()
for(species in unique(isna$query)) {
  #na_classes <- rbind(na_classes, tax_name(sci = species, get = "class", db = "itis"))
}
#write_csv(na_classes, "data-processed/intermediate-data/acclitherm-na-classes.csv")
na_classes <- read_csv("data-processed/intermediate-data/acclitherm-na-classes.csv")

classes <- rbind(na_classes, notna)
#write_csv(classes, "data-processed/intermediate-data/acclitherm-classes.csv")

## fill in missing classes manually 
missing <- classes[which(is.na(classes$class)),]

missing$class[which(str_detect(missing$query, c("Hyla")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Litoria")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Cyprinodon")))] <- "Actinopteri"
missing$class[which(str_detect(missing$query, c("Eleutherodactylus")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Takydromus")))] <- "Reptilia"
missing$class[which(str_detect(missing$query, c("Sphenomorphus")))] <- "Reptilia"
missing$class[which(str_detect(missing$query, c("Chirodica")))] <- "Insecta"
missing$class[which(str_detect(missing$query, c("Crinia")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Limnodynastes")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Anaxyrus")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Cryptopygus")))] <- "Entognatha"
missing$class[which(str_detect(missing$query, c("Lithobates")))] <- "Amphibia"
missing$class[which(str_detect(missing$query, c("Ambassis")))] <- "Actinopteri"
missing$class[which(str_detect(missing$query, c("Achirus")))] <- "Actinopteri"
missing$class[which(str_detect(missing$query, c("Leuresthes")))] <- "Actinopteri"
missing$class[which(str_detect(missing$query, c("Quietula")))] <- "Actinopteri"
missing$class[which(str_detect(missing$query, c("Acanthoclinus")))] <- "Actinopteri"

classes <- classes %>%
  filter(!is.na(class)) %>%
  rbind(., missing)

write_csv(classes, "data-processed/intermediate-data/acclitherm-classes-checked.csv")

acclitherm_classes <- read_csv("data-processed/intermediate-data/acclitherm-classes-checked.csv") %>% 
  filter(!is.na(class)) %>% 
  distinct(query, class) %>% 
  rename(genus_species = query)

acclitherm <- select(acclitherm, -class) %>%
  left_join(., acclitherm_classes)
  
write_csv(acclitherm, "data-processed/intermediate-data/acclitherm_Rohr-Comte-Intra.csv")

acclitherm %>% 
  ggplot(aes(x = latitude, y = parameter_value, color = realm_general)) + geom_point() +
  ylab("Thermal limit (°C)") + xlab("Latitude") + facet_grid(realm_general ~ parameter_tmax_or_tmin)

acclitherm %>% 
  filter(parameter_tmax_or_tmin == "tmax") %>% 
  ggplot(aes(x = latitude, y = parameter_value)) + geom_point() +
  ylab("Thermal limit (°C)") + xlab("Latitude") + facet_wrap( ~ genus_species, scales = "free")

ggsave("figures/all_limits.png", width = 49, height = 30, limitsize = FALSE)


## clean up combined dataset -----------------------------------------------
## fix taxonomy columns and remove unnecessary columns leftover from Rohr and Comte 
acclitherm <- acclitherm %>%
  mutate(family = ifelse(is.na(family), family1, family)) %>%
  mutate(elevation_of_collection = ifelse(is.na(elevation_of_collection), elevation, 
                                          elevation_of_collection)) %>%
  select(-family1, -family2, -family3, -genus2, -species2, -genus3, -species3) %>%
  select(-n_cat, -record_number, -record, -n_comment, -raw_ctm2, -acclim, -acclim_time_life_120, 
         -log_acclim_time, -safety, -photoperiod, -ep1, -ep2, -log_elevation, -geog_cat, 
         -log_abs_lat, -comments, -elevation, -abs_lat,-range, -log_range, -introduced, 
         -elev_min, -elev_max, -svl, -log_svl, -tmxslope, -bioclim5, -iucn_2014, -iucn_2104risk) %>%
  select(-rate_acclimation_c_day, -methodology, -length_experiment_min, -body_length_mm, -weight_g,
         -phylogeny, -acclim_time_original, -acclim_time_units, -acclimation, -sampling_habitat, -year,
         -realm_general, -realm_general3) %>%
  rename("citation_Rohr" = citation, "reference_Comte" = reference, "realm_general" = realm_general2) 

acclitherm = acclitherm[,(c(1, 6:7, 4:5, 2, 8, 3, 9:22, 24, 25,36, 26,27, 35, 28, 32, 31, 29, 30, 23, 33:34))]
colnames(acclitherm)

acclitherm = acclitherm %>%
  group_by(genus_species) %>%
  fill(phylum, .direction = "downup") %>%
  fill(order, .direction = "downup") %>%
  fill(class, .direction = "downup")


## taxize and change names to accepted names 
## use taxize to get a list of species synonyms and the most taxonomically correct names:
acc_species <- unique(acclitherm$genus_species)
taxa_tt <- data.frame(binomial = acc_species)

#tsn_search_tt <- get_tsn(as.character(taxa_tt$binomial), accepted = FALSE)
#saveRDS(tsn_search_tt, "data-processed/intermediate-data/tsn_search_acclitherm.rds")
tsns_tt <- data.frame(readRDS("data-processed/intermediate-data/tsn_search_acclitherm.rds"))
tsns_tt$binomial <- taxa_tt$binomial

found <- tsns_tt %>%
  subset(match == "found")  ## get only found found spp

## get synonyms
#syns <- synonyms(tsn_search_tt)
#saveRDS(syns, "data-processed/intermediate-data/syns_acclitherm.rds")
syns <- readRDS("data-processed/intermediate-data/syns_acclitherm.rds")

syns_df <- ldply(syns, data.frame) %>%
  select(-c(1,9))
syns_df <- left_join(syns_df, found, by = c("sub_tsn" = "ids")) %>%
  filter(!is.na(sub_tsn)) %>%
  select(binomial, everything())

## add back species that were not found: 
not_found <- tsns_tt %>%
  subset(match == "not found") 

no_syn <- tsns_tt %>%
  subset(match == "found") %>%
  filter(!ids %in% syns_df$sub_tsn)

syns_df <- bind_rows(syns_df, no_syn) %>% 
  select(-class, -ids) %>%
  bind_rows(., not_found) %>%
  mutate(acc_name = ifelse(is.na(acc_name), as.character(binomial), as.character(acc_name)))

names <- syns_df %>%
  select(binomial, acc_name) %>%
  filter(!duplicated(.))

## save a key to reference correct names later:
write.csv(names, "data-processed/acclitherm_taxize-key.csv", row.names = FALSE)

## save a key to reference synonyms names later:
write.csv(syns_df, "data-processed/acclitherm_synonyms-key.csv", row.names = FALSE)

## correct names:
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

## make some figures
acclitherm %>% 
  # filter(parameter_tmax_or_tmin == "tmax") %>% 
  group_by(population_id) %>% 
  ggplot(aes(x = acclim_temp, y = parameter_value, color = latitude)) + geom_point() +
  ylab("Thermal limit (°C)") + xlab("Acclimation temperature") + facet_wrap( ~ parameter_tmax_or_tmin, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none")

ggsave("figures/ARR-all.png", width = 10, height = 8)

acclitherm %>%
  filter(parameter_tmax_or_tmin == "tmax") %>% 
  filter(!is.na(acclim_temp)) %>% 
  mutate(lat2 = round(abs(latitude), digits = 0)) %>% 
  group_by(population_id, latitude) %>% 
  do(tidy(lm(parameter_value ~ acclim_temp, data = .), conf.int = TRUE)) %>% 
  filter(term == "acclim_temp", estimate > 0) %>% 
  ggplot(aes(x = abs(latitude), y = estimate)) + geom_point() +
  ylim(0, 1.5) +ylab("ARR") + xlab("Absolute latitude")

ggsave("figures/ARR-latitude.png", width = 6, height = 4)

acclitherm %>% 
  filter(!is.na(acclim_temp)) %>%
  filter(parameter_tmax_or_tmin == "tmax") %>% 
  ggplot(aes(x = latitude, y = parameter_value, color = acclim_temp)) + geom_point() +
  ylab("Thermal limit (°C)") + xlab("Latitude") +
  facet_wrap( ~ parameter_tmax_or_tmin, scales = "free") +
  scale_color_viridis_c(name = "Acclimation temperature (°C)")

ggsave("figures/CTmax-latitude.png", width = 10, height = 4)


## get rid of CTmin, calculate ARRs -----------------------------------------------
## populations with at least 2 CTmax estimates: 631
pops <- acclitherm %>%
  distinct(population_id, genus_species, acclim_temp) %>%
  group_by(population_id) %>%
  tally() %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(., acclitherm)

## species with at least 2 CTmax estimates of the same type: 45
specs <- acclitherm %>%
  filter(!genus_species %in% pops$genus_species) %>%
  distinct(population_id, genus_species, acclim_temp) %>%
  group_by(genus_species) %>%
  tally() %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(., acclitherm)

## plot specs ones:
specs %>% 
  ggplot(aes(x = acclim_temp, y = parameter_value, color = population_id)) + geom_point() +
  ylab("Thermal limit (°C)") + xlab("Acclimation temperature") + 
  facet_wrap( ~ genus_species, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none")
## lets get rid of these, they look too heterogeneous


## ARR calculation:
acclitherm_pops <- acclitherm %>%
  filter(!is.na(acclim_temp)) %>%
  distinct(population_id, genus_species, acclim_temp) %>%
  group_by(population_id) %>%
  tally()  %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(., acclitherm)

acclitherm_pops %>%
  filter(parameter_tmax_or_tmin == "tmax") %>%
  group_by(genus_species, population_id) %>%
  do(tidy(lm(parameter_value ~ acclim_temp, data = .), conf.int = TRUE)) %>% #fit lm to each group
  filter(term == "acclim_temp") %>% #extract slope and intercept 
  rename(ARR = estimate) #rename these ARR
 
## which ARR when there are multiple populations?


## save final species list (406)
acclitherm_species <- select(acclitherm_pops, genus_species, genus, species, 
                             phylum, order, family, class) %>%
  unique()

write_csv(acclitherm_species, "data-processed/acclitherm_species-list.csv")


## clean realm column 
acclitherm <- acclitherm %>%
  filter(realm_general == "Aquatic") %>%
  mutate(realm_general = ifelse(realm_specific %in% c("marine", "estuarine"),
                                "Marine",
                                ifelse(realm_specific %in% c("pond", "stream", "estuarine"),
                                       "Freshwater",
                                       ifelse(class == "Amphibia",
                                              "Terrestrial",
                                              "Aqautic")))) %>%
  rbind(., filter(acclitherm, realm_general != "Aquatic")) %>%
  mutate(realm_general = ifelse(realm_general == "Aquatic & terrestrial",
                                "Terrestrial",
                                realm_general))

## make sure all amphibias are terrestrial 
acclitherm <- acclitherm %>%
  filter(class == "Amphibia") %>%
  mutate(realm_general = "Terrestrial") %>%
  rbind(., filter(acclitherm, class != "Amphibia"))

## save final dataset
write_csv(acclitherm, "data-processed/acclitherm.csv")

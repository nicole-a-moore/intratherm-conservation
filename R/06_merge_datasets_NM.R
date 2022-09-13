## script to check the species overlap between intratherm, bioshifts, new papers to extract, morley papers, and the population time series data 
library(tidyverse)

intratherm_data<-read_csv("data-raw/intratherm.csv")
bioshifts_data<-read_csv("data-raw/BioShifts/Bioshifts/BioShifts.csv")
acclimation_2019_species<-read_csv("data-raw/2019_new_acclimation_papers_species.csv")
acclitherm_compiled_nikki<-read_csv("data-processed/acclitherm_synonyms-key.csv")
morley<-read_csv("data-raw/existing-data/morley.csv") 

morley_species <- morley[9:nrow(morley),1:46]
colnames(morley_species) <- str_replace_all(morley[8,], " ", "_")

morley_species <- morley_species %>%
  filter(!Life_History_stage %in% c("juvenile", "larvae")) %>% ## filter out studies of juveniles, larvae
  select(Name, Reference, doi) %>%
  unique(.) %>%
  filter(!is.na(Reference))

population_species <- read_csv("data-processed/population-ts_species-list.csv")
  
#how many of the fast-compiled acclitherm 1.0 species are in bioshifts?
merged_accli_shifts<-inner_join(acclitherm_compiled_nikki, bioshifts_data, by=c("acc_name" = "Species"))
merged_accli_shifts$Position

merged_accli_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(acc_name) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  dim(.)

#53 species between acclitherm 1.0 and bioshifts
#27 species between acclitherm 1.0 and bioshifts trailing edge


#how many more would be in the 2019 acclitherm search additions?
merged_accli_shifts<-inner_join(acclimation_2019_species, bioshifts_data, by=c("species" = "Species"))
merged_accli_shifts$Position

merged_accli_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(species) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  dim(.)

#8 more species between acclitherm 1.1 and bioshifts
#5 more species between acclitherm 1.1 and bioshifts trailing edge

#which species?
overlap_species_new_papers_2019<-merged_accli_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(species) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  select(species,ref)

write_csv(overlap_species_new_papers_2019, "data-processed/overlap_species_new_papers_2019.csv")


#how many more would be in the Morley 2019 dataset?

View(morley_species)

merged_accli_shifts<-inner_join(morley_species, bioshifts_data, by=c("Name" = "Species"))
merged_accli_shifts$doi

merged_accli_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(Name) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  dim(.)

#14 more species between acclitherm 1.1 and bioshifts
#7 more species between acclitherm 1.1 and bioshifts trailing edge

#which species?
overlap_species_Morley<-merged_accli_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(Name) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  select(Name, Reference.x, doi)

write_csv(overlap_species_Morley, "data-processed/overlap_species_Morley.csv")



#how many of the intratherm 1.0 species are in bioshifts?
#first try without taxonomic harmonization
merged_intra_shifts<-inner_join(intratherm_data, bioshifts_data, by=c("genus_species" = "Species"))
merged_intra_shifts$Position # this column describes range position of range shift - we want trailing edge

merged_intra_shifts %>%
  #filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(genus_species) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  dim(.)

#32 species overlap between intratherm and bioshifts


merged_intra_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(genus_species) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup %>%
  dim(.)

#17 species overlap  between intratherm and warm range edge of bioshifts

#which species?
merged_intra_shifts_warm <- merged_intra_shifts %>%
  filter(Position=="Trailing edge") %>% #subset to trailing edge
  group_by(genus_species) %>% #just get one row for each species for now
  slice(1) %>% 
  ungroup 

merged_intra_shifts_warm$genus_species

#View(merged_intra_shifts_warm)
#mostly fish, mostly elevational trailing edge shifts
#some shift rates are negative, some are not significant... so actually could ask if thermal danger explains variation here.
merged_intra_shifts_warm$parameter_value

merged_intra_shifts_warm %>%
  ggplot(aes(y=ShiftR, x=parameter_value, col=Signif)) +
  facet_wrap(~Gradient) +
  geom_point()


## how many population time series species are in morley and new 2019 acclitherm papers?
length(which(unique(morley_species$Name) %in% unique(population_species$genus_species))) ## 73 species

morley_population_merge <- inner_join(morley_species, population_species, by = c("Name" = "genus_species")) %>%
  select(Name, Reference, doi)

length(which(unique(acclimation_2019_species$species) %in% unique(population_species$genus_species))) ## 23 species

newpapers_population_merge <- inner_join(acclimation_2019_species, population_species, 
                                      by = c("species" = "genus_species")) %>%
  select(species, ref)
  
  
## next: create master list of papers to be extracted 
## (all 2019 search or morley papers that have data on species that overlap population time series data OR bioshifts data)

newpapers_population_merge <- rename(newpapers_population_merge, "genus_species" = species, "reference" = ref)
newpapers_population_merge$paper_source = "new_papers_2019"
newpapers_population_merge$doi = NA 

morley_population_merge <- rename(morley_population_merge, "genus_species" = Name, "reference" = Reference)
morley_population_merge$paper_source = "Morley"

overlap_species_new_papers_2019 <- rename(overlap_species_new_papers_2019, "genus_species" = species, "reference" = ref)
overlap_species_new_papers_2019$paper_source = "new_papers_2019"
overlap_species_new_papers_2019$doi = NA


overlap_species_Morley <- rename(overlap_species_Morley, "genus_species" = Name, "reference" = Reference.x)
overlap_species_Morley$paper_source = "Morley"

extraction_list <- rbind(newpapers_population_merge, overlap_species_new_papers_2019) %>%
  rbind(., morley_population_merge) %>%
  rbind(., overlap_species_Morley) %>%
  unique()

length(unique(extraction_list$genus_species)) ## 98 new species!
length(unique(extraction_list$reference)) ## 68 papers to extract

extraction_list$extracted = "N"
extraction_list$discarded = "N"
extraction_list$notes = NA

write_csv(extraction_list, "data-processed/extraction-list.csv")
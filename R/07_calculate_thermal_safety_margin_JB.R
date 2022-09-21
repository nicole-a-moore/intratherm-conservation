### Calculate thermal safety margin



library(tidyverse)

pop_data <- readRDS("data-processed/population-time-series-with-temps.rds")

pops_raw <- bind_rows(pop_data)

pops <- bind_rows(pop_data) %>% 
  filter(!is.na(abundance))

sum_temp <- pops_raw %>% 
  filter(!is.na(temperature)) %>% 
  group_by(year, population_id) %>% 
  summarise(q99=quantile(temperature, probs=0.99))



names(pops)

acclitherm <- read_csv("data-processed/acclitherm_after-matching.csv")


pops2 <- pops %>% 
  left_join(acclitherm, by = "genus_species")




pops2 %>% 
  filter(genus_species == "Abramis brama") %>% 
  ggplot(aes(x = date, y = temperature)) + geom_line() 

pops2 %>% 
  filter(genus_species == "Abramis brama") %>% 
  ggplot(aes(x = date, y = abundance)) + geom_line() 

pops_raw %>% 
  filter(genus_species == "Abramis brama") %>% 
  ggplot(aes(x = date, y = abundance)) + geom_line() 

pops %>% 
  filter(genus_species == "Abramis brama") %>% 
  ggplot(aes(x = date, y = abundance, color = temperature)) + geom_point() +
  facet_wrap( ~ population_id)


pops2 %>% 
  filter(genus_species == "Abramis brama") %>% 
  mutate(tsm = parameter_value - temperature) %>% 
  ggplot(aes(x = date, y = abundance, color = tsm)) + geom_point() +
  facet_wrap( ~ population_id.x) + scale_color_viridis_c()

ab <- pops %>% 
  filter(genus_species.x == "Abramis brama") 
  
  

ab %>% 
  filter(!is.na(abundance)) %>% 
  mutate(tsm = temperature - parameter_value) %>% View
ggplot(aes(x = date, y = temperature)) + geom_line() +
  facet_wrap(~ population_id)

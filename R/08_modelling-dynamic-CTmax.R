## predicted CTmax and population dynamics analysis 
library(tidyverse)
library(broom)

## read in R object of population and temperature time series dataframes:
pops <- readRDS("data-processed/population-time-series-with-temps.rds")

## create acclimation temperature column for each population that is mean temperature over the 7 days before 
pop = 1 
while (pop <= length(pops)) {
  
  ## get pop 
  pd <- pops[[pop]]
  
  i = 1
  acclim_temps = c()
  while (i <= nrow(pd) - 7) {
    acclim_temps <- append(acclim_temps, mean(pd$temperature[i:(i+6)], na.rm = TRUE))
    i = i + 1
  }
  
  ## add acclimation temperature column and save 
  pd$acclim_temps <- append(rep(NA, 7), acclim_temps)
  pops[[pop]] = pd
  
  pop = pop + 1
  
  print(paste("on population: ", pop, sep = ""))
}

saveRDS(pops, "data-processed/population-time-series-with-temps-and-acclim-temps.rds")


## read in acclitherm data
acclitherm <- read_csv("data-processed/acclitherm_after-matching.csv") %>%
  filter(parameter_tmax_or_tmin == "tmax") %>%
  filter(!is.na(acclim_temp)) 

multi_acc <- acclitherm %>% 
  group_by(genus_species, population_id) %>% 
  tally() %>%   
  filter(n > 1)

acclitherm <-  acclitherm %>% 
  filter(population_id %in% c(multi_acc$population_id)) 

## calculate one acclimation response ratio per species
## do this by calculating population-level ARRs, grouping by species, and averaging slope and intercept
arr_fits <- acclitherm %>% 
  group_by(population_id) %>% 
  filter(length(unique(acclim_temp)) > 1) %>%
  do(tidy(lm(parameter_value~acclim_temp, data=.))) %>%
  mutate(genus_species = str_split_fixed(population_id, "\\_", 2)[,1]) %>% 
  select(-c("p.value", "std.error", "statistic")) %>%
  spread(key = term, value = estimate) %>%
  dplyr::rename("intercept" = `(Intercept)`, "ARR_slope" = acclim_temp) %>%
  group_by(genus_species) %>%
  mutate(mean_ARR_slope = mean(ARR_slope),
         mean_intercept = mean(intercept)) %>%
  select(genus_species, mean_ARR_slope, mean_intercept) %>%
  unique()

## bind population dataframes together 
pops <- bind_rows(pops) %>%
  mutate(genus_species = str_split_fixed(population_id, pattern = "_", n = 3)[,1]) %>%
  mutate(acclim_temp = lag(temperature, n = 7)) %>%
  filter(!is.na(acclim_temp))

## model dynamic CTmax by using acclimation response ratios to predict CTmax each day for a given acclimation temperature
predictions <- pops %>%
  select(genus_species, acclim_temp, date, temperature, population_id, abundance) %>%
  group_by(genus_species) %>% 
  left_join(., arr_fits) %>%
  ## calculate dynamic CTmax 
  mutate(dynamicCTmax = mean_ARR_slope*acclim_temp + mean_intercept) 

## model static CTmax as the CTmax acclimated to the mean temperature of the location
predictions = predictions %>%
  ungroup() %>%
  group_by(population_id) %>%
  mutate(mean_acclim_temp = mean(temperature, na.rm = TRUE)) %>%
  mutate(staticCTmax = mean_ARR_slope*mean_acclim_temp + mean_intercept) %>%
  select(-mean_acclim_temp)

## save: 
saveRDS(predictions, "data-processed/population-time-series-with-temps_thermal-data.rds")


##check out each population:
i = 1
while (i < length(unique(predictions$population_id))+1) {
	pop <- predictions %>%
		filter(population_id == unique(predictions$population_id)[i])
	
	avg <- (mean(pop$temperature, na.rm=TRUE) + mean(pop$dynamicCTmax, na.rm=TRUE)) /2
	avg_abd <- mean(pop$abundance, na.rm=TRUE)
	
	scale <- avg/avg_abd
	
	pop_id_split <- str_split_fixed(pop$population_id, n = 4, pattern = "_")
	label <- paste(pop_id_split[1,1], "\n", pop_id_split[1,2], ", ", pop_id_split[1,3], sep = "")
	
	pop %>% 
		ggplot(aes(x = date, y = temperature)) + 
		geom_line(aes(colour = "Temperature")) +
		geom_line(aes(y = dynamicCTmax, x = date, colour = "Predicted CTmax")) +
		geom_line(data = na.omit(pop), aes(y = abundance*scale, x = date, colour = "Abundance")) + 
		labs(x = "Date", y = "Temperature (°C)") +
		scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Abundance")) + 
		theme(axis.title.y.right = element_text(color = "blue")) +
		scale_colour_manual(name = label,
							values = c(`Predicted CTmax`="orange", 
												 Abundance="blue", Temperature="black"))
	
	ggsave(filename = paste("figures/temp-ctmax-abundance-figures/", unique(pop$population_id), ".png", sep = ""), height = 6, width = 12, units = "in", device = "png")
	
	i = i + 1
}






## garbage:
 
## see how many times temperature exceeds predicted CTmax when time lag in acclimation is 0 days 
count <- predictions %>%
	mutate(temp_dif = predictedCTmax - temperature) 

length(which(count$temp_dif < 0)) ## CTmax is exceeded a total of 23 times 

## how many different populations is CTmax exceeded in?
length(unique(count$population_id[which(count$temp_dif < 0)]))  ## in 2 populations only

count <- count %>% 
	filter(population_id %in% unique(count$population_id[which(count$temp_dif < 0)]))



## see how much that changes when you incorporate a delay in acclimation
pops2 <- pops %>%
	mutate(acclim_temp = lag(temperature, n = 7))

predictions2 <- pops %>%
	select(genus_species, acclim_temp, date, temperature, population_id, abundance) %>%
	group_by(genus_species) %>% 
	nest() %>% 
	left_join(., arr_fits) %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(predictedCTmax = .fitted, se = .se.fit) 

count2 <- predictions2 %>%
	mutate(temp_dif = predictedCTmax - temperature) 

length(which(count2$temp_dif < 0)) ## 23

count2 <- count2 %>% 
	filter(population_id %in% unique(count2$population_id[which(count2$temp_dif < 0)]))


## see how many days the temperature is within 3 degrees of ctmax 
count3 <- predictions %>%
	mutate(temp_dif = predictedCTmax - (temperature + 3))

length(which(count3$temp_dif < 0)) ## 1431

## how many different populations is CTmax exceeded in?
length(unique(count3$population_id[which(count3$temp_dif < 0)])) ## 27

count3 <- count3 %>% 
	filter(population_id %in% unique(count3$population_id[which(count3$temp_dif < 0)]))



## try something new:
## when acclim_temp is outside of the range used in the original studies, don't allow extrapolation -- instead assume slope of 0


## add column for range of acclimation temps used for each species in study 
predictions3 <- intratherm %>%
	group_by(genus_species) %>%
	mutate(max_acclim_temp = max(acclim_temp)) %>%
	mutate(min_acclim_temp = min(acclim_temp)) %>% 
	select(genus_species, min_acclim_temp, max_acclim_temp) %>%
	distinct() %>%
	rename(acclim_temp = min_acclim_temp) %>%
	nest() %>% 
	left_join(., arr_fits, by = "genus_species") %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(min_parameter_value = .fitted, se.min = .se.fit) %>%
	select(genus_species, max_acclim_temp, everything()) %>%
	rename(min_acclim_temp = acclim_temp, acclim_temp = max_acclim_temp) %>%
	nest() %>% 
	left_join(., arr_fits, by = "genus_species") %>%
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(max_parameter_value = .fitted, max_acclim_temp = acclim_temp) %>%
	ungroup() %>%
	select(genus_species, min_acclim_temp, min_parameter_value, max_acclim_temp, max_parameter_value) %>%
	left_join(predictions, ., by = "genus_species") %>%
	mutate(predictedCTmax = ifelse(max_acclim_temp < acclim_temp, 
								   max_parameter_value, 
								   predictedCTmax
								   ))

count4 <- predictions3 %>%
	mutate(temp_dif = predictedCTmax -  (temperature + 3)) 

length(which(count4$temp_dif < 0)) ## CTmax is exceeded a total of 1861 times now

## how many different populations is CTmax exceeded in?
length(unique(count4$population_id[which(count4$temp_dif < 0)])) ## 44

count4 <- count4 %>% 
	filter(population_id %in% unique(count4$population_id[which(count4$temp_dif < 0)]))

## what are their realms?
population_overlap <- read_csv("data-processed/population-overlap.csv") %>%
	mutate(population_id = paste(genus_species, latitude, longitude, StudyID, sep = "_")) %>%
	filter(population_id %in% count4$population_id) 

## check out those graphs:
i = 1
while (i < length(unique(count4$population_id))+1) {
	pop <- count4 %>%
		filter(population_id == unique(count4$population_id)[i])
	
	avg <- (mean(pop$temperature, na.rm=TRUE) +  mean(pop$predictedCTmax, na.rm=TRUE)) /2
	avg_abd <- mean(pop$abundance, na.rm=TRUE)
	
	scale <- avg/avg_abd
	
	pop_id_split <- str_split_fixed(pop$population_id, n = 4, pattern = "_")
	label <- paste(pop_id_split[1,1], "\n", pop_id_split[1,2], ", ", pop_id_split[1,3], sep = "")
	
	pop %>% 
		ggplot(aes(x = date, y = temperature)) + 
		geom_line(aes(colour = "Temperature")) +
		geom_line(aes(y = predictedCTmax, x = date, colour = "Predicted CTmax")) +
		geom_line(data = na.omit(pop), aes(y = abundance*scale, x = date, colour = "Abundance")) + 
		labs(x = "Date", y = "Temperature (°C)") +
		scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Abundance")) + 
		theme(axis.title.y.right = element_text(color = "blue")) +
		scale_colour_manual(name = label,
							values = c(`Predicted CTmax`="orange", 
									   Abundance="blue", Temperature="black"))
	
	ggsave(filename = paste("figures/temp-ctmax-abundance-figures/adapted-ARR-function/", unique(pop$population_id), ".png", sep = ""), height = 6, width = 12, units = "in", device = "png")
	
	i = i + 1
	
}

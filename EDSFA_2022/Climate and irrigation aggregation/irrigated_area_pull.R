#Query the harvested cropland and irrigated, harvested cropland areas at the state level. 
#Calculate the proportion of cropland area in each state that is irrigated, in order to control for irrigation in the model of yield stability. 

library(rnassqs)
library(tidyverse)
library(usmap)
library(sf)
library(tigris)

nassqs_auth(key = "88135768-C75A-3AB0-9F93-00FA96F7B2CB")

#Pull state level total harvested areas from census years 
total_area <- nassqs(short_desc = "AG LAND, CROPLAND, HARVESTED - ACRES", 
                     domain_desc = "TOTAL", 
                     agg_level_desc = "STATE", 
                     source_desc = "CENSUS",
                     year = c(1981:2021)) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  rename(fips = state_ansi) %>%
  rename(crop_area = Value)

#Plot for each year
plot_usmap(data = total_area, values = "crop_area", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Harvested area", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~year)

#Pull irrigated area from census years: 
#FRIS years (2013 and 2018) are also returned by this query as they are a follow-on to the census. However, there are not paired census data on non-irrigated area for these years, so 2013 and 2018 are excluded from the calculations. 
irrigated_area <- nassqs(short_desc = "AG LAND, CROPLAND, HARVESTED, IRRIGATED - ACRES", 
                     domain_desc = "TOTAL", 
                     agg_level_desc = "STATE", 
                     source_desc = "CENSUS",
                     year = c(1981:2021)) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  rename(fips = state_ansi) %>%
  filter(year %in% c(1997, 2002, 2007, 2012, 2017)) %>%
  rename(irr_area = Value)

#Replace NA with 0 for proportion 
irrigated_area[is.na(irrigated_area)] <- 0

#Plot for each year
plot_usmap(data = irrigated_area, values = "irr_area", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Harvested area", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~year)

#Join dataframes and calculate proportion for each year
proportion_irrigated <- merge(total_area[,c("fips", "year", "crop_area", "state_name")], 
                              irrigated_area[,c("fips", "year", "irr_area")], 
                              by = c("fips", "year")) %>%
  mutate(prop_irr = irr_area/crop_area) %>%
  filter(!(fips %in% c("15", "02"))) #drop HI and AK

#Plot proportions across years; makes sense
plot_usmap(data = proportion_irrigated, values = "prop_irr", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Harvested area", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~year)

#Export csv for manual entry of 1982, 1987, and 1992 data. 
#In New Hampshire in 1987, the irrigated harvested cropland area was withheld for privacy. This value was filled via linear interpolation. 
#write.csv(proportion_irrigated, file = "irrigated_area.csv")

#Load in completed data, add fips codes, add decades, calculate proportions
irr_all <- read.csv("./Climate and irrigation aggregation/irrigated_area_all.csv") %>%
  mutate(fips = fips(state_name)) %>%
  mutate(decade = ifelse(.$year %in% c(2002, 2007), "2000_2009", "2010_2019")) %>%
  mutate(decade = ifelse(.$year %in% c(1992, 1997), "1990_1999", .$decade)) %>%
  mutate(decade = ifelse(.$year %in% c(1982, 1987), "1980_1989", .$decade)) %>%
  mutate(prop_irr = irr_area/crop_area)

#Plot timeseries for each state to check for any unusual values; quite consistent over time.
ggplot(irr_all, aes(x = year, y = prop_irr)) + 
  geom_point() + geom_line() + facet_wrap(~state_name)

ggplot(irr_all, aes(x = year, y = irr_area)) + 
  geom_point() + geom_line() + facet_wrap(~state_name)

ggplot(irr_all, aes(x = year, y = crop_area)) + 
  geom_point() + geom_line() + facet_wrap(~state_name)
  
#Save.
write.csv(irr_all, file = "./Data/Inputs/Model_covariates/irrigation_model_input.csv")



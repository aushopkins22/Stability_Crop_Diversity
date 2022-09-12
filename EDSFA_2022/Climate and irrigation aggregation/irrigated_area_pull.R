library(rnassqs)
library(tidyverse)
library(usmap)
library(sf)
library(tigris)

nassqs_auth(key = "88135768-C75A-3AB0-9F93-00FA96F7B2CB")

#####Calculate the proportion of area in each county containing cropland for weighting climate data#####
#Pull county-level harvested areas from census years 
total_area <- nassqs(short_desc = "AG LAND, CROPLAND, HARVESTED - ACRES", 
                     domain_desc = "TOTAL", 
                     agg_level_desc = "COUNTY", 
                     source_desc = "CENSUS",
                     year = c(1981:2021)) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  mutate(fips = paste0(state_ansi, county_ansi))

#Plot for each year
plot_usmap(data = total_area, values = "Value", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Harvested area", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~year)

#Calculate and plot average area across all the census years
avg_area <- total_area %>%
  filter(!is.na(Value)) %>%
  aggregate(Value~fips, ., mean)

plot_usmap(data = avg_area, values = "Value", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Harvested area", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black"))

#Get county areas, merge with harvested area, and calculate proportion
county_borders <- counties(cb = FALSE, resolution = "500k") %>%
  st_transform(., crs = "+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

counties <- data.frame(fips = paste0(county_borders$STATEFP, county_borders$COUNTYFP), 
                   county_area = st_area(county_borders))

prop_area_cultivated <- merge(total_area, counties, by = "fips", all = T) %>%
  mutate(prop_area = as.numeric(Value/(county_area*0.000247105))) %>%
  filter(prop_area < 1 | is.na(prop_area))
#There is one county and one year for which the proportion is great than 1; it was removed

#Plot proportion of county area occupied by harvested area
plot_usmap(data = prop_area_cultivated, values = "prop_area", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Harvested area/county area", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~year)

write.csv(prop_area_cultivated, file = "county average harvested area.csv")

#####State-level proportion acreage irrigated; stability model covariate#####
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

#Pull irrigated area from census years: the FRIS years are also included here. 
#It seems best to have paired years for irrigated and non-irrigated area, so I excluded 2013 and 2018 for now
irrigated_area <- nassqs(short_desc = "AG LAND, CROPLAND, HARVESTED, IRRIGATED - ACRES", 
                     domain_desc = "TOTAL", 
                     agg_level_desc = "STATE", 
                     source_desc = "CENSUS",
                     year = c(1981:2021)) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  rename(fips = state_ansi) %>%
  filter(year %in% c(1997, 2002, 2007, 2012, 2017)) %>%
  rename(irr_area = Value)

#Replace NA with 0 
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

#Export csv for manual entry of 1982, 1987, and 1992 data
#write.csv(proportion_irrigated, file = "irrigated_area.csv")

#Load in completed data, add fips codes, add decades, calculate proportions
irr_all <- read.csv("./Climate and irrigation aggregation/irrigated_area_all.csv") %>%
  mutate(fips = fips(state_name)) %>%
  mutate(decade = ifelse(.$year %in% c(2002, 2007), "2000_2009", "2010_2019")) %>%
  mutate(decade = ifelse(.$year %in% c(1992, 1997), "1990_1999", .$decade)) %>%
  mutate(decade = ifelse(.$year %in% c(1982, 1987), "1980_1989", .$decade)) %>%
  mutate(prop_irr = irr_area/crop_area)

#Plot timeseries for each state to check for any unusual values; looks pretty consistent over time!
ggplot(irr_all, aes(x = year, y = prop_irr)) + 
  geom_point() + geom_line() + facet_wrap(~state_name)

ggplot(irr_all, aes(x = year, y = irr_area)) + 
  geom_point() + geom_line() + facet_wrap(~state_name)

ggplot(irr_all, aes(x = year, y = crop_area)) + 
  geom_point() + geom_line() + facet_wrap(~state_name)
  
#Save (aggregate to decade later if desired)
write.csv(irr_all, file = "irrigation_model_input.csv")



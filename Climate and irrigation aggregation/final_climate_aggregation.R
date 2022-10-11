#Aggregate climate data, only including months with mean min temperature > 0
#Output mean, sd, and instability of ppt and temperature by state and decade

library(tidyverse)
library(patchwork)
library(usmap)

#Load prepared input files: these are monthly average PRISM data that have been weighted by harvested area at the grid cell level
ppt <- read.csv("./Climate and irrigation aggregation/prism_state_level_harvested_area_weighted_ppt_1981_2020_20Apr2022.csv") %>%
  select(-X) %>%
  mutate(year = substr(.$time, 1, 4), 
         month = substr(.$time, 6, 7))

tavg <- read.csv("./Climate and irrigation aggregation/prism_state_level_harvested_area_weighted_tavg_1981_2020_20Apr2022.csv") %>%
  select(-X) %>%
  mutate(year = substr(.$time, 1, 4), 
         month = substr(.$time, 6, 7))

tmin <- read.csv("./Climate and irrigation aggregation/prism_state_level_harvested_area_weighted_tmin_1981_2020_20Apr2022.csv") %>%
  select(-X) %>%
  mutate(year = substr(.$time, 1, 4), 
         month = substr(.$time, 6, 7))

#Merge ppt and tavg data, filter to only include months > 0
clim <- merge(ppt, tavg, by = c("state_id", "time", "state_abb", "year", "month")) 
clim <- merge(clim, tmin, by = c("state_id", "time", "state_abb", "year", "month")) %>%
  filter(m_prism_tmin > 0) 

#Check the total number of months included for each state (all months, all years = 480, warm places should be > cold places)
check <- aggregate(m_prism_ppt~state_abb, clim, NROW)

#Calculate average ppt and tavg grouped by state and year (monthly averages) and add decades
clim_agg <- aggregate(cbind(m_prism_ppt, m_prism_tavg)~state_id + year + state_abb, clim, mean) %>%
  mutate(decade = ifelse(.$year %in% c(2000:2009), "2000_2009", "2010_2019")) %>%
  mutate(decade = ifelse(.$year %in% c(1990:1999), "1990_1999", .$decade)) %>%
  mutate(decade = ifelse(.$year %in% c(1980:1989), "1980_1989", .$decade)) %>%
  mutate(fips = fips(state_abb)) %>%
  select(-state_id)

#Calculate mean and standard deviations by state and decade; calculate instability as the CV (sd/mean)
clim_output <- clim_agg %>%
  group_by(fips, decade, state_abb) %>%
  summarize(mean_ppt = mean(m_prism_ppt), mean_tavg = mean(m_prism_tavg), 
         sd_ppt = sd(m_prism_ppt), sd_tavg = sd(m_prism_tavg)) %>%
  select(fips, state_abb, decade, mean_ppt, mean_tavg, sd_ppt, sd_tavg) %>%
  ungroup() %>%
  distinct() %>%
  mutate(ppt_instability_cv = (sd_ppt/mean_ppt), tavg_instability_cv = (sd_tavg/mean_tavg))
  
#Plot precip data to see if it makes sense 
plot_usmap(data = clim_output, values = "mean_ppt", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Mean precip", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~decade)

#Plot temp data to see if it makes sense 
plot_usmap(data = clim_output, values = "mean_tavg", size = 0.1, exclude = c("HI", "AK")) +
  scale_fill_continuous(low = "white", high = "#2d4d68", 
                        name = "Mean tavg", label = scales::comma, 
                        na.value = "grey") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) + 
  facet_wrap(~decade)

write.csv(clim_output, "./Data/Inputs/Model_covariates/decadal_climate_model_input.csv")
write.csv(clim_agg, "./Data/Inputs/Model_covariates/annual_climate_model_input.csv")


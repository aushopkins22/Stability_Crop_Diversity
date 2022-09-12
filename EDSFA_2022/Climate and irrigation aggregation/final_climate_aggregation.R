#Aggregate climate data, only including months with mean min temperature > 0
#Output mean, sd, and instability (calculated two ways -mean/sd) and sd/mean) of ppt and temperature by state and decade

library(tidyverse)
library(patchwork)

#Load input files Kyoung prepared; these are monthly average PRISM data that have been weighted by harvested area
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

#Check number of months per year for each state
#check <- aggregate(month~year + state_abb, ppt, NROW)
#check <- aggregate(month~year + state_abb, tavg, NROW)

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

#Calculate mean and standard deviations by state and decade; calculate instability using both the renard and tilman method (-mean/sd) and as the CV (sd/mean)
clim_output <- clim_agg %>%
  group_by(fips, decade) %>%
  mutate(mean_ppt = mean(m_prism_ppt), mean_tavg = mean(m_prism_tavg), 
         sd_ppt = sd(m_prism_ppt), sd_tavg = sd(m_prism_tavg)) %>%
  select(fips, state_abb, decade, mean_ppt, mean_tavg, sd_ppt, sd_tavg) %>%
  distinct() %>%
  mutate(ppt_instability_rt = -1*(mean_ppt/sd_ppt), tavg_instability_rt = -1*(mean_tavg/sd_tavg), 
         ppt_instability_cv = (sd_ppt/mean_ppt), tavg_instability_cv = (sd_tavg/mean_tavg)) 
  
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

#plot different instability metrics; we will want to choose one method
a <- ggplot(clim_output, aes(x = ppt_instability_rt)) + geom_histogram()
b <- ggplot(clim_output, aes(x = ppt_instability_cv)) + geom_histogram()

c <- ggplot(clim_output, aes(x = tavg_instability_rt)) + geom_histogram()
d <- ggplot(clim_output, aes(x = tavg_instability_cv)) + geom_histogram()

(a|b)/(c|d)

write.csv(clim_output, "decadal_climate_model_input.csv")
write.csv(clim_agg, "annual_climate_model_input.csv")


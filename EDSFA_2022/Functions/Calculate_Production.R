### Function designed to take in the converted NASS data and the output from the residuals calculation and calculate the yield and production stability for USD and kcal over a given timeframe
### Updated: 7/20/2022
### Last change author: Avery
####################################

Calculate_Production <- function(nass.data, timeframe){
  
  # Calculate annual and rolling average production over the specified time frame
  production <- nass.data %>%
    group_by(State_Abbr, fips, Year) %>%
    summarize(ann_kcal_prod = sum(Production_kcal, na.rm = T), 
              ann_usd_prod = sum(Production_USD, na.rm = T), 
              ann_area = sum(Crop_Area_ha, na.rm = T)) %>% # First calculate annual average kcal and USD production and area
    group_by(State_Abbr, fips) %>%
    arrange(Year, .by_group = T) %>% # Then regroup by state, arrange by year, and calculate the rolling mean
    mutate(rollmean_kcal_prod = rollmean(ann_kcal_prod, timeframe, align = "right", fill = NA, na.rm = T), 
           rollmean_usd_prod = rollmean(ann_usd_prod, timeframe, align = "right", fill = NA, na.rm = T)) %>%
    ungroup()
  
  # Calculate annual and rolling average yields over the specified timeframe
  kcal_yield <- nass.data %>%
    filter(!is.na(Production_kcal)) %>% # Remove rows not associated with caloric production
    group_by(State_Abbr, fips, Year) %>%
    summarize(cropland_kcal = sum(Crop_Area_ha, na.rm = T), 
              kcal_prod = sum(Production_kcal, na.rm = T)) %>%
    mutate(ann_kcal_yield = kcal_prod/cropland_kcal) %>%
    group_by(State_Abbr, fips) %>%
    arrange(Year, .by_group = T) %>% # Then regroup by state, arrange by year, and calculate the rolling mean
    mutate(rollmean_kcal_yield = rollmean(ann_kcal_yield, timeframe, align = "right", fill = NA, na.rm = T)) %>%
    ungroup() %>%
    select(State_Abbr, fips, Year, ann_kcal_yield, rollmean_kcal_yield)
  
  usd_yield <- nass.data %>%
    filter(!is.na(Production_USD)) %>% # Remove rows not associated with usd production
    group_by(State_Abbr, fips, Year) %>%
    summarize(cropland_usd = sum(Crop_Area_ha, na.rm = T), 
              usd_prod = sum(Production_USD, na.rm = T)) %>%
    mutate(ann_usd_yield = usd_prod/cropland_usd) %>%
    group_by(State_Abbr, fips) %>%
    arrange(Year, .by_group = T) %>% # Then regroup by state, arrange by year, and calculate the rolling mean
    mutate(rollmean_usd_yield = rollmean(ann_usd_yield, timeframe, align = "right", fill = NA, na.rm = T)) %>%
    ungroup() %>%
    select(State_Abbr, fips, Year, ann_usd_yield, rollmean_usd_yield)
  
  annual_means <- left_join(production, usd_yield) %>%
    left_join(., kcal_yield)
  
  return(annual_means)
}


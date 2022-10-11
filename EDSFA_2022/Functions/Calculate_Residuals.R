# Function designed to take in the converted NASS data and return residuals of regressions of 
#cropland area and USD and caloric production and yield by a quadratic function of year.

Calculate_Residuals <- function(input.file){
  
  ### Checking to make sure that the input file matches what the function needs to run.
  
  colnames_check <- colnames(input.file)
  
  ## These are the required column names.
  
  colnames_true <- c("State_Abbr",
                     "fips",
                     "Year",
                     "Crop_Name",
                     "Crop_Area_ha",
                     "Production_kg",
                     "Production_kcal",
                     "Production_USD")
  
  ## If the input file is right, all the values in check should be TRUE. If they're not, the function will stop and ask to check the input.data.
  
  check <- colnames_check %in% colnames_true
  
  if(FALSE %in% check) {
    
    stop("These data are not set up the right way. Check your column names.")
  }
  
  ###
  
  #Sum area and production by state and year and calculate yields
  total_production <- input.file %>%
    group_by(State_Abbr, fips, Year) %>%
    summarize(total_cropland = sum(Crop_Area_ha, na.rm = T), 
              kcal_prod = sum(Production_kcal, na.rm = T), 
              usd_prod = sum(Production_USD, na.rm = T)) %>%
    mutate(Year = as.numeric(Year), 
           Year2 = as.numeric(Year)^2) %>% # Add a quadratic term for regression
    group_by(State_Abbr, fips) # Group by state to drop the year grouping
  
  #Calculate the residuals for area, kcal production, and usd production. Retaining area to get an indicator of area instability.
  residuals_area <- total_production %>%
    do(augment(lm(total_cropland~Year + Year2, data = ., na.action = na.exclude))) %>% # Run the linear regression for each state, store the residuals
    rename(resid_area = .resid, 
           st_resid_area = .std.resid) %>% # Rename to enable join
    select(State_Abbr, fips, Year, resid_area, st_resid_area) %>%
    ungroup()
  
  residuals_usd <- total_production %>%
    do(augment(lm(usd_prod~Year + Year2, data = ., na.action = na.exclude))) %>% # Run the linear regression for each state, store the residuals
    rename(resid_usd_prod = .resid, 
           st_resid_usd_prod = .std.resid) %>% # Rename to enable join
    select(State_Abbr, fips, Year, resid_usd_prod, st_resid_usd_prod) %>%
    ungroup()
  
  residuals_kcal <- total_production %>%
    do(augment(lm(kcal_prod~Year + Year2, data = ., na.action = na.exclude))) %>% # Run the linear regression for each state, store the residuals
    rename(resid_kcal_prod = .resid, 
           st_resid_kcal_prod = .std.resid) %>% # Rename to enable join
    select(State_Abbr, fips, Year, resid_kcal_prod, st_resid_kcal_prod) %>%
    ungroup()
  
  ## Calculate kcal and USD yields based on the associated area
  kcal_yield <- input.file %>%
    filter(!is.na(Production_kcal)) %>% # Remove rows not associated with caloric production
    group_by(State_Abbr, fips, Year) %>%
    summarize(cropland_kcal = sum(Crop_Area_ha, na.rm = T), 
              kcal_prod = sum(Production_kcal, na.rm = T)) %>%
    mutate(kcal_yield = kcal_prod/cropland_kcal) %>%
    mutate(Year = as.numeric(Year), 
           Year2 = as.numeric(Year)^2) %>% # Add a quadratic term for regression
    group_by(State_Abbr, fips) %>% # Group by state to drop the year grouping
    select(-kcal_prod, -cropland_kcal)
  
  usd_yield <- input.file %>%
    filter(!is.na(Production_USD)) %>% # Remove rows not associated with usd production
    group_by(State_Abbr, fips, Year) %>%
    summarize(cropland_usd = sum(Crop_Area_ha, na.rm = T), 
              usd_prod = sum(Production_USD, na.rm = T)) %>%
    mutate(usd_yield = usd_prod/cropland_usd) %>%
    mutate(Year = as.numeric(Year), 
           Year2 = as.numeric(Year)^2) %>% # Add a quadratic term for regression
    group_by(State_Abbr, fips) %>% # Group by state to drop the year grouping
    select(-usd_prod, -cropland_usd)
  
  #Calculate residuals for kcal and usd yields
  residuals_usd_yield <- usd_yield %>%
    do(augment(lm(usd_yield~Year + Year2, data = ., na.action = na.exclude))) %>% # Run the linear regression for each state, store the residuals
    rename(resid_usd_yield = .resid, 
           st_resid_usd_yield = .std.resid) %>% # Rename to enable join
    select(State_Abbr, fips, Year, resid_usd_yield, st_resid_usd_yield) %>%
    ungroup()
  
  residuals_kcal_yield <- kcal_yield %>%
    do(augment(lm(kcal_yield~Year + Year2, data = ., na.action = na.exclude))) %>% # Run the linear regression for each state, store the residuals
    rename(resid_kcal_yield = .resid, 
           st_resid_kcal_yield = .std.resid) %>% # Rename to enable join
    select(State_Abbr, fips, Year, resid_kcal_yield, st_resid_kcal_yield) %>%
    ungroup()
  
  #Join residuals for all response variables
  residuals <- left_join(residuals_area, residuals_usd) %>%
    left_join(., residuals_kcal) %>%
    left_join(., residuals_usd_yield) %>%
    left_join(., residuals_kcal_yield)
  
  return(residuals)
}

### Function designed calculate the Shannon's diversity index and the effective number of cropping species (ENCS) for the United States based on NASS data.
### Updated: 7/1/2022
### Last change author: Avery
####################################

Calculate_Shannons_Diversity <- function(input.file){

  ## Checking to make sure that the input file matches what the function needs to run.
  
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
  
  #Calculate annual total area and productions
  totals <- input.file %>%
    group_by(State_Abbr, fips, Year) %>%
    summarize(total_cropland = sum(Crop_Area_ha, na.rm = T), 
              total_kcal = sum(Production_kcal, na.rm = T), 
              total_usd = sum(Production_USD, na.rm = T)) %>%
    ungroup()
  
  #Join totals with crop-specific observations, calculate pi*ln(pi) for each variable
  calculate_crop_vals_for_shannons <- left_join(input.file, totals) %>%
    group_by(Crop_Name, State_Abbr, fips, Year) %>%
    summarize(Pi_ln_Pi_area = (Crop_Area_ha/total_cropland)*log(Crop_Area_ha/total_cropland), 
           Pi_ln_Pi_usd = (Production_USD/total_usd)*log(Production_USD/total_usd), 
           Pi_ln_Pi_kcal = (Production_kcal/total_kcal)*log(Production_kcal/total_kcal)) %>%
    mutate(Pi_ln_Pi_area = replace_na(Pi_ln_Pi_area, 0), 
           Pi_ln_Pi_usd = replace_na(Pi_ln_Pi_usd, 0),
           Pi_ln_Pi_kcal = replace_na(Pi_ln_Pi_kcal, 0)) %>%
    ungroup()
  
  #Summarize to state-level annual values of Shannons and ENCS for each variable
  calculate_shannons <- calculate_crop_vals_for_shannons %>%
    group_by(State_Abbr, fips, Year) %>%
    summarize(Shannons_area = -1*sum(Pi_ln_Pi_area, na.rm = T), 
              Shannons_usd = -1*sum(Pi_ln_Pi_usd, na.rm = T),
              Shannons_kcal = -1*sum(Pi_ln_Pi_kcal, na.rm = T)) %>%
    mutate(ENCS_area = exp(Shannons_area), 
           ENCS_usd = exp(Shannons_usd), 
           ENCS_kcal = exp(Shannons_kcal))
  
}
    

# Function designed to take in the converted NASS data and the output from the residuals calculation and 
# calculate the yield and production stability for USD and kcal over a given timeframe


Calculate_Stability <- function(production, residuals, diversity, timeframe){
  
  # Prepare production/yield and residual data for stability calculation (stability = mean/residual SD)
  stability.input.df <- left_join(production, residuals) %>%
    select(State_Abbr, fips, Year, ann_kcal_prod, ann_usd_prod, ann_kcal_yield, ann_usd_yield, 
           resid_kcal_prod, resid_usd_prod, resid_kcal_yield, resid_usd_yield, ann_area, resid_area)
  
  # Calculate the standard deviation of all four sets of residuals and the mean of all four sets of production/yield data (using the {timeframe} years prior to each year), then calculate mean/SD 
  stability <- stability.input.df %>%
    group_by(fips, State_Abbr) %>%
    # calculate rolling means and SDs
    mutate(rm_kcal_prod = rollapply(ann_kcal_prod, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T), 
           sd_kcal_prod_resid = rollapply(resid_kcal_prod, timeframe, align = "right", fill = NA, FUN = sd, na.rm = T), 
           rm_kcal_yield = rollapply(ann_kcal_yield, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T), 
           sd_kcal_yield_resid = rollapply(resid_kcal_yield, timeframe, align = "right", fill = NA, FUN = sd, na.rm = T), 
           rm_usd_prod = rollapply(ann_usd_prod, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T), 
           sd_usd_prod_resid = rollapply(resid_usd_prod, timeframe, align = "right", fill = NA, FUN = sd, na.rm = T), 
           rm_usd_yield = rollapply(ann_usd_yield, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T), 
           sd_usd_yield_resid = rollapply(resid_usd_yield, timeframe, align = "right", fill = NA, FUN = sd, na.rm = T), 
           rm_area = rollapply(ann_area, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T), 
           sd_area_resid = rollapply(resid_area, timeframe, align = "right", fill = NA, FUN = sd, na.rm = T)) %>%
    # calculate stability
    mutate(stability_kcal_prod = rm_kcal_prod/sd_kcal_prod_resid,  
           stability_kcal_yield = rm_kcal_yield/sd_kcal_yield_resid, 
           stability_usd_prod = rm_usd_prod/sd_usd_prod_resid, 
           stability_usd_yield = rm_usd_yield/sd_usd_yield_resid, 
           instability_area = sd_area_resid/rm_area) %>%
    ungroup()
  
  # Merge with diversity
  diversity_stability <- left_join(stability, diversity) %>%
    # calculate rolling mean diversity
    mutate(rm_shannons_area = rollapply(Shannons_area, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T),
           rm_shannons_kcal = rollapply(Shannons_kcal, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T),
           rm_shannons_usd = rollapply(Shannons_usd, timeframe, align = "right", fill = NA, FUN = mean, na.rm = T))

  
  return(diversity_stability)
}


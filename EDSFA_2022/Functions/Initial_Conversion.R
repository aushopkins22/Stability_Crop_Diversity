### Convert NASS data to kg and then clean
### Last modified: 7/22/2022

Initial_Conversion <- function(input.data,
                               kcal.conversion.data, 
                               price.conversion.data){
  
  ## Start by converting production data to kg
  # Lemon box weights are consistent across states, so they are not treated separately here like the rest of the citrus
  
  price_removed <- input.data %>%
    select(-Price_Received_USD, -Price_Received_Units)
  
  commodity_production_kg <- left_join(price_removed, 
                                       kcal.conversion.data, 
                                       by=c("Crop", 
                                            "Crop_Utilization", 
                                            "Crop_Class",
                                            "Crop_Production",
                                            "Production_Units")) %>%
    mutate(Production_Weight = as.numeric(Production_Weight), 
           Conversion_to_kg = as.numeric(Conversion_to_kg), 
           # Create new column for crops w/ multiple production units. 
           Conversion_to_kg_citrus = case_when(Crop=="GRAPEFRUIT" & State_Abbr=="CA" ~ 36.2874,
                                               Crop=="GRAPEFRUIT" & State_Abbr=="AZ" ~ 36.2874,
                                               Crop=="GRAPEFRUIT" & State_Abbr=="FL" ~ 38.5554,
                                               Crop=="GRAPEFRUIT" & State_Abbr=="TX" ~ 36.2874,
                                               Crop=="ORANGES" & State_Abbr=="CA" ~ 36.2874,
                                               Crop=="ORANGES" & State_Abbr=="FL" ~ 40.8233,
                                               Crop=="ORANGES" & State_Abbr=="TX" ~ 38.5554,
                                               Crop=="TANGELOS" & State_Abbr=="CA" ~ 36.2874,
                                               Crop=="TANGELOS" & State_Abbr=="FL" ~ 43.0913,
                                               Crop=="TANGERINES" & State_Abbr=="CA" ~ 36.2874,
                                               Crop=="TANGERINES" & State_Abbr=="AZ" ~ 36.2874,
                                               Crop=="TANGERINES" & State_Abbr=="FL" ~ 43.0913)) %>%
    mutate(Conversion_to_kg = coalesce(Conversion_to_kg, Conversion_to_kg_citrus)) %>%
    mutate(Production_kg = Conversion_to_kg * Production_Weight) %>%
    select(-Production_Weight, -Production_Units, -Conversion_to_kg, -Conversion_to_kg_citrus)
  
  ## Print a warning message if there are different numbers of rows in the output data than the input data
  
  if(nrow(input.data) != nrow(commodity_production_kg)) {
    message(c("Number of rows differs between input data and converted production data. Check for duplicates in dataframes or missing units in conversions sheet.")) }
  
  ## Convert price received to $/kg
  
  production_removed <- input.data %>%
    select(-Production_Weight, -Production_Units)
  
  commodity_price_kg <- left_join(production_removed, 
                                  price.conversion.data, 
                                  by=c("Crop", 
                                       "Crop_Utilization", 
                                       "Crop_Class",
                                       "Crop_Production",
                                       "Price_Received_Units")) %>%
    mutate(Price_Received_USD = as.numeric(Price_Received_USD), 
           Conversion_usd_per_kg = as.numeric(Conversion_usd_per_kg), 
           Conversion_citrus = case_when(Crop=="GRAPEFRUIT" & State_Abbr=="CA" ~ 1/36.2874,
                                         Crop=="GRAPEFRUIT" & State_Abbr=="AZ" ~ 1/36.2874,
                                         Crop=="GRAPEFRUIT" & State_Abbr=="FL" ~ 1/38.5554,
                                         Crop=="GRAPEFRUIT" & State_Abbr=="TX" ~ 1/36.2874,
                                         Crop=="ORANGES" & State_Abbr=="CA" ~ 1/36.2874,
                                         Crop=="ORANGES" & State_Abbr=="FL" ~ 1/40.8233,
                                         Crop=="ORANGES" & State_Abbr=="TX" ~ 1/38.5554,
                                         Crop=="TANGELOS" & State_Abbr=="CA" ~ 1/36.2874,
                                         Crop=="TANGELOS" & State_Abbr=="FL" ~ 1/43.0913,
                                         Crop=="TANGERINES" & State_Abbr=="CA" ~ 1/36.2874,
                                         Crop=="TANGERINES" & State_Abbr=="AZ" ~ 1/36.2874,
                                         Crop=="TANGERINES" & State_Abbr=="FL" ~ 1/43.0913)) %>%
    mutate(Conversion_usd_per_kg = coalesce(Conversion_usd_per_kg, Conversion_citrus)) %>%
    mutate(Price_Received_USD_kg = Conversion_usd_per_kg * Price_Received_USD) %>%
    select(-Price_Received_USD, -Price_Received_Units, -Conversion_usd_per_kg, -Conversion_citrus)
  
  
  if(nrow(input.data) != nrow(commodity_price_kg)) {
    message(c("Number of rows differs between input data and converted price data. Check for duplicates in dataframes or missing units in conversions sheet.")) }
  
  ## Fill the price data with the average across states that have data, keeping utilization practices separate.
  commodity_price_kg_filled <- commodity_price_kg %>%
    group_by(Year, Crop, Crop_Class, Crop_Utilization, Crop_Production) %>%
    mutate(national_price = mean(Price_Received_USD_kg, na.rm = T)) %>%
    ungroup() %>%
    mutate(Price_Received_USD_kg = ifelse(is.na(.$Price_Received_USD_kg), 
                                          .$national_price, 
                                          .$Price_Received_USD_kg)) %>%
    mutate(Price_Received_USD_kg = na_if(Price_Received_USD_kg, "NaN")) %>%
    select(-national_price)
  
  
  ## Merge converted data
  commodity_kg <- left_join(commodity_production_kg, commodity_price_kg_filled) %>%
    mutate(Crop_Name = ifelse(.$Crop_Utilization %in% c("SILAGE", "GRAIN"),  
                              paste0(.$Crop, "-", .$Crop_Class, "-", .$Crop_Utilization), 
                              paste0(.$Crop, "-", .$Crop_Class)))
  
  ## Clean the converted data: 
  # If there are multiple utilization practices associated with price and production data, take the production-weighted average of price data across utilization practices.
  # Then add areas and production across crop classes, production practices, and utilization practices. 
  multiple.util.practices <- commodity_kg %>%
    group_by(State_Abbr, Year, Crop_Name) %>%
    filter(!is.na(Production_kg)) %>%
    filter(!is.na(Price_Received_USD_kg)) %>%
    mutate(count_util = n_distinct(Crop_Utilization)) %>%
    filter(count_util > 1) %>%
    summarise(Price_Received_USD_kg = weighted.mean(Price_Received_USD_kg, Production_kg, na.rm = T),
              Crop_Area_ha = sum(Crop_Area_ha, na.rm = T),
              Production_kg = sum(Production_kg, na.rm = T), 
              kcal_kg = mean(kcal_kg, na.rm = T)) %>% # All these crops have the same kcal values across utilizations
    mutate(Unique_ID = paste0(State_Abbr, Year, Crop_Name)) %>%
    ungroup()
  
  
  single.util.practice <- commodity_kg %>%
    mutate(Unique_ID = paste0(State_Abbr, Year, Crop_Name)) %>%
    filter(!(Unique_ID %in% c(multiple.util.practices$Unique_ID))) %>%
    group_by(State_Abbr, Year, Crop_Name) %>%
    summarise(Price_Received_USD_kg = mean(Price_Received_USD_kg, na.rm = T), # If production is zero for all utilizations, this will become NaN
              Crop_Area_ha = sum(Crop_Area_ha, na.rm = T),
              Production_kg = sum(Production_kg, na.rm = T), 
              kcal_kg = mean(kcal_kg, na.rm = T)) %>%
    ungroup()
  
  multiple.util.practices <- multiple.util.practices %>%
    select(-Unique_ID)
  
  clean.data <- rbind(multiple.util.practices, single.util.practice) %>%
    mutate(FIPS = fips(.$State_Abbr)) %>%
    mutate(Crop_Area_ha = na_if(Crop_Area_ha, 0),
           Production_kg = na_if(Production_kg, 0), 
           Price_Received_USD_kg = na_if(Price_Received_USD_kg, "NaN"), 
           Price_Received_USD_kg = na_if(Price_Received_USD_kg, 0))
  
  return(clean.data)
  
}
  

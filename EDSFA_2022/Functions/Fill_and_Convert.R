### Gap filling function:
### Last modified: 9/6/2022
  ## Production and area data filled by linear interpolation
  ## Price data are filled by the average of other states in that year
  ## After filling, converts production to USD and kcal and incorporates the ppi correction

Fill_and_Convert <- function(input.data,
                             year.tolerance,
                             ppi.data){
  
  kcal.vals <- input.data %>%
    select(Crop_Name, kcal_kg) %>%
    filter(kcal_kg != "NaN") %>%
    distinct() %>%
    filter(!(Crop_Name == "ALMONDS-ALL CLASSES" & kcal_kg == 2316)) # all almond production is "shelled", so remove the unshelled value
    
  
  filled.data <- input.data %>%
                  select(-kcal_kg) %>%
                  group_by(Crop_Name, State_Abbr) %>%
                  mutate(Crop_Area_ha = na.approx(object = Crop_Area_ha,
                                                  na.rm = FALSE,
                                                  maxgap = year.tolerance,
                                                  rule = 1)) %>%
                  mutate(Production_kg = na.approx(object = Production_kg,
                                                   na.rm = FALSE,
                                                   maxgap = year.tolerance,
                                                   rule = 1)) %>%
                  ungroup() %>% #ungroup by state-crop to regroup by year-crop for the price fill
                  group_by(Year, Crop_Name) %>%
                  mutate(national_price = mean(Price_Received_USD_kg, na.rm = T)) %>%
                  ungroup() %>%
                  mutate(Price_Received_USD_kg = ifelse(is.na(.$Price_Received_USD_kg), 
                                                        .$national_price, 
                                                        .$Price_Received_USD_kg)) %>%
                  mutate(Price_Received_USD_kg = na_if(Price_Received_USD_kg, "NaN"))
    
  
  ## Convert filled kg and price data to USD and kcal production
  
  filled.converted <- filled.data %>%
                        left_join(., kcal.vals) %>%
                        mutate(Production_kcal = Production_kg * kcal_kg,
                               Production_USD = Production_kg * Price_Received_USD_kg)
  
  ## Adjust price based on PPI
  
  total.ppi <- ppi.data %>%
                rename(Year = year) %>%
                mutate(Year = as.character(Year))
  
  final.data <- left_join(filled.converted, 
                          total.ppi) %>%
                mutate(Production_USD = .$Production_USD * as.numeric(total.ppi[total.ppi$Year == "2010", 'value'])/.$value) %>%
                select(-value,
                       -series_id,
                       -period,
                       -national_price) %>%
                filter(!(is.na(Production_USD) & is.na(Production_kcal)))
  
  
  return(final.data)
  
}
  

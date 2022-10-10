# Function designed to call in NASS data on yield and acreage for the all states in the period between 1981 and 2021

Call_NASS_Data <- function(Crop, 
                           Crop_Class, 
                           Crop_Utilization,
                           Crop_Production,
                           Price_Data,
                           Area_Data,
                           Start_Year,
                           End_Year){ 
  
  ## Print the crop that the function is working on. 
  
  percent.done <- round(((i/124)*100),3)
  
  print(paste0("Now working on: ", Crop, " (", percent.done, "% complete)"))
  
  ## Set up a dataframe with a row for every state in each year from Start_Year to End_Year.
  
  suppressMessages(state.names <- as.data.frame(states()))
  
  ## Filter out the non-continental US
  
  state.names <- state.names %>%
    select(STUSPS, NAME) %>%
    filter(!NAME %in% c("Alaska", 
                        "Hawaii",
                        "United States Virgin Islands",
                        "Commonwealth of the Northern Mariana Islands",
                        "American Samoa",
                        "District of Columbia",
                        "Puerto Rico",
                        "Guam"))
  
  crop.data.proto <- data.frame(State_Abbr = rep(state.names$STUSPS, times = (End_Year - Start_Year + 1)),
                                State_Name = rep(state.names$NAME, times = (End_Year - Start_Year + 1)),
                                Year = rep(seq(Start_Year, End_Year), each = length(state.names$NAME)),
                                Crop = as.character(Crop),
                                Crop_Class = as.character(Crop_Class),
                                Crop_Utilization = as.character(Crop_Utilization), 
                                Crop_Production = as.character(Crop_Production))
  
  #Pull survey area for all crops
  
  area.params <- list(commodity_desc = as.character(Crop), 
                      class_desc = as.character(Crop_Class),
                      util_practice_desc = as.character(Crop_Utilization),
                      unit_desc = "ACRES",
                      domain_desc = "TOTAL",
                      source_desc = "SURVEY",
                      domaincat_desc = "NOT SPECIFIED",
                      agg_level_desc = "STATE",
                      reference_period_desc = "YEAR",
                      prodn_practice_desc = as.character(Crop_Production),
                      year = Start_Year:End_Year)
  
  ## Check the number of observations.
  
  obs.number <- as.numeric(nassqs_record_count(area.params)[1])
  
  print(paste0("Getting area data for ", Crop, " - ", Crop_Class, " - ", Crop_Utilization, " (", obs.number, " observations)"))
  
  ## Call to the NASS API and get the area data for that crop in the specified time period. 
  
  area.data <- if(obs.number > 0) {
    nassqs(area.params,
           httr::timeout(5)) %>%
      filter(statisticcat_desc %in% c("AREA HARVESTED", "AREA BEARING")) %>%
      mutate(Value = gsub(",", "", Value)) %>% # NASS delivers everything with commas, this takes them out.
      select(c(state_alpha, year, Value, statisticcat_desc)) %>% # Subsetting
      rename(State_Abbr = state_alpha) %>% # Renaming
      rename(Area_Class = statisticcat_desc) %>%
      mutate(Year = as.numeric(year)) %>% # Changing type
      mutate(Crop_Area_ac = as.numeric(Value)) %>% # Changing type
      mutate(Crop_Area_ha = Crop_Area_ac/2.4710) %>% # Convert to hectares
      select(c(State_Abbr, Year, Crop_Area_ha, Area_Class))} else {
        tibble(State_Abbr = crop.data.proto$State_Abbr,
               Year = crop.data.proto$Year,
               Crop_Area_ha = NA,
               Area_Class = NA) }
  
  ## For tree crops, also pull census data
  if(Area_Data == "Census"){
    
    area.params <- list(commodity_desc = as.character(Crop), 
                        class_desc = as.character(Crop_Class),
                        util_practice_desc = as.character(Crop_Utilization),
                        unit_desc = "ACRES",
                        domain_desc = "TOTAL",
                        source_desc = "CENSUS",
                        domaincat_desc = "NOT SPECIFIED",
                        agg_level_desc = "STATE",
                        reference_period_desc = "YEAR",
                        prodn_practice_desc = as.character(Crop_Production),
                        year = Start_Year:End_Year)
    
    area.call <- nassqs(area.params,
                        httr::timeout(5)) %>%
      filter(statisticcat_desc %in% c("AREA HARVESTED", "AREA BEARING")) %>% ## This needs to be fixed I think.
      mutate(Value = gsub(",", "", Value)) %>% # NASS delivers everything with commas, this takes them out.
      select(c(state_alpha, year, Value, statisticcat_desc)) %>% # Subsetting
      rename(State_Abbr = state_alpha) %>% # Renaming
      rename(Area_Class = statisticcat_desc) %>%
      mutate(Year = as.numeric(year)) %>% # Changing type
      mutate(Crop_Area_ac = as.numeric(Value)) %>% # Changing type
      mutate(Crop_Area_ha = Crop_Area_ac/2.4710) %>% # Convert to hectares
      select(c(State_Abbr, Year, Crop_Area_ha, Area_Class))
    
    #Then fill the census data
    
    area.states <- unique(area.call$State_Abbr)
    
    census.area.data <- tibble(State_Abbr = NA,
                               Year =  1981,
                               Crop_Area_ha = 0.0,
                               Area_Class = NA)
    
    for(i in 1:length(area.states)) {
      
      temp.data_census <- area.call[area.call$State_Abbr == area.states[i],]
      
      temp.years <- data.frame(Year = seq(min(temp.data_census$Year), 2020))
      
      temp.data_full <- suppressMessages(left_join(temp.years, temp.data_census)) %>%
        mutate(State_Abbr = temp.data_census$State_Abbr[1],
               Area_Class = temp.data_census$Area_Class[1],
               Crop_Area_ha = na.approx(object = Crop_Area_ha,
                                        na.rm = FALSE,
                                        maxgap = 10,
                                        rule = 2)) %>%
        select(State_Abbr, 
               Year,
               Crop_Area_ha,
               Area_Class)
      
      census.area.data <- rbind(census.area.data, 
                                temp.data_full)
      
    }
    
    census.area.data <- census.area.data[-1,] %>%
      rename(census_area = Crop_Area_ha)
    
    #Then merge census and survey data and replace missing survey values with census values
    
    area.data <- full_join(area.data, census.area.data) %>%
      mutate(Crop_Area_ha = ifelse(is.na(.$Crop_Area_ha), .$census_area, Crop_Area_ha)) %>%
      select(-census_area)
    
  } 
  
  
  ## Join the area data to the original dataframe.
  
  area.output <- suppressMessages(left_join(crop.data.proto, area.data))
  
  ## Create parameters to call to the NASS API for production values.
  
  production.params <- list(commodity_desc = as.character(Crop), 
                            class_desc = as.character(Crop_Class),
                            util_practice_desc = as.character(Crop_Utilization),
                            statisticcat_desc = "PRODUCTION",
                            domain_desc = "TOTAL",
                            source_desc = "SURVEY",
                            domaincat_desc = "NOT SPECIFIED",
                            agg_level_desc = "STATE",
                            reference_period_desc = "YEAR",
                            prodn_practice_desc = as.character(Crop_Production),
                            year = Start_Year:End_Year)
  
  ## Check how many observations are present for that crop.
  
  obs.number <- as.numeric(nassqs_record_count(production.params)[1])
  
  print(paste0("Getting production data for ", Crop, " - ", Crop_Class, " - ", Crop_Utilization, " (", obs.number, " observations)"))
  
  ## Call to NASS API to get production data. 
  
  production.data_weight <- if(obs.number > 0) {
    nassqs(production.params,
           httr::timeout(5)) %>%
      mutate(Value = gsub(",", "", Value)) %>%
      filter(unit_desc != "$") %>%
      filter(unit_desc != "$, PHD EQUIV") %>%
      filter(unit_desc != "$, ON TREE EQUIV") %>%
      select(c(state_alpha, year, Value, unit_desc)) %>%
      rename(State_Abbr = state_alpha) %>%
      rename(Production_Units = unit_desc) %>%
      mutate(Year = as.numeric(year)) %>%
      mutate(Production_Weight = as.numeric(Value)) %>%
      select(c(State_Abbr, Year, Production_Weight, Production_Units))} else {
        tibble(State_Abbr = crop.data.proto$State_Abbr,
               Year = crop.data.proto$Year,
               Production_Weight = NA,
               Production_Units = NA)}
  
  ## Create parameters to call to the NASS API for price received data.
  
  received <- ifelse(Price_Data == "Price",
                     "PRICE RECEIVED",
                     "PRODUCTION")
  
  if(received == "PRICE RECEIVED") {
    
    
    price.params.a <- list(commodity_desc = as.character(Crop), 
                           class_desc = as.character(Crop_Class),
                           util_practice_desc = as.character(Crop_Utilization),
                           statisticcat_desc = received,
                           domain_desc = "TOTAL",
                           source_desc = "SURVEY",
                           domaincat_desc = "NOT SPECIFIED",
                           agg_level_desc = "STATE",
                           reference_period_desc = "MARKETING YEAR",
                           prodn_practice_desc = as.character(Crop_Production),
                           year = Start_Year:End_Year)
    
    
    price.params.b <- list(commodity_desc = as.character(Crop), 
                           class_desc = as.character(Crop_Class),
                           util_practice_desc = as.character(Crop_Utilization),
                           statisticcat_desc = received,
                           domain_desc = "TOTAL",
                           source_desc = "SURVEY",
                           domaincat_desc = "NOT SPECIFIED",
                           agg_level_desc = "STATE",
                           reference_period_desc = "YEAR",
                           prodn_practice_desc = as.character(Crop_Production),
                           year = Start_Year:End_Year)
    
  } else {
    
    
    price.params.a <- list(commodity_desc = as.character(Crop), 
                           class_desc = as.character(Crop_Class),
                           util_practice_desc = as.character(Crop_Utilization),
                           statisticcat_desc = received,
                           domain_desc = "TOTAL",
                           source_desc = "SURVEY",
                           unit_desc = "$",
                           domaincat_desc = "NOT SPECIFIED",
                           agg_level_desc = "STATE",
                           reference_period_desc = "MARKETING YEAR",
                           prodn_practice_desc = as.character(Crop_Production),
                           year = Start_Year:End_Year)
    
    price.params.b <- list(commodity_desc = as.character(Crop), 
                           class_desc = as.character(Crop_Class),
                           util_practice_desc = as.character(Crop_Utilization),
                           statisticcat_desc = received,
                           domain_desc = "TOTAL",
                           source_desc = "SURVEY",
                           unit_desc = "$",
                           domaincat_desc = "NOT SPECIFIED",
                           agg_level_desc = "STATE",
                           reference_period_desc = "YEAR",
                           prodn_practice_desc = as.character(Crop_Production),
                           year = Start_Year:End_Year)
  }
  
  obs.number.a <- as.numeric(nassqs_record_count(price.params.a)[1])
  obs.number.b <- as.numeric(nassqs_record_count(price.params.b)[1])
  
  
  Time_Period <- ifelse(obs.number.a >= obs.number.b & obs.number.a != 0,
                        "MARKETING YEAR",
                        "YEAR")
  
  agg_level <- ifelse(obs.number.a == 0 & obs.number.b == 0 & Price_Data != "Production",
                      "NATIONAL",
                      "STATE")
  
  if(Price_Data =="Price"){
    
    
    price.params <- list(commodity_desc = as.character(Crop), 
                         class_desc = as.character(Crop_Class),
                         util_practice_desc = as.character(Crop_Utilization),
                         statisticcat_desc = received,
                         domain_desc = "TOTAL",
                         source_desc = "SURVEY",
                         domaincat_desc = "NOT SPECIFIED",
                         agg_level_desc = agg_level,
                         reference_period_desc = Time_Period,
                         prodn_practice_desc = as.character(Crop_Production),
                         year = Start_Year:End_Year)
  } else {
    
    price.params <- list(commodity_desc = as.character(Crop), 
                         class_desc = as.character(Crop_Class),
                         util_practice_desc = as.character(Crop_Utilization),
                         statisticcat_desc = received,
                         unit_desc = "$",
                         domain_desc = "TOTAL",
                         source_desc = "SURVEY",
                         domaincat_desc = "NOT SPECIFIED",
                         agg_level_desc = agg_level,
                         reference_period_desc = Time_Period,
                         prodn_practice_desc = as.character(Crop_Production),
                         year = Start_Year:End_Year)
  }  
  
  obs.number <- as.numeric(nassqs_record_count(price.params)[1])
  
  print(paste0("Getting $ data for ", Crop, " - ", Crop_Class, " - ", Crop_Utilization, " (", obs.number, " observations)"))
  
  production.data_USD <- if(obs.number > 0) {
    nassqs(price.params,
           httr::timeout(5)) %>%
      filter(unit_desc != "$ / BOX, ON TREE EQUIV") %>% 
      filter(unit_desc != "$ / TON, FRESH BASIS") %>% # Gets rid of duplicated prunes
      mutate(Value = gsub(",", "", Value)) %>%
      select(c(state_alpha, year, Value, unit_desc)) %>%
      mutate(Price_Received_USD = as.numeric(Value)) %>%
      rename(State_Abbr = state_alpha) %>%
      mutate(Year = as.numeric(year)) %>%
      mutate(Price_Received_Units = unit_desc) %>%
      select(c(State_Abbr, Year, Price_Received_USD, Price_Received_Units))} else {
        tibble(State_Abbr = crop.data.proto$State_Abbr,
               Year = crop.data.proto$Year,
               Price_Received_USD = NA,
               Price_Received_Units = NA)}
  
  
  ## Join production data together.
  
  production.output <- suppressMessages(full_join(production.data_weight, 
                                                  production.data_USD))
  
  if(Price_Data != "Price") {
    
    production.output <- production.output %>%
      mutate(Price_Received_USD = round(Price_Received_USD/Production_Weight, 2)) %>%
      mutate(Price_Received_Units = ifelse(TRUE %in% is.na(.$Price_Recieved_USD), 
                                           NA, 
                                           paste0("$/", unique(.$Production_Units)))) 
    
  }
  
  
  ## Join production data with area data.
  
  output.data <- suppressMessages(left_join(area.output, 
                                            production.output))
  
  
  ## Filtering out the years prior to initial observations. ##
  
  first.year <- output.data %>%
    filter(is.na(Crop_Area_ha) == F |
             is.na(Production_Weight) == F|
             is.na(Price_Received_USD) == F) %>%
    group_by(State_Abbr) %>%
    mutate(initial_year = min(Year)) %>%
    select(State_Abbr, initial_year) %>%
    distinct()
  
  output.data <- left_join(output.data, first.year) %>%
    filter(Year >= initial_year) %>%
    select(-initial_year)
  
  
  ## Dealing with multiple units for an individual state-crop-year ##
  
  test.for.boxes <- as.character(c("BOXES", "TONS") %in% unique(output.data$Production_Units))
  
  if(FALSE %in% test.for.boxes){
    
    NULL
    
  } else {
    
    output.data <- output.data %>%
      filter(Production_Units == "BOXES")
    
  }
  
  ## Similar fix as above but for strawberries (They only started to report tons in 2019 and 2020).
  
  test.for.strawberries <- as.character(c("CWT", "TONS") %in% unique(output.data$Production_Units))
  
  if(FALSE %in% test.for.strawberries){
    
    NULL
    
  } else {
    
    output.data <- output.data %>%
      filter(Production_Units == "CWT")
    
  }
  
  ## Similar fix as above but now for prunes.
  
  test.for.prunes <- as.character(c("$ / TON, DRY BASIS", "$ / TON") %in% unique(output.data$Price_Received_Units))
  
  if(FALSE %in% test.for.prunes){
    
    NULL
    
  } else {
    
    output.data <- output.data %>%
      filter(Price_Received_Units == "$ / TON, DRY BASIS")
    
  }
  

  if(unique(output.data$Crop) == "TANGELOS"){ 
    
    output.data <- output.data %>%
      filter(Price_Received_Units == "$ / BOX, PHD EQUIV" | is.na(Price_Received_Units))
  }
  
  ## Return the data.
  
  return(output.data)
  
}
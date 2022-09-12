NASS_Summary_Table <- function(commodity.data,
                               commodity.list){
  
  total.observations <- nrow(commodity.data)
  
  summary <- tibble(Crop = commodity.list$Crop,
                    Crop_Class = commodity.list$Crop_Class,
                    Crop_Utilization = commodity.list$Crop_Utilization,
                    Crop_Production = commodity.list$Crop_Production,
                    Area_Obs = vector(mode = "numeric", length = nrow(commodity.list)),
                    Area_States_Represented = vector(mode = "numeric", length = nrow(commodity.list)),
                    Area_Start_Year = vector(mode = "numeric", length = nrow(commodity.list)),
                    Area_End_Year = vector(mode = "numeric", length = nrow(commodity.list)),
                    Production_Obs = vector(mode = "numeric", length = nrow(commodity.list)),
                    Production_States_Represented = vector(mode = "numeric", length = nrow(commodity.list)),
                    Production_Start_Year = vector(mode = "numeric", length = nrow(commodity.list)),
                    Production_End_Year = vector(mode = "numeric", length = nrow(commodity.list)),
                    Production_Units = vector(mode = "character", length = nrow(commodity.list)),
                    Price_Start_Year = vector(mode = "numeric", length = nrow(commodity.list)),
                    Price_End_Year = vector(mode = "numeric", length = nrow(commodity.list)),
                    Price_States_Represented = vector(mode = "numeric", length = nrow(commodity.list)),
                    Price_Obs = vector(mode = "numeric", length = nrow(commodity.list)),
                    Price_Units = vector(mode = "character", length = nrow(commodity.list)),
                    Mismatch = vector(mode = "character", length = nrow(commodity.list)))
  
  for(i in 1:nrow(commodity.list)){
    
    if(i == round((nrow(commodity.list)/2),0)) {
      
      print("Halfway done.")
    
    }
    
    ## Area
    
    temp.data_Area <- commodity.data %>%
                        filter(Crop %in% summary$Crop[i],
                                Crop_Class %in%  summary$Crop_Class[i],
                                 Crop_Utilization %in% summary$Crop_Utilization[i],
                                  Crop_Production %in% summary$Crop_Production[i]) %>%
                        select(State_Abbr, 
                               Year,
                               Crop_Area_ha) %>%
                        filter(!is.na(Crop_Area_ha))
    
    summary$Area_Obs[i] <- nrow(temp.data_Area)
    
    summary$Area_States_Represented[i] <- length(unique(temp.data_Area$State_Abbr))
    
    if(summary$Area_Obs[i] > 0){
    
       summary$Area_Start_Year[i] <- min(temp.data_Area$Year)
    
       summary$Area_End_Year[i] <- max(temp.data_Area$Year) } else {

      summary$Area_Start_Year[i] <- NA
      
      summary$Area_End_Year[i] <- NA } 
    
    
    ## Production
    
    temp.data_Production <- commodity.data %>%
                             filter(Crop %in% summary$Crop[i],
                                      Crop_Class %in% summary$Crop_Class[i],
                                        Crop_Utilization %in% summary$Crop_Utilization[i],
                                          Crop_Production %in% summary$Crop_Production[i]) %>%
                             select(State_Abbr, 
                                    Year,
                                    Production_Weight,
                                    Production_Units) %>%
                             filter(!is.na(Production_Weight))
    
    summary$Production_Obs[i] <- nrow(temp.data_Production)
    
    summary$Production_States_Represented[i] <- length(unique(temp.data_Production$State_Abbr))
    
    ## Checking for double units 
    
    temp_Units <- unique(temp.data_Production$Production_Units)
    
    if(length(temp_Units > 1)) {
      
      temp_Units <- paste(temp_Units, sep = "", collapse = "")
      
    }
    
    if(summary$Production_Obs[i] > 0){
      
      summary$Production_Start_Year[i] <- min(temp.data_Production$Year)
      
      summary$Production_End_Year[i] <- max(temp.data_Production$Year) 
      
      summary$Production_Units[i] <- temp_Units } else {
      
      summary$Production_Start_Year[i] <- NA
      
      summary$Production_End_Year[i] <- NA 
      
      summary$Production_Units[i] <- NA } 
    
    ## Price
    
    temp.data_Price <- commodity.data %>%
                        filter(Crop %in% summary$Crop[i],
                                Crop_Class %in% summary$Crop_Class[i],
                                  Crop_Utilization %in% summary$Crop_Utilization[i],
                                    Crop_Production %in% summary$Crop_Production[i]) %>%
                        select(State_Abbr, 
                               Year,
                               Price_Received_USD,
                               Price_Received_Units) %>%
                        filter(!is.na(Price_Received_USD))
    
    summary$Price_Obs[i] <- nrow(temp.data_Price)
    
    summary$Price_States_Represented[i] <- length(unique(temp.data_Price$State_Abbr))
    
    ## Checking for double units 
    
    temp_Units <- unique(temp.data_Price$Price_Received_Units)
    
    if(length(temp_Units > 1)) {
      
      temp_Units <- paste(temp_Units, sep = "", collapse = "")
      
    }
    
    if(summary$Price_Obs[i] > 0){
      
      summary$Price_Start_Year[i] <- min(temp.data_Price$Year)
      
      summary$Price_End_Year[i] <- max(temp.data_Price$Year) 
      
      summary$Price_Units[i] <- temp_Units } else {
      
      summary$Price_Start_Year[i] <- NA
      
      summary$Price_End_Year[i] <- NA 
      
      summary$Price_Units[i] <- NA } 
    
    ## Cross comparison
    
    if(summary$Area_Obs[i] == summary$Production_Obs[i] &
       summary$Production_Obs[i] == summary$Price_Obs[i]) {
      
      summary$Mismatch[i] <- "No" } else {
      
      summary$Mismatch[i] <- "Yes"     
      
      }
    
    }
  
  print(paste("Total observations:", total.observations))
  
  return(summary)

}


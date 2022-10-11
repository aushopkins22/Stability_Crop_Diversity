## Check to be sure all data was successfully pulled for all crops in the comodity input list. 
# If crops are missing (typically due to server timeouts), retry the query. 

Check_NASS_Failures <- function(original.data,
                                generated.data){
  
  `%!in%` <- Negate(`%in%`)

  test.dataframe <- unique(generated.data[,c(4, 5, 6, 7)]) %>%
                      arrange(Crop, 
                              Crop_Class,
                              Crop_Utilization)
  
  original.data <- original.data %>%
                    arrange(Crop, 
                            Crop_Class,
                            Crop_Utilization)
  
  if(identical(original.data[,c(1,2,3,4)], test.dataframe) == FALSE) {

    missing.data <-  anti_join(original.data, test.dataframe)
 
    print(paste0("Data for ", nrow(missing.data), " Crops were not pulled from NASS."))
    print(missing.data)
 
    for (i in 1:nrow(missing.data)) {
   
      try({temp.data <- Call_NASS_Data(Crop = missing.data$Crop[i],
                                       Crop_Class = missing.data$Crop_Class[i],
                                       Crop_Utilization = missing.data$Crop_Utilization[i],
                                       Crop_Production = missing.data$Crop_Production[i],
                                       Price_Data = missing.data$Price_Data[i],
                                       Area_Data = missing.data$Area_Data[i],
                                       Start_Year = 1981,
                                       End_Year = 2020)
      
      temp.data <- temp.data %>%
                    filter(is.na(Crop_Area_ha) == F |
                    is.na(Production_Weight) == F|
                    is.na(Price_Received_USD) == F)
      
      generated.data <- rbind(generated.data, temp.data)
      
      } # End try
      ) # Close try
   } # End for loop
     
  } else {
  
  print("All data is present.")
   
  }
  
  return(generated.data)
  
} # End function

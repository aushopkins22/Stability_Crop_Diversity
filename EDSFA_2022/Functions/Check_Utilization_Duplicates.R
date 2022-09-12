#Check for duplicate utilization practices, select "all" where possible. 

Check_Utilization_Duplicates <- function(input.data){
  
  `%!in%` <- Negate(`%in%`)
  
  input.data$Unique_ID <- paste0(input.data$State_Abbr, "_",
                                 input.data$Crop, "_",
                                 input.data$Crop_Class, "_",
                                 input.data$Year)
  
  empty.data <- input.data[1,]
  
  index <- unique(input.data$Unique_ID)
  
  for(i in 1:length(index)){
    
    temp.data <- input.data %>%
      filter(Unique_ID == index[i])
    
    if("ALL UTILIZATION PRACTICES" %in% temp.data$Crop_Utilization){
      
      row.number <- which(temp.data$Crop_Utilization == "ALL UTILIZATION PRACTICES")
      
      if(is.na(temp.data$Production_Weight[row.number]) == FALSE){
        
        temp.data <- temp.data %>%
          filter(Crop_Utilization == "ALL UTILIZATION PRACTICES")
      }
    }
    
    empty.data <- rbind(empty.data, temp.data)
    
    if(i == round(length(index) * 0.25, 0)){
      print("25% done")
    }
    
    if(i ==round(length(index) * 0.50, 0)){
      print("50% done")
    }
    
    if(i == round(length(index) * 0.75, 0)){
      print("75% done")
    }
    
    if(i == round(length(index) * 0.99, 0)){
      print("Wrapping things up.")
    }
  }
  
  empty.data <- empty.data[-1,]
  
  return(empty.data)
}
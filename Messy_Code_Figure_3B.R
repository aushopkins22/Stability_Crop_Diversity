### Calculating the price elasticity of supply

Rolling_Price_Elasticity <- function(Production, Price){
  

  Numerator <- (Production[2] - Production[1]) / ((Production[2] + Production[1])/2)
  
  Denominator <- (Price[2] - Price[1]) / ((Price[2] + Price[1])/2)
  
  PE <- (Numerator/Denominator)
  
  return(PE)
  
  print(PE)
  
}


i <- 1

y <- 1

x <- 1

clean.filled.data$Year <- as.numeric(clean.filled.data$Year)

clean.filled.data$Price_Elasticity <- NA

for(i in 1:length(unique(clean.filled.data$Crop_Name))){
  
  print(c(i, unique(clean.filled.data$Crop_Name)[i]))
  
    if(i  == round(length(unique(clean.filled.data$Crop_Name))*0.25)){
   
       print("25 % Done")
    }
  
    if(i == round(length(unique(clean.filled.data$Crop_Name))*0.5)){
    
      print("50 % Done")
    }
  
    if(i == round(length(unique(clean.filled.data$Crop_Name))*0.75)){
    
      print("75 % Done")
    }
  
    if(i == round(length(unique(clean.filled.data$Crop_Name))*0.99)){
    
      print("Almost done!")
    }
  
  ##
  
  crop.data <- clean.filled.data %>% 
                filter(Crop_Name == unique(clean.filled.data$Crop_Name[i]))
  
  ##  
  
  for(y in 1:length(unique(crop.data$State_Abbr))){
    
    print(unique(crop.data$State_Abbr)[y])
    
    state.crop.data <- crop.data %>% 
                        filter(State_Abbr == crop.data$State_Abbr[y])
    
    ##
      
    for(x in 1:(length(unique(state.crop.data$Year)) - 1)){
      
        
      row_id <- which(clean.filled.data$Crop_Name == crop.data$Crop_Name[i] & 
                      clean.filled.data$State_Abbr == unique(state.crop.data$State_Abbr) &
                      clean.filled.data$Year == state.crop.data$Year[x])
        
      ##
      
      clean.filled.data$Price_Elasticity[row_id] <- Rolling_Price_Elasticity(Production = state.crop.data$Production_kg[c(x, x + 1)],
                                                                             Price = state.crop.data$Price_Received_USD_kg[c(x, x + 1)])
      
      }
    }
  }
  
clean.filled.data %>% 
  group_by(Crop_Name) %>% 
  summarise(Price_Elasticity = mean(Price_Elasticity, na.rm = T))

##############

ratios <- nass_full %>%
            group_by(Crop_Name) %>%
            summarise(Average_USD_Production = mean(Production_USD, na.rm = TRUE),
                      Average_kcal_Production = mean(Production_kcal, na.rm = TRUE),
                      Average_crop_area = mean(Crop_Area_ha, na.rm = T)) %>%
            mutate(USD = Average_USD_Production/Average_crop_area,
                   kcal = Average_kcal_Production/Average_crop_area,
                   ratio = log(kcal/USD))


annual_elasticity <- nass %>%
                      mutate(Price = Production_USD/Production_kg) %>%
                      group_by(Crop_Name, State_Abbr, fips) %>%
                      arrange(Year, .by_group = TRUE) %>%
                      mutate(pct_change_prod = Production_kg/lag(Production_kg) - 1,
                      pct_change_price = Price/lag(Price) - 1) %>%
                      mutate(supply_elasticity = pct_change_prod/pct_change_price) %>%
                      ungroup() %>%
                      left_join(., ratios)

totals <- nass %>%
            group_by(Crop_Name) %>%
            summarise(Total_Production_kg = sum(Production_kg, na.rm = T))


weighted_elasticity <- annual_elasticity %>%
                        left_join(., totals) %>%
                        mutate(Weight_kg = Production_kg/Total_Production_kg)  %>%
                        mutate(Weight_kg = ifelse(is.na(Weight_kg), 0, Weight_kg)) %>%
                        mutate(Weighted_elasticity = Weight_kg * supply_elasticity,
                        kcal_per_kg = Production_kcal/Production_kg) %>%
                        group_by(Crop_Name) %>%
                        summarize(Weighted_elasticity = abs(sum(Weighted_elasticity, na.rm = T)),
                        kcal_per_kg = max(kcal_per_kg, na.rm = T)) %>%
                        left_join(., ratios) %>%
                        mutate(elastic_categorical = ifelse(Weighted_elasticity > 1, "elastic", "inelastic"))

tests <- Percentages %>% 
          rename(USD_Stabilization = USD,
                 kcal_Destabilization = kcal) %>% 
          left_join(.,
                    weighted_elasticity,
                    by = c("Drop_Crop" = "Crop_Name"))

ggplot(data = tests,
       aes(x = elastic_categorical,
           y = ratio)) +
  geom_boxplot()

b <- ggplot(tests,
       aes(x = kcal_Destabilization,
           y = ratio, fill = kcal_Diversity)) +
  geom_point(shape = 21,
             size = 5) +
  geom_smooth(method = "lm", 
              se = F, 
              color = "black",
              lty = 2) +
  labs(tag = "(b)") +
  ylab("ln(kcal per USD)") +
  xlab("Caloric Destabilization Potential") +
  theme(plot.tag.position = c(0.13, 0.97),
        plot.tag = element_text(size = 20),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        axis.title = element_text(size = 20,
                                  color = "black"),
        axis.text = element_text(size = 15,
                                 color = "black"), 
        legend.position = c(0.92,0.85),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

b

summary(lm(ratio ~ kcal_Destabilization, data = tests))

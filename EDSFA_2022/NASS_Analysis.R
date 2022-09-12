# NASS Data curation code
# Updated: 7/27/2022
# Last change author: Avery

### Clear working environment.

rm(list = ls())

### Load required packages.

library(tidyverse)
library(rnassqs)
library(tigris)
library(broom)
library(tidyUSDA)
library(zoo)
library(patchwork)
library(usmap)

##### Prepare data and source functions #####
### Set NASS API Key.

nassqs_auth("11FCF4DC-1B74-3577-B8A5-5F09B27E2390")

### Input data 

commodity.list <- read_csv("./Data/Inputs/Commodity_Input_Data_Trees.csv") 

## Conversions data

kcal_conversions <- read_csv("./Data/Inputs/kcal_conversion.csv")
price_conversions <- read_csv("./Data/Inputs/price_conversion.csv")

## Read in PPI data

ppi.data <- read_csv("./Data/Inputs/Annual_PPI_Data.csv")

## Set up an empty dataframe for calling the data

commodity.data <- tibble(State_Abbr = "",
                         State_Name = "",
                         Year = "",
                         Crop = "",
                         Crop_Class = "",
                         Crop_Utilization = "",
                         Crop_Production = "",
                         Crop_Area_ha = 1,
                         Area_Class = "",
                         Production_Weight = 1,
                         Production_Units = "",
                         Price_Received_USD = 1,
                         Price_Received_Units = "")

### Read in functions from the Function directory.

source("./Functions/Call_NASS_Data_updated.R") 

source("./Functions/Checking_NASS_Failures.R")

source("./Functions/Check_Utilization_Duplicates.R")

source("./Functions/NASS_Summary_Table.R")

source("./Functions/Initial_Conversion.R")

source("./Functions/Fill_and_Convert.R")

##### Read in commodity data from NASS ######

### Use the Call_NASS_Data() function to get all the data from the quick stats API. 

for(i in 1:nrow(commodity.list)) {

  try({temp.data <- Call_NASS_Data(Crop = commodity.list$Crop[i],
                                   Crop_Class = commodity.list$Crop_Class[i],
                                   Crop_Utilization = commodity.list$Crop_Utilization[i],
                                   Crop_Production = commodity.list$Crop_Production[i],
                                   Price_Data = commodity.list$Price_Data[i],
                                   Area_Data = commodity.list$Area_Data[i],
                                   Start_Year = 1981,
                                   End_Year = 2020)
  
    commodity.data <- rbind(commodity.data, temp.data)}
    
  )

}

## Remove the empty first row of the dataframe.

commodity.data <- commodity.data[-1,]

## Check if all the data was pulled (keep re-running this until you get a complete data message).

commodity.data <- Check_NASS_Failures(commodity.list,
                                      commodity.data)

## Check for crops with multiple utilization practices

commodity.data <- Check_Utilization_Duplicates(commodity.data)


##### Check, convert, and clean the data #####

summary <- NASS_Summary_Table(commodity.data,
                              commodity.list)

## Write intermediate data out.

write.csv(commodity.data, "./Data/Outputs/Intermediate_Data/Commodity_Data.csv")


## Quick plot of area, production, and price over time for each state and crop to check data coverage of original pulled data

check_data_figs <- function(crop, state){
  
  sub <- commodity.data %>%
    filter(Crop == crop, 
           State_Abbr == state) %>%
    mutate(Year = as.numeric(Year))
  
  a <- ggplot(sub, aes(x = Year, y = Crop_Area_ha)) + 
    geom_point(aes(color = Crop_Class, shape = Crop_Utilization), size = 2) +
    geom_line(aes(color = Crop_Class, linetype = Crop_Utilization)) +
    xlab(NULL) + 
    theme_classic() + 
    ggtitle(paste(crop, state))
  
  b <- ggplot(sub, aes(x = Year, y = Production_Weight)) + 
    geom_point(aes(color = Crop_Class, shape = Crop_Utilization), size = 2) +
    geom_line(aes(color = Crop_Class, linetype = Crop_Utilization)) +
    xlab(NULL) +
    theme_classic()
  
  c <- ggplot(sub, aes(x = Year, y = Price_Received_USD)) + 
    geom_point(aes(color = Crop_Class, shape = Crop_Utilization), size = 2) +
    geom_line(aes(color = Crop_Class, linetype = Crop_Utilization)) +
    theme_classic() 
  
  a/b/c
}

check_data_figs("OATS", "AL")

#Check relationship between area and production for all states.

# ggplot(commodity.data, aes(x = Crop_Area_ha, y = Production_Weight)) + 
#  geom_point(aes(color = State_Abbr)) + 
#  theme_classic() + facet_wrap(~Crop, scales = "free")

#This function converts the data to standard units (prices in $/kg and production in kg) and collapses most utilization practices (excluding those that have different caloric contents, such as shelled vs. unshelled almonds)

clean.kg.data <- Initial_Conversion(input.data = commodity.data,
                                    kcal.conversion.data = kcal_conversions, 
                                    price.conversion.data = price_conversions)

#This function fills gaps up to the specified year tolerance, calculates total calories and dollars, and collapses remaining utilization practices. 

clean.filled.data <- Fill_and_Convert(input.data = clean.kg.data,
                                      year.tolerance = 5,
                                      ppi.data = ppi.data)

#Store the clean data

write.csv(clean.filled.data, "./Data/Outputs/Intermediate_Data/Clean_Data.csv")


##### Final data checks #####

#Quick look at some summary stats

#NASS_Summary_Table(clean.filled.data,
#                   commodity.list)

## Plot yields by crop and state, using FAO yield data from N. America and Europe as an order-of-magnitude check of the cleaned/converted data

fao <- read.csv("Data/Inputs/FAO data check.csv") %>%
  rename(Crop_Name = USDA_name)

yield_boxplot <- function(crop){
  
plotdf <- clean.filled.data %>%
  mutate(USD_yield = Production_USD/Crop_Area_ha, 
         kcal_yield = Production_kcal/Crop_Area_ha, 
         kg_yield = Production_kg/Crop_Area_ha) %>%
  left_join(., fao) %>%
  filter(Crop_Name == crop)

a <- ggplot(plotdf, aes(x = State_Abbr, y = USD_yield)) + 
  geom_boxplot() + xlab(NULL) +
  ggtitle(paste(crop))
b <- ggplot(plotdf, aes(x = State_Abbr, y = kcal_yield)) + 
  geom_boxplot() + xlab(NULL)
c <- ggplot(plotdf, aes(x = State_Abbr, y = kg_yield)) + 
  geom_boxplot() + 
  geom_hline(linetype = "dashed", aes(yintercept = FAO_yield/10))

a/b/c

}

yield_boxplot(unique(clean.filled.data$Crop_Name)[1])

#Timeseries figs for filled data
check_clean_data <- function(crop, state){
  
  sub <- clean.filled.data %>%
    filter(Crop_Name == crop, 
           State_Abbr == state) %>%
    mutate(Year = as.numeric(Year))
  
  a <- ggplot(sub, aes(x = Year, y = Crop_Area_ha)) + 
    geom_point(size = 2) +
    geom_line() +
    xlab(NULL) + 
    theme_classic() + 
    ggtitle(paste(crop, state)) +
    theme(legend.position = "none")
  
  b <- ggplot(sub, aes(x = Year, y = Production_kg)) + 
    geom_point(size = 2) +
    geom_line() +
    xlab(NULL) +
    theme_classic() +
    theme(legend.position = "none")
  
  c <- ggplot(sub, aes(x = Year, y = Production_kcal)) + 
    geom_point(size = 2) +
    geom_line() +
    xlab(NULL) +
    theme_classic() +
    theme(legend.position = "none")
  
  d <- ggplot(sub, aes(x = Year, y = Production_USD)) + 
    geom_point(size = 2) +
    geom_line() +
    theme_classic() +
    theme(legend.position = "none")
  
  a/b/c/d
}

check_clean_data("ALMONDS-ALL CLASSES", "CA")

#Check that all expected kg production data are associated with kcal values
check <- clean.filled.data %>%
  filter(is.na(Production_kcal)) %>%
  filter(!(Crop_Name %in% c("COTTON-PIMA", "COTTON-UPLAND", "CORN-ALL CLASSES-SILAGE", "HAY-ALL CLASSES", "HOPS-ALL CLASSES", "TOBACCO-ALL CLASSES")))



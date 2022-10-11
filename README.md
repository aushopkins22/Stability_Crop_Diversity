# Stability_Crop_Diversity

## Overview

!! This repository contains R code and data associated with the manuscript XXXX. 

!! Add some background/abstract type text here. 


## File descriptions

Key components of this code base are: 

1. "NASS_data_assembly.R" includes code to query the NASS QuickStats database for crop areas, prices, and production. It also applies unit conversions, fills gaps, aggregates across crop subgroups, adjusts for inflation, and conducts data checks.
	- Data inputs:  
		- "Commodity_Input_Data.csv": Crop-specific query parameters corresponding to NASS classifications.       
		- "kcal_conversion.csv": Crop-specific unit conversion factors to convert from the NASS production units to caloric content, with kcal per kilogram data acquired from the USDA Nutrient Database.  
		- "price_conversion.csv": Crop-specific unit conversion factors to convert from the NASS price units to USD per kilogram.  
		- "Annual_PPI_Data.csv": U.S. Bureau of Labor Statistics data on the Producer Price Index for farm products, used to adjust for inflation. 
		- "FAO data check.csv": Aggregated FAO yield data from N. America and Europe and roughly aligned with the NASS crop classifications, used as a coarse check for order-of-magnitude anomalies in yield data.  
              
	- Data outputs:  
		- "Intermediate_Commodity_Data.csv": Raw data query results from NASS, including area, prices, and production.  
		- "Clean_Data.csv": Cleaned, aggregated, converted, and gap-filled data on crop area, caloric production, and USD production at the state-crop-year level. This file is used for all subsequent analyses.  

2. "Analysis_and_figures.R" includes code to conduct all analyses reported in the manuscript prior to the results section titled "Crop-specific contributions to yield stability".  
	- Data inputs:  
		- "irrigation_model_input.csv": Contains state-level data on the harvested cropand area, the irrigated harvested cropland area, and the proportion of area irrigated for all Census years between 1982-2017. This was produced via the script "irrigated_area_pull.R" in the "Climate and irrigation aggregation" folder.  
		- "annual_climate_model_input.csv": Contain annual data on mean precipitation and mean average temperature, weighted by cropland area (see Methods) and aggregated to the state level in the script "final_climate_aggregation.R".  
		- "decadal_climate_model_input.csv": Contains state-level decadal data on the mean, standard deviation, and coefficient of variation for precipiation and temperature produced by the script "final_climate_aggregation.R".
		- "Clean_Data.csv": Assembled data from NASS, output from the "NASS_data_assembly.R" script as described above. 
	- Outputs: 
		- Main text Figures 1 and 2 and corresponding results
		- SI figures 1 through 7 

3. "Leave_one_out.R" description
	- Data inputs: 
	- Outputs:
		-Main text Figure 3 and corresponding results
            

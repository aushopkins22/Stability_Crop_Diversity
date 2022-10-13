## Code for performing the leave-one out analysis that produces Figures 3A and 3B, as well as Supplemental Figure 5
## Written by: Sam Leuthold (sam.leuthold@gmail.com) and Avery Driscoll (Which email would you like here?)
## Last Updated: 10/13/2022; Sam Leuthold

## Load required packages ##

require(pacman)

p_load(tidyverse,
       patchwork,
       ggrepel,
       progress,
       usmap,
       zoo,
       broom)

## Read in required functions ##

source("./Functions/Calculate_Shannons_Diversity.R")
source("./Functions/Calculate_Production.R")
source("./Functions/Calculate_Residuals.R")
source("./Functions/Calculate_Stability.R")

options(dplyr.summarise.inform = FALSE) # Quiet summaries from dplyr

## Read in and clean data for commodity inputs and full dataset ##

crop.list <- read_csv(file = "./Data/Inputs/Commodity_Input_Data.csv") %>%
               mutate(Crop_Name = ifelse(.$Crop_Utilization %in% c("SILAGE", "GRAIN"),  
                                         paste0(.$Crop, "-", .$Crop_Class, "-", .$Crop_Utilization), 
                                         paste0(.$Crop, "-", .$Crop_Class)))
              

nass_full <- read.csv("./Data/Outputs/Intermediate_Data/Clean_Data.csv") %>%
              select(-X, 
                     -kcal_kg, 
                     -FIPS, 
                     -Price_Received_USD_kg) %>%
              mutate(fips = fips(State_Abbr))


## Calculate stability inputs ##

production_full <- Calculate_Production(nass_full, 10)
residuals_full <- Calculate_Residuals(nass_full)
diversity_full <- Calculate_Shannons_Diversity(nass_full)

## Create a full dataset.

full.dataset <- Calculate_Stability(production_full, 
                                    residuals_full,
                                    diversity_full,
                                    10) %>%
                  select(State_Abbr, 
                         Year,
                         stability_usd_yield,
                         stability_kcal_yield)

## Create an empty dataframe to fill.

results <- data.frame()


## Create a progress bar to track for-loop progress

pb <- progress_bar$new(total = nrow(crop.list))

## Fill the dataframe

for(i in 1:nrow(crop.list)){
  
  ## Call in a dataframe that excludes crop i.
  
  nass <- read.csv("./Data/Outputs/Intermediate_Data/Clean_Data.csv") %>%
            select(-X, 
                   -kcal_kg, 
                   -FIPS, 
                   -Price_Received_USD_kg) %>%
            mutate(fips = fips(State_Abbr)) %>%
            filter(Crop_Name != crop.list$Crop_Name[i])
  
  ## Call in dataframe for only crop i.
  
  crop.production <- read.csv("./Data/Outputs/Intermediate_Data/Clean_Data.csv") %>%
                       select(-X,
                              -kcal_kg,
                              -FIPS,
                              -Price_Received_USD_kg) %>%
                       mutate(fips = fips(State_Abbr)) %>%
                       filter(Crop_Name == crop.list$Crop_Name[i]) %>%
                       mutate(Year_State = paste0(Year, "_", State_Abbr)) %>%
                       group_by(Year, State_Abbr, Year_State) %>%
                       summarise(Total_Crop_USD = sum(Production_USD, na.rm = T),
                                 Total_Crop_kcal = sum(Production_kcal, na.rm = T))        
  
  ## Create dataframe that calculates total production of each state year excluding crop i.
  
  all.production <- nass %>%
                      group_by(Year, State_Abbr) %>%
                      summarise(Total_Crop_USD = sum(Production_USD, na.rm = T),
                                Total_Crop_kcal = sum(Production_kcal, na.rm = T)) %>%
                      mutate(Year_State = paste0(Year, "_", State_Abbr)) %>%
                      group_by(Year, State_Abbr, Year_State) %>%
                      filter(Year_State %in% crop.production$Year_State)
  
  ## Create a weighing factor based on state-year crop production.
  
  weighting.factor <- tibble(Drop_Crop = crop.list$Crop_Name[i],
                             State_Abbr = crop.production$State_Abbr,
                             Year = crop.production$Year,
                             USD_Weight = crop.production$Total_Crop_USD/(sum(crop.production$Total_Crop_USD) + sum(all.production$Total_Crop_USD)),
                             kcal_Weight = crop.production$Total_Crop_kcal/(sum(crop.production$Total_Crop_kcal) + sum(all.production$Total_Crop_kcal)))
  
  ## Calculate stability inputs on subset data
  
  production <- suppressMessages(Calculate_Production(nass, 10))
  residuals <- suppressMessages(Calculate_Residuals(nass))
  diversity <- suppressMessages(Calculate_Shannons_Diversity(nass))
  
  ## Calculate stability for subset data and calculate log response ratio using full data set stability.
  
  stability <- suppressMessages(Calculate_Stability(production, 
                                                    residuals,
                                                    diversity,
                                                    10)) %>%
               filter(!is.na(stability_usd_yield)) %>% 
              
               ## Simplify dataset
              
               select(State_Abbr, 
                      Year,
                      stability_usd_yield,
                      stability_kcal_yield) %>%
               rename(stability_usd_yield_Drop = stability_usd_yield,
                      stability_kcal_yield_Drop = stability_kcal_yield) %>%
               left_join(., full.dataset,
                         by = c("State_Abbr", "Year")) %>%
               mutate(Drop_Crop = crop.list$Crop_Name[i],
                      USD_LRR = -log(stability_usd_yield_Drop/stability_usd_yield),
                      kcal_LRR = -log(stability_kcal_yield_Drop/stability_kcal_yield)) %>% 
               left_join(., weighting.factor,
                         by = c("State_Abbr", "Year", "Drop_Crop")) %>% 
               mutate(kcal_LRR = ifelse(Drop_Crop == "HAY-ALL CLASSES", 0, kcal_LRR))
  
  ## Coerce stability data into dataframe.
  
  stability <- data.frame(stability)
  
  ## Bind rows to results dataframe
  
  results <- rbind(results, stability)
  
  pb$tick()
  
}

## Change years to categorical decade.

results <- results %>% 
            mutate(Decade = case_when(Year %in% 1981:1990 ~ "1980s",
                                      Year %in% 1991:2000 ~ "1990s",
                                      Year %in% 2001:2010 ~ "2000s",
                                      Year %in% 2011:2022 ~ "2010s"))



### Data production for Figure 3A. 

## Calculate annual LRR across states for USD Stability

mean_results_USD <- results %>% 
                      filter(!is.na(USD_LRR),
                             USD_LRR != 0) %>% 
                      group_by(Drop_Crop, Year) %>% 
                      summarise(USD_LRR = mean(USD_LRR, na.rm = T))
                    
## Calculate annual LRR across states for kcal Stability

mean_results_kcal <- results %>% 
                      filter(!is.na(kcal_LRR),
                             kcal_LRR != 0) %>% 
                      group_by(Drop_Crop, Year) %>% 
                      summarise(kcal_LRR = mean(kcal_LRR, na.rm = T))

## Join the LRR together and pivot for state-year LRR

mean_results_states <- left_join(mean_results_USD,
                                 mean_results_kcal) %>% 
                        pivot_longer(c(USD_LRR,
                                       kcal_LRR),
                                     names_to = c("Parameter"),
                                     values_to = "LRR")                     

## Join the LRRs together and calculate overall means.

mean_results <- left_join(mean_results_USD,
                          mean_results_kcal) %>%  
                  mutate(kcal_LRR = ifelse(is.na(kcal_LRR) == TRUE, ## Accounting for non caloric crops (i.e., hay, cotton, tobacco)
                                           0,
                                           kcal_LRR)) %>% 
                  group_by(Drop_Crop) %>% 
                  summarize(USD_LRR = mean(USD_LRR),
                            kcal_LRR = mean(kcal_LRR)) %>% 
                  mutate(Influence = abs(USD_LRR) + abs(kcal_LRR)) %>% 
                  pivot_longer(c(USD_LRR,
                                 kcal_LRR),
                               names_to = c("Parameter"),
                               values_to = "LRR") %>% 
                  arrange(desc(Influence)) %>% 
                  slice(1:30)

## Trim state-year results to include only top thirty crops.

mean_results_states <- mean_results_states %>% 
                        filter(Drop_Crop %in% mean_results$Drop_Crop)

#### Plotting Figure 3A

## Rename crops for plotting.

crop_names <- as_labeller(c(`RICE-ALL CLASSES` = "Rice", 
                            `ALMONDS-ALL CLASSES` = "Almonds",
                            `ORANGES-ALL CLASSES` = "Oranges",
                            `HAY-ALL CLASSES` = "Hay",
                            `PEANUTS-ALL CLASSES` = "Peanuts",
                            `CRANBERRIES-ALL CLASSES` = "Cranberries",
                            `WALNUTS-ENGLISH` = "Walnuts",
                            `SOYBEANS-ALL CLASSES` = "Soybeans",
                            `WHEAT-ALL CLASSES` = "Wheat", 
                            `HAZELNUTS-ALL CLASSES` = "Hazelnuts",
                            `CORN-ALL CLASSES-GRAIN` = "Corn (grain)",
                            `SWEET CORN-ALL CLASSES` = "Sweet corn",
                            `BROCCOLI-ALL CLASSES` = "Broccoli",
                            `SUGARBEETS-ALL CLASSES` = "Sugar beets",
                            `LETTUCE-HEAD` = "Lettuce (head)",
                            `POTATOES-ALL CLASSES` = "Potatoes", 
                            `WHEAT-ALL CLASSES` = "Wheat"))

Figure_3A <- ggplot() +
               geom_col(data = mean_results,
                        aes(x = LRR,
                            y = reorder(Drop_Crop, Influence),
                            fill = Parameter,
                            color = "black"),
                        position = position_dodge()) +
               geom_jitter(data = mean_results_states,
                           aes(x = LRR,
                               y = Drop_Crop,
                               color = Parameter),
                           alpha = 0.5,
                           height = 0.25) +
               geom_vline(xintercept = 0) +
               scale_y_discrete(labels = crop_names) +
               scale_x_continuous(name = "Log Response Ratio",
                                  limits = c(-0.35, 0.35)) +
               scale_fill_manual(name = "Stability Parameter",
                                 values = c(kcal_LRR = "#C35D33",
                                            USD_LRR = "#A7C2A4"),
                                 labels = c(kcal_LRR = "Calories",
                                            USD_LRR = "US Dollars")) +
               scale_color_manual(name = "Stability Parameter",
                                  values = c(kcal_LRR = "#C35D33",
                                             USD_LRR = "#A7C2A4"),
                                  labels = c(kcal_LRR = "Calories",
                                             USD_LRR = "US Dollars")) +
               labs(tag = "(a)") +
               ylab("") +
               theme(plot.tag.position = c(0.21, 0.97),
                     plot.tag = element_text(size = 20),
                     panel.background = element_rect(fill = "white",
                                                     color = "black"),
                     axis.title = element_text(size = 20,
                                               color = "black"),
                     axis.text = element_text(size = 15,
                                              color = "black"), 
                     legend.justification = c(1, 0), 
                     legend.position = c(1, 0),
                     legend.key.size = unit(0.75, "cm"),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 15))


Figure_3A

### Calculate the (de)stabilization indices.

Percentages_USD <- results %>% 
                     filter(!is.na(USD_LRR),
                            USD_LRR != 0) %>% 
                     mutate(USD_Direction = ifelse(USD_LRR < 0,
                                                   0,
                                                   1)) %>% 
                     group_by(Drop_Crop) %>% 
                     summarise(USD = sum(USD_Direction)/n())

Percentages_kcal <- results %>% 
                      filter(!is.na(kcal_LRR),
                             kcal_LRR != 0) %>% 
                      mutate(kcal_Direction = ifelse(kcal_LRR < 0,
                                                     0,
                                                     1)) %>% 
                      group_by(Drop_Crop) %>% 
                      summarise(kcal = sum(kcal_Direction)/n()) %>% 
                      mutate(kcal = 1 - kcal)

Percentages <- left_join(Percentages_USD, 
                         Percentages_kcal)

### Calculate the log transformed calorie to dollar ratio

ratios <- nass_full %>%
            group_by(Crop_Name) %>%
            summarise(Average_USD_Production = mean(Production_USD, na.rm = TRUE),
                      Average_kcal_Production = mean(Production_kcal, na.rm = TRUE),
                      Average_crop_area = mean(Crop_Area_ha, na.rm = T)) %>%
            mutate(USD = Average_USD_Production/Average_crop_area,
                   kcal = Average_kcal_Production/Average_crop_area,
                   ratio = log(kcal/USD))

### Join data together

plot.b.data <- Percentages %>% 
                rename(USD_Stabilization = USD,
                       kcal_Destabilization = kcal) %>% 
                left_join(.,
                          ratios,
                          by = c("Drop_Crop" = "Crop_Name"))

Figure_3B <- ggplot(plot.b.data,
                    aes(x = kcal_Destabilization,
                        y = ratio)) +
             geom_point(shape = 21,
                        size = 5,
                        fill = "grey") +
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
                                            color = "black"))          


Figure_3B

### Save Figure 3

Figure_3A|Figure_3B

#ggsave(filename = "Figure3.jpeg", path = "~/Desktop", units = "in", height = 8, width = 18)
#ggsave(filename = "Figure3.jpeg", path = ".././Manuscript/", units = "in", height = 8, width = 18)

#### Supplemental Figure 5

## Calculate full diversity

shannons <- diversity_full

## Calculate average state level deiversity

state_diversity <- shannons %>% 
                     group_by(State_Abbr) %>%
                     summarize(avg_ENCS_kcal = mean(Shannons_kcal, na.rm = T), 
                               avg_ENCS_usd = mean(Shannons_usd, na.rm = T)) %>%
                     ungroup()

## Calculate weighted diversity

effective_diversity <- nass_full %>% 
                        group_by(Crop_Name) %>% 
                        summarise(Total_Production_kcal = sum(Production_kcal, na.rm = T),
                                  Total_Production_USD = sum(Production_USD, na.rm = T)) %>% 
                        left_join(., nass_full,
                                  by = "Crop_Name") %>% 
                        mutate(Weight_USD = Production_USD/Total_Production_USD,
                               Weight_kcal = Production_kcal/Total_Production_kcal) %>% 
                        mutate(Weight_USD = ifelse(is.na(Weight_USD), 0, Weight_USD),
                               Weight_kcal = ifelse(is.na(Weight_kcal), 0, Weight_kcal)) %>% 
                        left_join(., state_diversity) %>% 
                        mutate(Weighted_Diversity_USD = Weight_USD * avg_ENCS_usd,
                               Weighted_Diversity_kcal = Weight_kcal * avg_ENCS_kcal) %>% 
                        group_by(Crop_Name) %>% 
                        summarize(USD_Diversity = sum(Weighted_Diversity_USD, na.rm = T),
                                  kcal_Diversity = sum(Weighted_Diversity_kcal, na.rm = T))
                      

## Join effective diversity with ratio data

effective_diversity_b <- effective_diversity %>% 
                           left_join(., ratios) %>% 
                           mutate(Crop_Name_chunks = str_split(Crop_Name, pattern = "-"),
                                  Crop_Name = map_chr(Crop_Name_chunks, 1),
                                  ratio = ratio) %>% 
                           select(Crop_Name,
                                  USD_Diversity,
                                  ratio) %>% 
                           mutate(Crop_Name = str_to_title(Crop_Name))

## Read in functional group data

functional_types <- read_csv("./Data/Inputs/Function_Groups.csv") %>% 
                      mutate(Crop = stringr::str_to_title(Crop)) %>% 
                      select(`Functional Type`,
                             Crop)

## Join functional groups with diversity data

effective_diversity_c <- left_join(effective_diversity_b, 
                                   functional_types,
                                   by = c("Crop_Name" = "Crop"))


## Create plot

Supplemental_Figure5 <- ggplot(data = effective_diversity_c, 
                               aes(x = USD_Diversity,
                                   y = ratio,
                                   color = as.factor(`Functional Type`))) +
                          geom_text_repel(aes(label = Crop_Name),
                                          size = 5,
                                          show.legend = F) +
                          geom_point(alpha = 0.01) +
                          scale_color_brewer(name = "Crop Functional Group",
                                             type = "div",
                                             palette = "Dark2") +
                          scale_x_continuous(limits = c(1, 3)) +
                          xlab("Diversity") +
                          ylab("ln(Calories per dollar)") +
                          guides(color = guide_legend(override.aes = list(size = 7, alpha = 1))) +
                          theme(plot.tag.position = c(0.13, 0.97),
                                plot.tag = element_text(size = 20),
                                panel.background = element_rect(fill = "white",
                                                                color = "black"),
                                axis.title = element_text(size = 20,
                                                          color = "black"),
                                axis.text = element_text(size = 15,
                                                         color = "black"), 
                                legend.justification = c(1,1),
                                legend.key  = element_rect(fill = "white", color = "white"),
                                legend.key.size = unit(0.75, "cm"),
                                legend.text = element_text(size = 13),
                                legend.title = element_text(size = 15))











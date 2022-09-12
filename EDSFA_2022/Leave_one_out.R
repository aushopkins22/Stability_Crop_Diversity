## Leaving one out.

library(tidyverse)
library(patchwork)
library(ggrepel)


source("./Functions/Calculate_Shannons_Diversity_updated.R")
source("./Functions/Calculate_Production.R")
source("./Functions/Calculate_Residuals.R")
source("./Functions/Calculate_Stability_updated.R")


## Read in data, and mutate names to match the names in the clean data file.

crop.list <- read_csv(file = "./Data/Inputs/Commodity_Input_Data_Trees.csv") %>%
               mutate(Crop_Name = ifelse(.$Crop_Utilization %in% c("SILAGE", "GRAIN"),  
                                         paste0(.$Crop, "-", .$Crop_Class, "-", .$Crop_Utilization), 
                                         paste0(.$Crop, "-", .$Crop_Class)))
              

nass_full <- read.csv("./Data/Outputs/Intermediate_Data/Clean_Data.csv") %>%
              select(-X, 
                     -kcal_kg, 
                     -FIPS, 
                     -Price_Received_USD_kg) %>%
              mutate(fips = fips(State_Abbr))


## Calculate stability inputs

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

## Fill the dataframe

options(dplyr.summarise.inform = FALSE)

for(i in 1:nrow(crop.list)){
  
  print(i/nrow(crop.list) * 100)
  
  nass <- read.csv("./Data/Outputs/Intermediate_Data/Clean_Data.csv") %>%
            select(-X, 
                   -kcal_kg, 
                   -FIPS, 
                   -Price_Received_USD_kg) %>%
            mutate(fips = fips(State_Abbr)) %>%
            filter(Crop_Name != crop.list$Crop_Name[i])
  
  
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
  all.production <- nass %>%
                      group_by(Year, State_Abbr) %>%
                      summarise(Total_Crop_USD = sum(Production_USD, na.rm = T),
                                Total_Crop_kcal = sum(Production_kcal, na.rm = T)) %>%
                      mutate(Year_State = paste0(Year, "_", State_Abbr)) %>%
                      group_by(Year, State_Abbr, Year_State) %>%
                      filter(Year_State %in% crop.production$Year_State)
  
  weighting.factor <- tibble(Drop_Crop = crop.list$Crop_Name[i],
                             State_Abbr = crop.production$State_Abbr,
                             Year = crop.production$Year,
                             USD_Weight = crop.production$Total_Crop_USD/(crop.production$Total_Crop_USD + all.production$Total_Crop_USD),
                             kcal_Weight = crop.production$Total_Crop_kcal/(crop.production$Total_Crop_kcal + all.production$Total_Crop_kcal))
  
  ## Calculate stability inputs on subset data
  
  production <- suppressMessages(Calculate_Production(nass, 10))
  residuals <- suppressMessages(Calculate_Residuals(nass))
  diversity <- suppressMessages(Calculate_Shannons_Diversity(nass))
  
  ## Calculate stability
  
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
  
  stability <- data.frame(stability)
  
  results <- rbind(results, stability)
  
}

results <- results %>% 
  mutate(Decade = case_when(Year %in% 1981:1990 ~ "1980s",
                            Year %in% 1991:2000 ~ "1990s",
                            Year %in% 2001:2010 ~ "2000s",
                            Year %in% 2011:2022 ~ "2010s"))



### Data production for Figure 3A. 

mean_results_USD <- results %>% 
                      filter(!is.na(USD_LRR),
                             USD_LRR != 0) %>% 
                      group_by(Drop_Crop, Year) %>% 
                      summarise(USD_LRR = mean(USD_LRR, na.rm = T))
                    
mean_results_kcal <- results %>% 
                      filter(!is.na(kcal_LRR),
                             kcal_LRR != 0) %>% 
                      group_by(Drop_Crop, Year) %>% 
                      summarise(kcal_LRR = mean(kcal_LRR, na.rm = T))

mean_results_states <- left_join(mean_results_USD,
                                 mean_results_kcal) %>% 
                        pivot_longer(c(USD_LRR,
                                       kcal_LRR),
                                     names_to = c("Parameter"),
                                     values_to = "LRR")                     

mean_results <- left_join(mean_results_USD,
                          mean_results_kcal) %>%  
                  mutate(kcal_LRR = ifelse(is.na(kcal_LRR) == TRUE,
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

mean_results_states <- mean_results_states %>% 
                        filter(Drop_Crop %in% mean_results$Drop_Crop)

#### Plotting Figure 3A

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

a <- ggplot() +
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
             legend.position = c(0.85,0.1),
             legend.key.size = unit(0.75, "cm"),
             legend.text = element_text(size = 13),
             legend.title = element_text(size = 15))


a


###
mean_results <- left_join(mean_results_USD,
                          mean_results_kcal) %>%  
                 mutate(kcal_LRR = ifelse(is.na(kcal_LRR) == TRUE,
                                          0,
                                          kcal_LRR)) %>% 
                 group_by(Drop_Crop) %>% 
                 summarize(USD_LRR = mean(USD_LRR),
                           kcal_LRR = mean(kcal_LRR)) %>% 
                 mutate(Influence = USD_LRR - kcal_LRR) %>% 
                 pivot_longer(c(USD_LRR,
                                kcal_LRR),
                              names_to = c("Parameter"),
                              values_to = "LRR")
###

shannons <- diversity_full

state_diversity <- shannons %>% 
                     group_by(State_Abbr) %>%
                     summarize(avg_ENCS_kcal = mean(ENCS_kcal, na.rm = T), 
                               avg_ENCS_usd = mean(ENCS_usd, na.rm = T)) %>%
                     ungroup()


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
                         Percentages_kcal) %>% 
                left_join(.,
                          effective_diversity,
                          by = c("Drop_Crop" = "Crop_Name"))

# # # # # # # # # # # 

ratios <- nass_full %>%
            group_by(Crop_Name) %>%
            summarise(Average_USD_Production = mean(Production_USD, na.rm = TRUE),
                      Average_kcal_Production = mean(Production_kcal, na.rm = TRUE),
                      Average_crop_area = mean(Crop_Area_ha, na.rm = T)) %>%
            mutate(USD = Average_USD_Production/Average_crop_area,
                   kcal = Average_kcal_Production/Average_crop_area,
                   ratio = log(kcal/USD))

plot.b.data <- Percentages %>% 
                rename(USD_Stabilization = USD,
                       kcal_Destabilization = kcal) %>% 
                left_join(.,
                          ratios,
                          by = c("Drop_Crop" = "Crop_Name"))

b <- ggplot(plot.b.data,
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
                                 color = "black"), 
        legend.position = c(0.92,0.85),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))


### Save Figure 3

a|b

ggsave(filename = "Figure3.jpeg", path = "~/Desktop", units = "in", height = 8, width = 18)


ggsave(filename = "Figure3.jpeg", path = ".././Manuscript/", units = "in", height = 8, width = 18)

###


simple.lm <- lm(kcal ~ USD, data = Percentages)

simple.lm

summary(simple.lm)

anova(simple.lm)

Percentages %>% 
  summarise(USD = mean(USD),
            kcal = mean(kcal, na.rm = T))

x <- Percentages %>% 
  mutate(Diff = akcal - USD) %>% 
  arrange(desc(kcal_Diversity))

###


ggplot(data = x,
       aes(x = USD_Diversity,
           y = Diff)) +
  geom_point()  +
  geom_smooth(method = "lm")

anova(lm(Diff ~ kcal_Diversity, data = x))


a|b

merging_data <- effective_diversity_b %>% 
                  filter(Crop_Name != "Lemons",
                         Crop_Name != "Mustard")

which(merging_data$Crop_Name != nathan.data$Crop_Name)

nathan.data <- Percentages %>% 
                select(Drop_Crop, 
                       USD,
                       kcal) %>% 
                mutate(Crop_Name_chunks = str_split(Drop_Crop, pattern = "-"),
                       Crop_Name = map_chr(Crop_Name_chunks, 1)) %>% 
                mutate(Crop_Name = str_to_title(Crop_Name)) %>% 
                select(-Drop_Crop,
                       -Crop_Name_chunks) %>% 
                bind_cols(., 
                          merging_data)


ggplot(data = nathan.data, 
       aes(x = ratio,
           y = USD)) +
  geom_point(pch = 21, 
             aes(fill = USD_Diversity),
             size = 5) +
  geom_smooth(method = "lm")


## As the ln(CPD) increases, the caloric stabilization potential decreases. 


Percentages <- Percentages %>% 
                mutate(Abs_Difference = abs(USD - kcal),
                       Difference = USD - kcal)

ggplot(Percentages, 
       aes(x = USD_Diversity,
           y = Difference)) +
  geom_point() +
  geom_smooth(method = "lm")


#### Supplemental Figure 1

##


effective_diversity_b <- effective_diversity %>% 
                           left_join(., ratios) %>% 
                           mutate(Crop_Name_chunks = str_split(Crop_Name, pattern = "-"),
                                  Crop_Name = map_chr(Crop_Name_chunks, 1),
                                  ratio = log(ratio)) %>% 
                           select(Crop_Name,
                                  USD_Diversity,
                                  ratio) %>% 
                           # filter(is.nan(ratio) != TRUE) %>% 
                           mutate(Crop_Name = str_to_title(Crop_Name))


functional_types <- read_csv("./Data/Inputs/Function_Groups.csv") %>% 
                      mutate(Crop = stringr::str_to_title(Crop)) %>% 
                      select(`Functional Type`,
                             Crop)

effective_diversity_c <- left_join(effective_diversity_b, 
                                   functional_types,
                                   by = c("Crop_Name" = "Crop"))


ggplot(data = effective_diversity_c, 
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
        legend.position = c(0.89,0.85),
        legend.key  = element_rect(fill = "white", color = "white"),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))






## Archive

# b <- ggplot(Percentages,
#             aes(x = USD,
#                 y = kcal)) +
#   scale_y_continuous(name = "Caloric destabilization potential",
#                      limits = c(0, 0.9)) +
#   scale_x_continuous(name = "Economic stabilization potential",
#                      limits = c(0, 0.9)) +
#   geom_point(aes(fill = USD_Diversity),
#              shape = 21, size = 5) +
#   geom_smooth(method = "lm", formula = y ~ x,
#               se =  FALSE,
#               lty = 2,
#               color = "black") +
#   scale_fill_distiller(name = "Diversity",
#                        palette = "YlGnBu", 
#                        direction = 1) +
#   labs(tag = "(b)") +
#   theme(plot.tag.position = c(0.13, 0.97),
#         plot.tag = element_text(size = 20),
#         panel.background = element_rect(fill = "white",
#                                         color = "black"),
#         axis.title = element_text(size = 20,
#                                   color = "black"),
#         axis.text = element_text(size = 15,
#                                  color = "black"), 
#         legend.position = c(0.92,0.85),
#         legend.key.size = unit(0.75, "cm"),
#         legend.text = element_text(size = 13),
#         legend.title = element_text(size = 15))





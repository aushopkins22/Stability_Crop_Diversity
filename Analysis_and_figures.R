#Analysis of patterns in crop diversity and yield stability and their drivers. 

#####Load data#####
library(tidyverse)
library(patchwork)
library(usmap)
library(broom)
library(zoo)
library(ggrepel)

#Load covariates
irr <- read.csv("./Data/Inputs/Model_covariates/irrigation_model_input.csv") %>%
  select(-X) %>%
  mutate(fips = fips(state_name)) %>%
  select(-state_name)

clim <- read.csv("./Data/Inputs/Model_covariates/decadal_climate_model_input.csv") %>%
  select(-X) %>%
  mutate(fips = fips(state_abb)) %>%
  select(decade, fips, ppt_instability_cv, tavg_instability_cv)

ann_clim <- read.csv("./Data/Inputs/Model_covariates/annual_climate_model_input.csv") %>%
  select(-X) %>%
  mutate(fips = fips(state_abb)) %>%
  select(-decade)

# Prepare covariates: 
# aggregate irrigation to decadal scale and merge with climate CVs
cov_decadal <- irr %>%
  group_by(fips, decade) %>%
  summarize(prop_irr = mean(prop_irr)) %>%
  left_join(., clim) %>%
  mutate(Year = ifelse(decade == "1980_1989", 1990, 2020)) %>% # Match years from diverity-stability data 
  mutate(Year = ifelse(decade == "1990_1999", 2000, Year)) %>%
  mutate(Year = ifelse(decade == "2000_2009", 2010, Year)) %>%
  select(-decade)

nass <- read.csv("./Data/Outputs/Intermediate_Data/Clean_Data.csv") %>%
  select(-X, -kcal_kg, -FIPS, -Price_Received_USD_kg) %>%
  mutate(fips = fips(State_Abbr))

source("./Functions/Calculate_Shannons_Diversity.R")
source("./Functions/Calculate_Production.R")
source("./Functions/Calculate_Residuals.R")
source("./Functions/Calculate_Stability.R")

#####Figure 1 and S5: Maps of average diversity and stability#####
shannons <- Calculate_Shannons_Diversity(nass)
residuals <- Calculate_Residuals(nass)
production <- Calculate_Production(nass, 10)
decadal_full_dataset <- Calculate_Stability(production, residuals, shannons, 10) %>%
  filter(Year %in% c(1990, 2000, 2010, 2020)) %>% # Years 1990, 2000, 2010, and 2020 each reflect the preceding decade
  select(Year, State_Abbr, fips, stability_kcal_prod, stability_kcal_yield, stability_usd_prod, stability_usd_yield, instability_area, rm_shannons_area, rm_shannons_usd, rm_shannons_kcal)

# national contribution of crops to caloric vs economic production across entire study period
check <- nass %>%
  group_by(Crop_Name) %>%
  summarize(kcal = sum(Production_kcal, na.rm = T), 
            usd = sum(Production_USD, na.rm = T)) %>%
  ungroup() %>%
  mutate(total_kcal = sum(kcal), 
         total_usd = sum(usd)) %>%
  mutate(prop_kcal = kcal/total_kcal, 
         prop_usd = usd/total_usd)

kcal_cont <- check %>%
  slice_max(., order_by = prop_kcal, n = 5)
sum(kcal_cont$prop_kcal)

usd_cont <- check %>%
  slice_max(., order_by = prop_usd, n = 5)
sum(usd_cont$prop_usd)

# Check average price by caloric content 
ratios <- nass %>%
  mutate(price = Production_USD/Production_kg, 
         caloric_content = Production_kcal/Production_kg) %>%
  group_by(Crop_Name) %>%
  summarize(price = mean(price, na.rm = T), 
            calorie = mean(caloric_content, na.rm = T))

summary.lm(lm(price~calorie, data = ratios))

fun_groups <- read.csv("./Data/Inputs/Function_Groups.csv") %>%
  mutate(Crop_Name = ifelse(Crop_Utilization == "GRAIN", paste(.$Crop, .$Crop_Class, .$Crop_Utilization, sep = "-"), 
                            paste(.$Crop, .$Crop_Class, sep = "-"))) %>%
  rename(group = Functional.Type) %>%
  mutate(group = ifelse(.$group == "Fats & Oils", "Nut & Seed", .$group))

plotdf <- left_join(ratios, fun_groups) %>%
  select(Crop_Name, group, calorie, price) %>%
  filter(!(calorie == "NaN")) %>%
  distinct()
  
ggplot(plotdf, aes(x = price, y = calorie)) + 
  geom_point(size = 2, alpha = 0.8, aes(color = group, shape = group)) + 
  scale_shape_manual(values = c(16,17,4,1,2), name = "Crop Type") + 
  scale_color_brewer(name = "Crop Type",
                     type = "div",
                     palette = "Dark2") +
  theme_classic() + 
  xlab("Average crop price (USD)") + ylab("Crop caloric content (Kcal)")

ggsave(filename = "Figure S3 Caloric content by price.jpeg", path = "~/Desktop", units = "in", height = 4, width = 6)
  

### Map diversity
# Calculate average diversity
plotdf <- shannons %>%
  group_by(fips, State_Abbr) %>%
  summarize(avg_shannons_kcal = mean(Shannons_kcal), 
            avg_shannons_usd = mean(Shannons_usd), 
            avg_shannons_area = mean(Shannons_area)) %>%
  mutate(area_minus_kcal = avg_shannons_area - avg_shannons_kcal, #check differences between metrics
         area_minus_usd = avg_shannons_area - avg_shannons_usd, 
         usd_minus_kcal = avg_shannons_usd - avg_shannons_kcal)

# check relationships between metrics 
cor.test(plotdf$avg_shannons_kcal, plotdf$avg_shannons_usd)
cor.test(plotdf$avg_shannons_kcal, plotdf$avg_shannons_area)
cor.test(plotdf$avg_shannons_usd, plotdf$avg_shannons_area)


a <- plot_usmap(data = plotdf, values = "avg_shannons_kcal", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "YlGnBu", 
                       name = "Crop diversity", 
                       na.value = "grey", 
                       limits = c(0,2.7),
                       direction = 1, 
                       breaks = c(0.5, 1, 1.5, 2, 2.5), 
                       labels = c("0.5", "1", "1.5", "2", "2.5")) +
  labs(tag = "(a)", subtitle = "Caloric diversity") + 
  theme(plot.tag.position = c(0.05, 0.92), 
        legend.title = element_text(hjust = 0.5, size = 11),  
        plot.subtitle = element_text(size = 12), 
        legend.text = element_text(size = 10)) + 
  guides(fill = guide_colourbar(title.position = "top"))

b <- plot_usmap(data = plotdf, values = "avg_shannons_usd", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "YlGnBu", 
                       name = "Crop diversity", 
                       na.value = "grey", 
                       limits = c(0,2.7),
                       direction = 1, 
                       breaks = c(0.5, 1, 1.5, 2, 2.5), 
                       labels = c("0.5", "1", "1.5", "2", "2.5")) +
  labs(tag = "(b)", subtitle = "Economic diversity") + 
  theme(plot.tag.position = c(0.05, 0.92), 
        legend.title = element_text(hjust = 0.5, size = 11), 
        plot.subtitle = element_text(size = 12), 
        legend.text = element_text(size = 10)) + 
  guides(fill = guide_colourbar(title.position = "top"))


# Map stability
plotdf <- decadal_full_dataset %>%
  group_by(fips, State_Abbr) %>%
  summarize(avg_stability_kcal = mean(stability_kcal_yield, na.rm = T), 
            avg_stability_usd = mean(stability_usd_yield, na.rm = T), 
            avg_prod_stab_kcal = mean(stability_kcal_prod, na.rm = T), 
            avg_prod_stab_usd = mean(stability_usd_prod, na.rm = T)) %>%
  mutate(usd_minus_kcal = avg_stability_usd - avg_stability_kcal)#check differences between metrics

mean(plotdf$avg_stability_usd, na.rm = T)
mean(plotdf$avg_stability_kcal, na.rm = T)

sd(plotdf$avg_stability_usd, na.rm = T)
sd(plotdf$avg_stability_kcal, na.rm = T)

# check relationship between caloric and economic stabilities
cor.test(plotdf$avg_stability_kcal, plotdf$avg_stability_usd)
summary.lm(lm(stability_kcal_yield~stability_usd_yield, subset(decadal_full_dataset, Year == 1990)))
summary.lm(lm(stability_kcal_yield~stability_usd_yield, subset(decadal_full_dataset, Year == 2000)))
summary.lm(lm(stability_kcal_yield~stability_usd_yield, subset(decadal_full_dataset, Year == 2010)))
summary.lm(lm(stability_kcal_yield~stability_usd_yield, subset(decadal_full_dataset, Year == 2020)))

## Check relationships between production stability and yield stability
cor.test(plotdf$avg_stability_kcal, plotdf$avg_prod_stab_kcal) #significant
cor.test(plotdf$avg_stability_usd, plotdf$avg_prod_stab_usd) #significant

summary.lm(lm(stability_kcal_yield~stability_kcal_prod, subset(decadal_full_dataset, Year == 1990)))
summary.lm(lm(stability_kcal_yield~stability_kcal_prod, subset(decadal_full_dataset, Year == 2000)))
summary.lm(lm(stability_kcal_yield~stability_kcal_prod, subset(decadal_full_dataset, Year == 2010)))
summary.lm(lm(stability_kcal_yield~stability_kcal_prod, subset(decadal_full_dataset, Year == 2020)))

summary.lm(lm(stability_usd_yield~stability_usd_prod, subset(decadal_full_dataset, Year == 1990)))
summary.lm(lm(stability_usd_yield~stability_usd_prod, subset(decadal_full_dataset, Year == 2000)))
summary.lm(lm(stability_usd_yield~stability_usd_prod, subset(decadal_full_dataset, Year == 2010)))
summary.lm(lm(stability_usd_yield~stability_usd_prod, subset(decadal_full_dataset, Year == 2020)))

#Truncate stability values for easier visualization (affects 3 states kcal stability)
plotdf$stability_kcal_trunc <- ifelse(plotdf$avg_stability_kcal > 20, 20, plotdf$avg_stability_kcal)
plotdf$stability_usd_trunc <- ifelse(plotdf$avg_stability_usd > 20, 20, plotdf$avg_stability_usd)

c <- plot_usmap(data = plotdf, values = "stability_kcal_trunc", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Yield Stability", 
                       na.value = "grey", 
                       limits = c(4,20),
                       direction = 1, 
                       breaks = c(5, 10, 15, 20),
                       labels=c("5", "10", "15", expression("">=20))) +
  labs(tag = "(c)", subtitle = "Caloric yield stability") + 
  theme(plot.tag.position = c(0.05, 0.92), 
        legend.title = element_text(hjust = 0.5, size = 11), 
        plot.subtitle = element_text(size = 12), 
        legend.text = element_text(size = 10)) + 
  guides(fill = guide_colourbar(title.position = "top"))

d <- plot_usmap(data = plotdf, values = "stability_usd_trunc", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Yield Stability", 
                       na.value = "grey", 
                       limits = c(4,20),
                       breaks = c(5, 10, 15, 20),
                       labels=c("5", "10", "15", expression("">=20)),
                       direction = 1) +
  labs(tag = "(d)", subtitle = "Economic yield stability") + 
  theme(plot.tag.position = c(0.05, 0.92), 
        legend.title = element_text(hjust = 0.5, size = 11), 
        plot.subtitle = element_text(size = 12), 
        legend.text = element_text(size = 10)) + 
  guides(fill = guide_colourbar(title.position = "top"))

(a + b + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.margin = margin(t = -25), legend.justification = "center"))/(c + d + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.margin = margin(t = -25), legend.justification = "center"))

ggsave(filename = "Fig 1 average diversity and stability maps.jpeg", path = "~/Desktop", units = "in", height = 7, width = 10)

# Calculate number of crops per state each year
n_crops <- nass %>%
  group_by(fips, State_Abbr, Year) %>%
  summarize(n = n_distinct(Crop_Name)) %>%
  group_by(Year)

ggplot(n_crops, aes(x = Year, y = n)) + 
  geom_point(aes(color = State_Abbr)) + 
  geom_line(aes(color = State_Abbr))

#Test relationship between diversity and average climate and irrigation
diversity_drivers <- ann_clim %>%
  rename(Year = year, State_Abbr = state_abb) %>%
  left_join(., shannons) %>%
  group_by(State_Abbr, fips) %>%
  summarize(m_prism_ppt = mean(m_prism_ppt), m_prism_tavg = mean(m_prism_tavg),
            shannons_kcal = mean(Shannons_kcal), shannons_usd = mean(Shannons_usd)) %>%
  ungroup()

avg_irr <- irr %>%
  group_by(fips) %>%
  summarize(m_prop_irr = mean(prop_irr)) %>%
  ungroup()

diversity_drivers <- left_join(diversity_drivers, avg_irr) %>%
  mutate(ppt_quad = m_prism_ppt^2, 
         tavg_quad = m_prism_tavg^2, 
         irr_quad = m_prop_irr^2)

cor.test(diversity_drivers$m_prism_tavg, diversity_drivers$shannons_kcal)
cor.test(diversity_drivers$m_prism_tavg, diversity_drivers$shannons_usd)

cor.test(diversity_drivers$m_prism_ppt, diversity_drivers$shannons_kcal)
cor.test(diversity_drivers$m_prism_ppt, diversity_drivers$shannons_usd)

cor.test(diversity_drivers$m_prop_irr, diversity_drivers$shannons_kcal)
cor.test(diversity_drivers$m_prop_irr, diversity_drivers$shannons_usd)

#####Figure S4: ln(Calories per dollar) by diversity######
#Calculate diversity by crop 
crop_diversity <- nass %>% 
  group_by(Crop_Name) %>% 
  summarise(Total_Production_kcal = sum(Production_kcal, na.rm = T),
            Total_Production_USD = sum(Production_USD, na.rm = T)) %>% 
  left_join(., nass,
            by = "Crop_Name") %>% 
  mutate(Weight_USD = Production_USD/Total_Production_USD,
         Weight_kcal = Production_kcal/Total_Production_kcal) %>% 
  mutate(Weight_USD = ifelse(is.na(Weight_USD), 0, Weight_USD),
         Weight_kcal = ifelse(is.na(Weight_kcal), 0, Weight_kcal)) %>% 
  left_join(., shannons) %>% 
  mutate(Weighted_Diversity_USD = Weight_USD * Shannons_usd,
         Weighted_Diversity_kcal = Weight_kcal * Shannons_kcal) %>% 
  group_by(Crop_Name) %>% 
  summarize(USD_Diversity = sum(Weighted_Diversity_USD, na.rm = T),
            kcal_Diversity = sum(Weighted_Diversity_kcal, na.rm = T))

#Caclulate calorie:price ratio
ratios <- ratios %>% 
  mutate(ratio = calorie/price)

#Merge diversity, kcal:dollar ratios, and crop types
plotdf <- crop_diversity %>% 
  left_join(., ratios) %>% 
  left_join(., fun_groups) %>%
  mutate(Crop_Name_chunks = str_split(Crop_Name, pattern = "-"),
         Crop_Name = map_chr(Crop_Name_chunks, 1),
         logratio = log(ratio)) %>% 
  select(Crop_Name,
         USD_Diversity,
         ratio,
         logratio,
         group) %>% 
  mutate(Crop_Name = str_to_title(Crop_Name)) %>%
  distinct() %>%
  filter(ratio != "NaN")

cor.test(plotdf$logratio, plotdf$USD_Diversity) 

#Plot ratio by diversity
ggplot(data = plotdf, 
       aes(x = USD_Diversity,
           y = logratio,
           color = group)) +
  geom_text_repel(aes(label = Crop_Name),
                  size = 2.5,
                  show.legend = F, 
                  max.overlaps = 25) +
  geom_point(alpha = 0.01) +
  scale_color_brewer(name = "Crop Type",
                     type = "div",
                     palette = "Dark2") +
  xlab("Economic diversity") +
  ylab("ln(Calories per dollar)") +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(plot.tag.position = c(0.13, 0.97),
        plot.tag = element_text(size = 20),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        axis.title = element_text(size = 15,
                                  color = "black"),
        axis.text = element_text(size = 12,
                                 color = "black"), 
        legend.position = c(0.89,0.85),
        legend.key  = element_rect(fill = "white", color = "white"),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

ggsave(filename = "./Figures/Figure S4 CPD by diversity.jpeg", units = "in", height = 6, width = 9)

#####Figure S1, S5, and S6: diversity and stability maps by decade#####
# Crop diversity maps by decade
plotdf <- decadal_full_dataset

a <- plot_usmap(data = plotdf, values = "rm_shannons_kcal", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "YlGnBu", 
                       name = "Crop diversity", 
                       na.value = "grey", 
                       direction = 1, 
                       limits = c(0, 3.2)) +
  labs(subtitle = "Caloric diversity by decade") + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = "right") + 
  facet_wrap(~Year, nrow = 1) 

b <- plot_usmap(data = plotdf, values = "rm_shannons_usd", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "YlGnBu", 
                       name = "Crop diversity", 
                       na.value = "grey", 
                       direction = 1, 
                       limits = c(0, 3.2)) +
  labs(subtitle = "Economic diversity by decade") + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = "right") + 
  facet_wrap(~Year, nrow = 1)

a / b

ggsave(filename = "Figure S5 Decadal diversity maps.jpeg", path = "~/Desktop", units = "in", height = 4, width = 10)

# Yield stability maps by decade
a <- plot_usmap(data = plotdf, values = "stability_kcal_yield", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Yield stability", 
                       na.value = "grey", 
                       #limits = c(0,40),
                       direction = 1) +
  labs(subtitle = "Caloric yield stability by decade") + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = "right") + 
  facet_wrap(~Year, nrow = 1) 

b <- plot_usmap(data = plotdf, values = "stability_usd_yield", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Yield stability", 
                       na.value = "grey", 
                       #limits = c(0,40),
                       direction = 1) +
  labs(subtitle = "Economic yield stability by decade") + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = "right") + 
  facet_wrap(~Year, nrow = 1) 

a / b

ggsave(filename = "Figure S6 Decadal yield stability maps.jpeg", path = "~/Desktop", units = "in", height = 4, width = 10)

#Check caloric stability in AZ in 1990s
az <- subset(nass, State_Abbr == "AZ")
ggplot(az, aes(x = Year, y = (Production_kcal/Crop_Area_ha))) + 
         geom_point(aes(color = Crop_Name)) + geom_line(aes(color = Crop_Name))


# Production stability maps by decade
a <- plot_usmap(data = plotdf, values = "stability_kcal_prod", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Production stability", 
                       na.value = "grey", 
                       direction = 1) +
  labs(subtitle = "Caloric production stability by decade") + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = "right") + 
  facet_wrap(~Year, nrow = 1) 

b <- plot_usmap(data = plotdf, values = "stability_usd_prod", size = 0.1, exclude = c("HI", "AK", "DC")) +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Production stability", 
                       na.value = "grey", 
                       direction = 1) +
  labs(subtitle = "Economic production stability by decade") + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = "right") + 
  facet_wrap(~Year, nrow = 1) 

a / b

ggsave(filename = "Figure S1 Decadal production stability maps.jpeg", path = "~/Desktop", units = "in", height = 4, width = 10)

#####Figure 2: Decadal regressions: full dataset####
shannons <- Calculate_Shannons_Diversity(nass)
residuals <- Calculate_Residuals(nass)
production <- Calculate_Production(nass, 10)
decadal_full_dataset <- Calculate_Stability(production, residuals, shannons, 10) %>%
  filter(Year %in% c(1990, 2000, 2010, 2020)) %>% # Years 1990, 2000, 2010, and 2020 each reflect the preceding decade
  select(Year, State_Abbr, fips, stability_kcal_prod, stability_kcal_yield, stability_usd_prod, stability_usd_yield, instability_area,  rm_shannons_area, rm_shannons_usd, rm_shannons_kcal)

#Center and scale variables
scaled_decadal_all <- left_join(cov_decadal, decadal_full_dataset) %>%
  mutate_at(c(2:4, 7:14), scale) %>%
  mutate(Year = as.character(Year))

# Fit model for kcal yield and production
kcalyield_mod_all <- lm(stability_kcal_yield~rm_shannons_kcal + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_all)
kcalprod_mod_all <- lm(stability_kcal_prod~rm_shannons_kcal + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_all)

# Fit model for usd yield
usdyield_mod_all <- lm(stability_usd_yield~rm_shannons_usd + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_all)
usdprod_mod_all <- lm(stability_usd_prod~rm_shannons_usd + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_all)

summary.lm(lm(kcalyield_mod_all))
summary.lm(lm(usdyield_mod_all)) 
summary.lm(lm(kcalprod_mod_all))
summary.lm(lm(usdprod_mod_all)) 

#Plot coefficients
kcal_coeff <- as.data.frame(summary(kcalyield_mod_all)$coefficients) %>%
  mutate(variable = rownames(.)) %>%
  filter(!(variable %in% c("(Intercept)", "Year2000", "Year2010", "Year2020"))) %>%
  mutate(variable = factor(variable, 
                           levels = c("rm_shannons_kcal", "ppt_instability_cv", "prop_irr", "tavg_instability_cv"),
                           labels = c("Crop diversity", "PPT instability", "Proportion irrigated", "Tavg instability")))

usd_coeff <- as.data.frame(summary(usdyield_mod_all)$coefficients) %>%
  mutate(variable = rownames(.)) %>%
  filter(!(variable %in% c("(Intercept)", "Year2000", "Year2010", "Year2020"))) %>%
  mutate(variable = factor(variable, 
                           levels = c("rm_shannons_usd", "ppt_instability_cv", "prop_irr", "tavg_instability_cv"),
                           labels = c("Crop diversity", "PPT instability", "Proportion irrigated", "Tavg instability")))

a <- ggplot(kcal_coeff, aes(x = reorder(variable, Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = (Estimate - 1.96*`Std. Error`), 
                      ymax = (Estimate + 1.96*`Std. Error`))) + 
  theme_classic(base_size = 12) + 
  ylab("Effect on caloric yield stability") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_discrete(limits = c("Crop diversity", "PPT instability", "Tavg instability", "Proportion irrigated")) +
  labs(tag = "(a)") + 
  theme(plot.tag.position = c(0.22, 0.95), plot.tag = element_text(size = 10),
        axis.text.x = element_blank()) 

b <- ggplot(usd_coeff, aes(x = reorder(variable, Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = (Estimate - 1.96*`Std. Error`), 
                      ymax = (Estimate + 1.96*`Std. Error`))) + 
  theme_classic(base_size = 12) + 
  ylab("Effect on economic yield stability") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_discrete(limits = c("Crop diversity", "PPT instability", "Tavg instability", "Proportion irrigated")) +
  labs(tag = "(b)") + 
  theme(plot.tag.position = c(0.22, 0.95), plot.tag = element_text(size = 10),  
        axis.text.x = element_text(angle = 90, hjust = 1))

a/b
ggsave(filename = "Fig 2 yield stability coefficients.jpeg", path = "~/Desktop", units = "in", height = 6.5, width = 3.5)

#####Figure S2, S7: Decadal regressions: comparing datasets and plotting production coefficients#####
# Calculate for only crops present somewhere in 1981:
crops_1981 <- aggregate(Year~Crop_Name, nass, min) %>%
  filter(Year == 1981)

# 30 crops; keeps ~18k obs
nass_1981 <- nass %>%
  filter(Crop_Name %in% c(crops_1981$Crop_Name))

shannons <- Calculate_Shannons_Diversity(nass_1981)
residuals <- Calculate_Residuals(nass_1981)
production <- Calculate_Production(nass_1981, 10)
decadal_1981_dataset <- Calculate_Stability(production, residuals, shannons, 10) %>%
  filter(Year %in% c(1990, 2000, 2010, 2020)) %>% # Years 1990, 2000, 2010, and 2020 each reflect the preceding decade
  select(Year, State_Abbr, fips, stability_kcal_prod, stability_kcal_yield, stability_usd_prod, stability_usd_yield, instability_area, rm_shannons_area, rm_shannons_usd, rm_shannons_kcal)

scaled_decadal_1981 <- left_join(cov_decadal, decadal_1981_dataset) %>%
  mutate_at(c(2:4, 7:14), scale) %>%
  mutate(Year = as.character(Year))

# Fit models for kcal yield and production
kcalyield_mod_1981 <- lm(stability_kcal_yield~rm_shannons_kcal + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_1981)
kcalprod_mod_1981 <- lm(stability_kcal_prod~rm_shannons_kcal + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_1981)

# Fit models for usd yield and production
usdyield_mod_1981 <- lm(stability_usd_yield~rm_shannons_usd + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_1981)
usdprod_mod_1981 <- lm(stability_usd_prod~rm_shannons_usd + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_1981)

summary.lm(lm(kcalyield_mod_1981))
summary.lm(lm(kcalprod_mod_1981))
summary.lm(lm(usdyield_mod_1981)) 
summary.lm(lm(usdprod_mod_1981)) 

# State-specific threshold; only crops with entire timeseries present
state_81_crops <- nass %>%
  filter(Year == 1981) %>%
  select(State_Abbr, Crop_Name) %>%
  distinct()

# filter data, keeps ~16k obs
nass_restricted <- data.frame()

for(i in c(unique(state_81_crops$State_Abbr))){
  temp_crops <- state_81_crops %>%
    filter(State_Abbr == i)
  
  temp_nass <- nass %>%
    filter(State_Abbr == i) %>%
    filter(Crop_Name %in% c(temp_crops$Crop_Name))
  
  nass_restricted <- rbind(nass_restricted, temp_nass)
}

shannons <- Calculate_Shannons_Diversity(nass_restricted)
residuals <- Calculate_Residuals(nass_restricted)
production <- Calculate_Production(nass_restricted, 10)
decadal_restricted_dataset <- Calculate_Stability(production, residuals, shannons, 10) %>%
  filter(Year %in% c(1990, 2000, 2010, 2020)) %>% # Years 1990, 2000, 2010, and 2020 each reflect the preceding decade
  select(Year, State_Abbr, fips, stability_kcal_prod, stability_kcal_yield, stability_usd_prod, stability_usd_yield, instability_area, rm_shannons_area, rm_shannons_usd, rm_shannons_kcal)

scaled_decadal_restricted <- left_join(cov_decadal, decadal_restricted_dataset) %>%
  mutate_at(c(2:4, 7:14), scale) %>%
  mutate(Year = as.character(Year))

# Fit models for kcal yield and production
kcalyield_mod_restricted <- lm(stability_kcal_yield~rm_shannons_kcal + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_restricted)
kcalprod_mod_restricted <- lm(stability_kcal_prod~rm_shannons_kcal + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_restricted)

# Fit models for usd yield and production
usdyield_mod_restricted <- lm(stability_usd_yield~rm_shannons_usd + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_restricted)
usdprod_mod_restricted <- lm(stability_usd_prod~rm_shannons_usd + Year + prop_irr + tavg_instability_cv + ppt_instability_cv, scaled_decadal_restricted)

summary.lm(lm(kcalyield_mod_restricted))
summary.lm(lm(kcalprod_mod_restricted))
summary.lm(lm(usdyield_mod_restricted))
summary.lm(lm(usdprod_mod_restricted))

### Plot coefficients for all subsets for yield:
# Extract yield coefficients: 
# caloric yield
kcal_coeff_kcal <- rbind(
  (as.data.frame(summary(kcalyield_mod_all)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "All crops")), 
  (as.data.frame(summary(kcalyield_mod_restricted)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "State-specific restriction")), 
  (as.data.frame(summary(kcalyield_mod_1981)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "Present since 1981"))
) %>%
  filter(!(variable %in% c("(Intercept)", "Year2000", "Year2010", "Year2020"))) %>%
  mutate(variable = factor(variable, 
                           levels = c("rm_shannons_kcal", "ppt_instability_cv", "prop_irr", "tavg_instability_cv"),
                           labels = c("Diversity", "PPT instability", "Proportion irrigated", "Tavg instability")))

# usd yield
usd_coeff_usd <- rbind(
  (as.data.frame(summary(usdyield_mod_all)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "All crops")), 
  (as.data.frame(summary(usdyield_mod_restricted)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "State specific restriction")), 
  (as.data.frame(summary(usdyield_mod_1981)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "Measured entire period"))
) %>%
  filter(!(variable %in% c("(Intercept)", "Year2000", "Year2010", "Year2020"))) %>%
  mutate(variable = factor(variable, 
                           levels = c("rm_shannons_usd", "ppt_instability_cv", "prop_irr", "tavg_instability_cv"),
                           labels = c("Diversity", "PPT instability", "Proportion irrigated", "Tavg instability")))


a <- ggplot(kcal_coeff_kcal, aes(x = reorder(variable, Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = (Estimate - 1.96*`Std. Error`), 
                      ymax = (Estimate + 1.96*`Std. Error`), 
                      color = crops_included), 
                  position=position_dodge(width=0.5)) + 
  theme_classic(base_size = 12) + 
  ylab("Effect on caloric yield stability") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_discrete(limits = c("Diversity", "PPT instability", "Tavg instability", "Proportion irrigated")) +
  labs(tag = "(a)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.tag.position = c(0.16, 0.98), plot.tag = element_text(size = 12),
        legend.position = "none")

b <- ggplot(usd_coeff_usd, aes(x = reorder(variable, Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = (Estimate - 1.96*`Std. Error`), 
                      ymax = (Estimate + 1.96*`Std. Error`), 
                      color = crops_included), 
                  position=position_dodge(width=0.5)) + 
  theme_classic(base_size = 12) + 
  ylab("Effect on economic yield stability") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = c(0.8,0.95), 
        plot.tag.position = c(0.16, 0.98), plot.tag = element_text(size = 12)) + 
  scale_x_discrete(limits = c("Diversity", "PPT instability", "Tavg instability", "Proportion irrigated")) +
  labs(tag = "(b)", color = "")

a|b

ggsave(filename = "Fig S6 all subsets yield coefficients.png", path = "~/Desktop", units = "in", height = 5.5, width = 10)

###Extract and plot production coefficients
# caloric prod
kcal_coeff_kcal <- rbind(
  (as.data.frame(summary(kcalprod_mod_all)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "All crops")), 
  (as.data.frame(summary(kcalprod_mod_restricted)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "State-specific restriction")), 
  (as.data.frame(summary(kcalprod_mod_1981)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "Present since 1981"))
) %>%
  filter(!(variable %in% c("(Intercept)", "Year2000", "Year2010", "Year2020"))) %>%
  mutate(variable = factor(variable, 
                           levels = c("rm_shannons_kcal", "ppt_instability_cv", "prop_irr", "tavg_instability_cv"),
                           labels = c("Diversity", "PPT instability", "Proportion irrigated", "Tavg instability")))

# usd prod
usd_coeff_usd <- rbind(
  (as.data.frame(summary(usdprod_mod_all)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "All crops")), 
  (as.data.frame(summary(usdprod_mod_restricted)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "State specific restriction")), 
  (as.data.frame(summary(usdprod_mod_1981)$coefficients) %>%
     mutate(variable = rownames(.)) %>%
     mutate(crops_included = "Measured entire period"))
) %>%
  filter(!(variable %in% c("(Intercept)", "Year2000", "Year2010", "Year2020"))) %>%
  mutate(variable = factor(variable, 
                           levels = c("rm_shannons_usd", "ppt_instability_cv", "prop_irr", "tavg_instability_cv"),
                           labels = c("Diversity", "PPT instability", "Proportion irrigated", "Tavg instability")))


a <- ggplot(kcal_coeff_kcal, aes(x = reorder(variable, Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = (Estimate - 1.96*`Std. Error`), 
                      ymax = (Estimate + 1.96*`Std. Error`), 
                      color = crops_included), 
                  position=position_dodge(width=0.5)) + 
  theme_classic(base_size = 12) + 
  ylab("Effect on caloric production stability") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_discrete(limits = c("Diversity", "PPT instability", "Tavg instability", "Proportion irrigated")) +
  labs(tag = "(a)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.tag.position = c(0.16, 0.98), plot.tag = element_text(size = 12),
        legend.position = "none")

b <- ggplot(usd_coeff_usd, aes(x = reorder(variable, Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = (Estimate - 1.96*`Std. Error`), 
                      ymax = (Estimate + 1.96*`Std. Error`), 
                      color = crops_included), 
                  position=position_dodge(width=0.5)) + 
  theme_classic(base_size = 12) + 
  ylab("Effect on economic production stability") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = c(0.8,0.96), legend.background=element_rect(fill = alpha("white", 0)), 
        plot.tag.position = c(0.16, 0.98), plot.tag = element_text(size = 12)) + 
  scale_x_discrete(limits = c("Diversity", "PPT instability", "Tavg instability", "Proportion irrigated")) +
  labs(tag = "(b)", color = "")

a|b

ggsave(filename = "Fig S7 all subsets prod coefficients.png", path = "~/Desktop", units = "in", height = 5.5, width = 10)



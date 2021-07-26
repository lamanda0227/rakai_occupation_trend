library(tidyverse)
library(reshape2)


## 1. Load data
load("./data/sample_data.RData")
my_data <- sample_data %>%
  select(study_id, family_id, visit, radio, bicycle, motorcyc, car, latrine, electric, roof, floor, walls)




## 2. Recode
df_data <- my_data %>% 
  mutate(radio_r = ifelse(radio == 1, 1,ifelse(radio %in% c(2,3,5), 0, NA)),
         bicycle_r = ifelse(bicycle == 1, 1,ifelse(bicycle %in% c(2,3,5), 0, NA)),
         car_r = ifelse(car == 1, 1,ifelse(car %in% c(2,3,5), 0, NA)),
         motorcyc_r = ifelse(motorcyc == 1, 1,ifelse(motorcyc %in% c(2,3,5), 0, NA)),
         latrine_r = ifelse(latrine == 1, 1,ifelse(latrine %in% c(2,3,5), 0, NA)),
         electric_r = ifelse(electric == 1, 1,ifelse(electric %in% c(2,3,5), 0, NA)),
         roof_r = ifelse(roof %in% c(1,3), 1,ifelse(roof %in% c(2,4,5), 0, NA)),
         floor_r = ifelse(floor %in% c(2,3), 1,ifelse(floor %in% c(1,5), 0, NA)),
         walls_r = ifelse(walls %in% c(3), 1,ifelse(walls %in% c(1,2,4,5), 0, NA))) 




## 3. Calculate raw SES index
df_asset_data <- df_data %>% 
  select(study_id, visit, radio_r, bicycle_r, motorcyc_r, car_r, latrine_r, electric_r, roof_r, floor_r, walls_r) %>% 
  pivot_longer(cols = radio_r:walls_r, names_to = "variable", values_to = "status") %>%
  arrange(study_id, visit)

df_pca_overall = readxl::read_xlsx("./data/PCA_weights.xlsx") %>%
  select(variable, Overall) %>%
  rename(score = Overall) 

df_merge_overall = left_join(df_asset_data, df_pca_overall, by = 'variable') %>%
  arrange(study_id, visit) %>%
  mutate(SES = status * score) 

df_ovSES_data = df_merge_overall %>%
  arrange(study_id, visit) %>% 
  group_by(study_id, visit) %>% 
  summarise(SES = sum(SES), na.rm = F) %>%
  ungroup()

df_SES_data = left_join(df_ovSES_data, df_data, by = c('study_id', 'visit')) %>%
  mutate(SES = structure(SES, label = "raw SES score")) %>%
  arrange(study_id, visit) %>%
  select(-na.rm)




## 4. Create SES z-score and 4-level SES categorical variable
df_cut_overall = readxl::read_xlsx("./data/SES_cutpoints.xlsx") %>%
  select(cutpoints, Overall) %>%
  pivot_wider(names_from = cutpoints, values_from = Overall) %>%
  rename(Q1_zovSES = Q1_stdSES, Q2_zovSES = Q2_stdSES, Q3_zovSES = Q3_stdSES)

df_SES_data = df_SES_data %>%
  mutate(zovSES = structure((SES - df_cut_overall$mean_SES) / df_cut_overall$sd_SES, label = "Overall standardized SES"), 
         SEScat = case_when(zovSES <= df_cut_overall$Q1_zovSES ~ 0,
                            zovSES > df_cut_overall$Q1_zovSES & zovSES <= df_cut_overall$Q2_zovSES ~ 1,
                            zovSES > df_cut_overall$Q2_zovSES & zovSES <= df_cut_overall$Q3_zovSES ~ 2,
                            zovSES > df_cut_overall$Q3_zovSES ~ 3)) %>%
  #mutate(SEScat = structure(SEScat, label = "zovSES categories"))
  mutate(SEScat = structure(ordered(SEScat, labels = c("lowest", "low-middle", "high-middle", "highest")), label = "zovSES categories")) 

#SES.labels = c("lowest", "low-middle", "high-middle", "highest")




#5 - Round-specific SES scores and SES category
df_cut = readxl::read_xlsx("./data/SES_cutpoints.xlsx") %>%
  select(-Overall) %>%
  pivot_longer(col = 2:18, names_to = "visit", values_to = "value") %>%
  mutate(visit = parse_number(visit)) %>%
  pivot_wider(names_from = cutpoints, values_from = value)

df_SEScut_data = left_join(df_SES_data, df_cut, by = 'visit') %>%
  rename(rmean_SES = mean_SES, rsd_SES = sd_SES, rQ1_zrSES = Q1_stdSES, rQ2_zrSES = Q2_stdSES, rQ3_zrSES = Q3_stdSES)

df_rSES_data = df_SEScut_data %>%
  mutate(zrSES = structure((SES - rmean_SES) / rsd_SES, label = "round standardized SES"))

df_rSES_data = df_rSES_data %>%
  mutate(rSEScat = case_when(
    zrSES <= rQ1_zrSES ~ 0,
    zrSES > rQ1_zrSES & zrSES <= rQ2_zrSES ~ 1,
    zrSES > rQ2_zrSES & zrSES <= rQ3_zrSES ~ 2,
    zrSES > rQ3_zrSES ~ 3)) %>%
  #mutate(rSEScat = structure(rSEScat, label = "zrSES categories")) %>%
  mutate(rSEScat = structure(ordered(rSEScat, labels = c("lowest", "low-middle", "high-middle", "highest")), label = "zrSES categories")) %>%
  select(study_id, visit, zrSES, rSEScat)

df_SES_data = left_join(df_SES_data, df_rSES_data, by = c('study_id', 'visit')) %>%
  arrange(study_id, visit)

#export(df_SES_data, file = "./data/SES_data.csv" , format = "csv")
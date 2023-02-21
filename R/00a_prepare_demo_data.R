rm(list=ls())

library(tidyverse)

# Get diretory of script 
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

# Load demographic data of 2019 
demo <- read.csv("../../data_extra/demo.csv")

# Age groups 
age_breaks <- c(-1,5,16,50,65,85,110)

# Per municipality, gender and age group 
demo <- demo %>%
  filter(TX_RGN_DESCR_NL == "Vlaams Gewest") %>%
  rename(nis = CD_REFNIS,
         age = CD_AGE,
         sex = CD_SEX,
         population = MS_POPULATION) %>%
  dplyr::select(nis, age, sex, population) %>%
  mutate(age = cut(age, age_breaks)) %>%
  group_by(nis, age, sex) %>%
  summarise(population = sum(population)) %>%
  ungroup()

# Data about increased compensation per gender 
tegemoetkoming <- read.csv('../../data_extra/tegemoetkoming.csv', sep = ';')
tegemoetkoming$tegmoetkoming_man <- as.numeric(str_replace(tegemoetkoming$mannen.met.verhoogde.tegemoetkoming..t.o.v..mannen.in.de.ziekteverzekering., ',', '.'))
tegemoetkoming$tegmoetkoming_vrouw <- as.numeric(str_replace(tegemoetkoming$vrouwen.met.verhoogde.tegemoetkoming..t.o.v..vrouwen.in.de.ziekteverzekering., ',', '.'))
tegemoetkoming$nis <- tegemoetkoming$gebiedscode

tegemoetkoming <- tegemoetkoming %>%
  dplyr::select(nis, tegmoetkoming_man, tegmoetkoming_vrouw)

# vector met percentage deelnemers met verhoogde tegemoetkoming in deze categorie volgens de clean Intego - 2019 ycg

## Old by hand vectors: (not accurate, and very reproducible)
verhouding_tegemoetkoming_man <- c(0.156, 0.187, 0.113, 0.116,0.171,0.371)
verhouding_tegemoetkoming_vrouw <- c(0.156, 0.181, 0.128, 0.143, 0.263, 0.504)

## Reproducible by using the cleaned data 
data_lrti <- read.csv('../data/clean/data_lrti_2019.csv') %>% 
  select(-X)

data_age_gender <- data_lrti %>% 
  group_by(age_category, sex) %>% 
  summarize(n_patients = sum(npatients))

data_age_gender_verhoogde_tegemoetkoming <- data_lrti %>% 
  group_by(age_category, sex, increased_compensation) %>% 
  summarize(n_patients = sum(npatients)) %>% 
  left_join(data_age_gender, by = c("age_category","sex"), suffix = c(".vt",".total"))  %>% 
  group_by(age_category, sex, increased_compensation) %>% 
  summarize(p_yes = n_patients.vt / n_patients.total) %>% 
  filter(increased_compensation != "No") %>% 
  pivot_wider(names_from = sex, values_from = p_yes) %>% 
  mutate(age_group = factor(age_category, levels = c("(-1,5]","(5,16]","(16,50]",
                                                     "(50,65]","(65,85]","(85,110]"))) %>% 
  arrange(age_group)

male_increased_compensation <- data_age_gender_verhoogde_tegemoetkoming$M
female_increased_compensation <- data_age_gender_verhoogde_tegemoetkoming$F

# Adjust number male acording to intego 
demo_man <- demo %>%
  filter(sex == 'M') %>%
  pivot_wider(names_from = age,
              values_from = population) %>%
  left_join(tegemoetkoming)

man_verhoogde_tegemoetkoming <- demo_man[, 3:8] * rep(male_increased_compensation, each = nrow(demo_man)) * demo_man$tegmoetkoming_man / 100 * rowSums(demo_man[, 3:8]) / rowSums(demo_man[, 3:8] * rep(male_increased_compensation, each = nrow(demo_man)))
man_geen_verhoogde_tegemoetkoming <- demo_man[, 3:8] - man_verhoogde_tegemoetkoming 

demo_man <- rbind(man_verhoogde_tegemoetkoming %>%
                    mutate(sex = 'M',
                           verhoogde_tegemoetkoming = TRUE,
                           nis = demo_man$nis),
                  man_geen_verhoogde_tegemoetkoming %>%
                    mutate(sex = 'M',
                           verhoogde_tegemoetkoming = FALSE,
                           nis = demo_man$nis)) %>% 
  pivot_longer(cols = c('(-1,5]','(5,16]','(16,50]','(50,65]','(65,85]','(85,110]'),
               names_to = 'age',
               values_to = 'population')

# Adjust numbers female according to intego
demo_vrouw <- demo %>%
  filter(sex == 'F') %>%
  pivot_wider(names_from = age,
              values_from = population) %>%
  left_join(tegemoetkoming)

vrouw_verhoogde_tegemoetkoming <- demo_vrouw[, 3:8] * rep(female_increased_compensation, each = nrow(demo_vrouw)) * demo_vrouw$tegmoetkoming_vrouw / 100 * rowSums(demo_vrouw[, 3:8]) / rowSums(demo_vrouw[, 3:8] * rep(female_increased_compensation, each = nrow(demo_vrouw)))
vrouw_geen_verhoogde_tegemoetkoming <- demo_vrouw[, 3:8] - vrouw_verhoogde_tegemoetkoming 

demo_vrouw <- rbind(vrouw_verhoogde_tegemoetkoming %>%
                      mutate(sex = 'F',
                             verhoogde_tegemoetkoming = TRUE,
                             nis = demo_vrouw$nis),
                    vrouw_geen_verhoogde_tegemoetkoming %>%
                      mutate(sex = 'F',
                             verhoogde_tegemoetkoming = FALSE,
                             nis = demo_vrouw$nis)) %>%
  pivot_longer(cols = c('(-1,5]','(5,16]','(16,50]','(50,65]','(65,85]','(85,110]'),
               names_to = 'age',
               values_to = 'population')

demo <- rbind(demo_man,
              demo_vrouw)

write.csv(demo,"../data/clean/demo_data.csv")


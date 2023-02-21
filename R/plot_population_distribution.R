
# Introduction  -----------------------------------------------------------

# This sscript generates a (NIS) map thaht show the approximate locations of the 
# practices in the lrti 2019 clean data. 

rm(list=ls())

# Set working directory 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

# Check files 
list.files("../data/")


# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)


# Get data ----------------------------------------------------------------

data <- read.csv("../data/clean/data_lrti_2019.csv") %>% select(-X)
practice_locations <- read.csv("../data/practice_location.csv")
fl_pop <- readxl::read_excel("../data/TF_SOC_POP_STRUCT_2019.xlsx")

# # Keep practice in data 
practice_locations <- practice_locations %>%
  filter(practice_id %in% unique(data$practice_id))


# Map of practice locations -----------------------------------------------

# Load map of Flander 
fl <-st_read(dsn="../data/clean/",layer="map_fl")

# Plot 
# png(filename = "practice_locations.png")
ggplot()+
  geom_sf(data = st_geometry(fl), fill=NA,size=.4)+
  geom_point(data=practice_locations, 
             mapping = aes(x=lon.pc_mean,y=lat.pc_mean,size=npatients),
             # size = 4,
             pch=21,
             # stroke=1.2,
             # color="grey",
             fill='black',
             alpha=0.6)+
  theme_void()+
  scale_size("Number of patients")
# dev.off()

# Map of INTEGO population distribution -----------------------------------

# Aggregate Flanders population on NIS code 
names(fl_pop)
fl_pop_nis <- fl_pop %>% 
  filter(CD_REFNIS %in% fl$nis_code) %>% 
  rename(nis_code = CD_REFNIS) %>% 
  group_by(nis_code) %>% 
  summarise(flanders_pop = sum(MS_POPULATION)) %>% 
  ungroup()

# Aggregate intego data on nis code 
names(data)
intego_pop <- data %>% 
  group_by(nis_code) %>% 
  summarize(intego_pop = sum(npatients))

# Combine datasets
intego_fl <- intego_pop %>% 
  mutate(nis_code = as.character(nis_code)) %>% 
  left_join(fl_pop_nis, by="nis_code") %>% 
  mutate(int_fl_prop = intego_pop/flanders_pop*100) %>% 
  tidyr::replace_na(list(intego_pop = 0, int_fl_prop = 0))

# Add to sf object
fl_pop_map <- fl %>% 
  mutate(nis_code = as.character(nis_code)) %>% 
  left_join(intego_fl, by="nis_code")


names(fl_pop_map)

# Log scale 
ggplot(data = fl_pop_map, 
       mapping = aes(fill=int_fl_prop))+
  geom_sf()+
  scale_fill_continuous("Intego coverage (%)",trans="log10")+
  theme_void()+
  theme(plot.margin = margin(.1,.1,.1,.1, unit = "cm"))

# Linear scale
png(filename="intego_coverage_linear.png")
ggplot(data = fl_pop_map, 
       mapping = aes(fill=int_fl_prop))+
  geom_sf()+
  scale_fill_continuous("Intego coverage (%)", trans="log")+
  theme_void()
# theme(plot.margin = margin(.1,.1,.1,.1, unit = "cm"))  
dev.off()


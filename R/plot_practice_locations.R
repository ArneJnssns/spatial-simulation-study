
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

# Keep practice in data 
practice_locations <- practice_locations %>% 
  filter(practice_id %in% unique(data$practice_id))

# Compute number of practices per nis code (municipality)
test <- practice_locations %>% 
  group_by(nis_code) %>% 
  summarize(n_practices = n())

mean(test$n_practices)
median(test$n_practices)


# Map of practice locations -----------------------------------------------

# Load map of Flander 
fl <-st_read(dsn="../data/clean/",layer="map_fl")

# Plot 
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






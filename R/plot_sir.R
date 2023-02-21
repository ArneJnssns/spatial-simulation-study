

# Introduction ------------------------------------------------------------

# This script produces a map of the Standardized Incidence Ratio (SIR) of LRTI in
# 2019 in flanders based on indirect standardization. 

# Clean environment
rm(list=ls())

# Set working directory 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

# Cheeck files 
list.files("../data/clean")


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(SpatialEpi)
library(sf)


# Get data ----------------------------------------------------------------

# LRTI 2019
data <- read.csv("../data/clean/data_lrti_2019.csv") %>% select(-X)
names(data)

# Integrate out practice_id; aggregate on age, gender, increased_compensation
# - Strata
data <- data %>% 
  group_by(nis_code, sex, age_category, increased_compensation) %>% 
  summarise(n_cases = sum(ncases),
            n_patients = sum(npatients))

# Generate all combinations of strata (tidyr::crossing)
sex <- unique(data$sex[!is.na(data$sex)])
age_category <- unique(data$age_category[!is.na(data$age_category)],)
increased_compensation <- unique(data$increased_compensation[!is.na(data$increased_compensation)])

strata <- tidyr::crossing(sex,age_category,increased_compensation)

data_compl <- complete(data %>% 
                         group_by(nis_code),
                       strata,
                       fill = list(n_cases=0, n_patients=0)) %>% 
  drop_na()

# Compute expected count with help of SpatialEpi package 

## Arrange, important that all vectors correspond
data_compl <- data_compl %>% 
  arrange(nis_code,sex,age_category,increased_compensation)

## Expected count (stratified by age, gender and increased_compensation)
E <- expected(population = data_compl$n_patients,
              n.strata = 2*2*6,
              cases = data_compl$n_cases)

## Table with observed counts per community 
d <- data_compl %>% 
  group_by(nis_code) %>% 
  summarize(O = sum(n_cases))

## Add Expected counts 
d$E <- E

## Other way, but vectors are ok above 
#d$E2 <- E[match(d$nis_code,unique(data_compl$nis_code))]

## Compute SIR as Observed/Expected
d$SIR = d$O / d$E


# Map of SIR --------------------------------------------------------------

# Remark: in R on Mac it takes long to render maps..

# Load map of Flander 
fl <-st_read(dsn="../data/clean/",layer="map_fl")

# Plot geometry ()
ggplot(data = st_geometry(fl))+
  geom_sf()

# Add SIR for each community and create categories to plot 
names(fl)
fl_sir <- fl %>% 
  left_join(d %>% 
              mutate(SIR = cut(SIR,
                               c(0,0.5,0.6,0.8,1.2,1.5,2,25), 
                               include.lowest = T,
                               right=F)), by = "nis_code")
names(fl_sir)

# Remove Herstappe, no patients 
fl_sir <- fl_sir %>% drop_na(SIR)

# Create the map using ggplot
# tiff(filename = "~/Desktop/sir.tiff", res=300)
ggplot(data = fl_sir, aes(fill=SIR))+
  geom_sf()+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)   # , breaks = c(0,0.5,0.6,0.8,1.2,1.5,2,25))
  # labs(title = "SIR of LRTI in 2019")
# dev.off()

# Save the figure by making a sscreenshot (better quality)



# Some other code fr omthe course Spatial Epidemiology
# library(RColorBrewer)
# nclr <- 8
# plotvar <- fl_sir$SIR
# plotcol <- rev(brewer.pal(nclr,"RdYlGn"))
# class <- classInt::classIntervals(plotvar,nclr,style="fixed",fixedBreaks= c(0,0.5,0.6,0.8,1.2,1.5,2,25))
# colcode <- findColours(class,plotclr)




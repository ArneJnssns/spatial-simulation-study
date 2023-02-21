################################################################################
# Load raw data (data from server), clean and saved cleaned data for later use
# - This is the newer version of older script check_downloaded_data.R
# - Created on 13-01-2021
################################################################################

# Cleaning: 
# - Remove communities without patients (this can be done because Herstappe is at the edge of Flanders?)
# -- Discussed wiht Thomas and better to keep them 
# -- Herstappe can be merged with Tongeren, but for now it is kept 
# - Remove Voeren because no adjacent neighbors in the map (strange for the model to model it like this then?)
# -- Discussed with Thomas and keep it, it just has no neighbours 

rm(list=ls())

# Packages
library(tidyverse)
library(spdep)
library(sf)

# Working directory
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

# Data locations
dir_data_raw <- "../data/downloaded_from_server/"
dir_data_clean <- "../data/clean/"
dir_output <- "../output_model_fit_case/"
dir_map <- "/Users/u0121893/OneDrive - KU Leuven/Spatial_Data/Shape_Files/Flanders/Shapefile/"


# INTEGO DATA LRTI 2019
#-------------------------------------------------------------------------------

# Aggregated data
data_raw <- read.csv(paste0(dir_data_raw,"inla.data.csv"),sep=",") %>% 
  mutate(NAAM = toupper(NAAM))
str(data_raw)

# Missing data?
missing_data <- data_raw %>%  
  filter(is.na(npatients)) %>% 
  pull("NAAM")

# # Remove Herstappe because no patients 
# data_complete <- data_raw %>% 
#   filter(!NAAM %in% missing_data)

# Instead of removing, join with Tongeren --> how ? 

# Now: keep it, in estimating the effects, it will be smoothed by the Tongeren effect I expect 
# - Only a problem when making predictions on this exact data becasue no patients at all 
data_complete <- data_raw

# # Remove Voeren becasue no 'true' neighbouring neighbors 
# data_complete_nb <- data_complete %>% 
#   filter(NAAM != "VOEREN")

# Do not remove Voeren, just no neighbours in analysis 
data_complete_nb <- data_complete 

unique(data_complete_nb$NAAM)

# Make ID variable (to be used in inla model)
nis_id <- tibble(nis_code = unique(data_complete_nb$nis_code)) %>% 
  arrange(nis_code) %>% 
  mutate(ID = row_number())

data_complete_nb_id <- data_complete_nb %>% 
  select(-ID,-X) %>% 
  left_join(nis_id, by = "nis_code")

# Data to model
data_clean <- data_complete_nb_id %>% 
  select(nis_code,
         ID,
         sex,
         age_category = age.category,
         increased_compensation,
         practice_id,
         ncases,
         npatients)

str(data_clean)

# Save cleaned data as csv file 
write.csv(data_clean, paste0(dir_data_clean,"data_lrti_2019.csv"))



# # Model estimates from server
# model_estimates.server <- read.csv(paste0(dir_data_raw,"binom_point_estimates.csv"))


# MAP OF FLANDERS (WO VOEREN & HERSTAPPE)
#-------------------------------------------------------------------------------

# Map of Flanders
map_fl <- st_read(dsn = dir_map,layer="Refgem") %>% 
  mutate(NAAM = toupper(NAAM))
str(map_fl)

# Clean the map for use in model

# ## Filter out missing data (HERSTAPPE) and VOEREN 
# map_fl_complete_nb <- map_fl %>% 
#   filter(!NAAM %in% c("HERSTAPPE","VOEREN"))

# -> Do not filter out Herstappe and Voeren 
map_fl_complete_nb <- map_fl

# # Plot boundaries of municipalities 
# plot(st_geometry(map_fl))

## Order the IDs
map_fl_clean <- map_fl_complete_nb %>% 
  mutate(NISCODE = as.integer(NISCODE)) %>% 
  arrange(NISCODE) %>% 
  left_join(nis_id, by = c("NISCODE" = "nis_code")) %>% 
  select(nis_code = NISCODE,
         name = NAAM,
         ID)

class(map_fl_clean)

# Save cleaned map 
st_write(map_fl_clean, paste0(dir_data_clean,"map_fl.shp"), delete_layer = T)


# Create file used in the model 

##  Neighbourhood matrix 
map_nb <- poly2nb(map_fl_clean,row.names=map_fl_clean$ID,queen=TRUE)
pdf(file = paste0(dir_output,"neighbors_map.pdf"), width = 400,height=350)
plot(st_geometry(map_fl_clean))
plot.nb(map_nb,coords = st_geometry(st_centroid(map_fl_clean)),add=TRUE,col="red")
dev.off()

## Save neighborhood matrix
saveRDS(map_nb,paste0(dir_data_clean,"flanders_nb_matrix.rds"))

## Save for model use 
nb2INLA(file=paste0(dir_data_clean,"map.gra"),map_nb)

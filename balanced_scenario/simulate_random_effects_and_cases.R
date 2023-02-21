
# 30/01/2023:
# - Run code again with new parameter estiamtes when using PC prior for spatial
#   and practice effect 

# Simulate data and save data for later use 

# Based on 10_simulation_different_....R

rm(list=ls())

library(SUMMER)
library(spdep)
library(dplyr)
library(tibble)
library(stringr)

# READ DATA
#------------------------------------------------------------------------------

# Working directory and others 
# wd <- "/Users/u0121893/OneDrive - KU Leuven/PhD/Research/Simulation/spatial/simulation-study-git/"
# setwd(wd)

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

dir_data_clean <- "../data/clean/"
dir_output <- "../output_model_fit_case/"

dir_simulated_and_support <- "../../github-repo-data/balanced_scenario/"

# Source useful scripts 
source("../R/functions_inla.R")
source("../R/functions_geo.R")

# Load the cleaned data 
data_inla <- read.csv(paste0(dir_data_clean,"data_lrti_2019.csv")) %>% select(-X)
map_fl <- st_read(dir_data_clean, layer  = "map_fl")
map_nb <- readRDS(paste0(dir_data_clean,"flanders_nb_matrix.rds"))

# Demographic data we need to construct our final dataset
demo <- read.csv(paste0(dir_data_clean,"demo_data.csv")) %>% 
  select(-X) %>% 
  filter(nis %in% unique(map_fl$nis_code))

# Initial dataframe with one practice per community 
nis_practice_og <- data.frame(nis=unique(demo$nis)) %>% 
  arrange(nis) %>% 
  rownames_to_column("nis_id") %>% 
  mutate(practice_id = 1)

# Model point estimates to use
est_fixed <- read.csv(paste0(dir_output,"02_fixed.csv")) %>% select(-X) %>% 
  mutate(param = str_remove(param,"sex|age_category|increased_compensation")) %>% 
  bind_rows(tibble(param = c("(16,50]","F","No"),
                   mean = rep(0,3))) 
estimates_constant <- est_fixed %>% select(param, mean) %>% 
  column_to_rownames(var="param")

# Load precision estimates 
precisions <- read.csv(paste0(dir_output,"02_precisions.csv"))
precision_spatial <- precisions %>% filter(X == "Precision for ID") %>% pull(mean)
precision_practice <- precisions %>% filter(X == "Precision for practice_id") %>% pull(mean)

# Compute standard deviations of random effects using the precision
stdev_spatial_est <- 1/sqrt(precision_spatial)
stdev_practice_est <- 1/sqrt(precision_practice)

# Neighborhood matrix 
nb_matrix <- nb2mat(map_nb,style="B",zero.policy = TRUE) 




# PARAMETERS
#-------------------------------------------------------------------------------

# Three varying parameters simulation on 11/04/2022

sd_practice <- c(0.1,0.3,0.6) # 0.6 ≈ stdev_practice_est
n_practices <- c(1,2,3) 

sd_spatial <- c(0.1, 0.3, 0.6) # 0.1 ≈ stdev_spatial_est

n_simulations <- 150 # number of simulations per practice parameter combination 

n_com <- n_distinct(demo$nis) # Number of communities --> now 298 becasue Voeren en Herstappe were removed --> do not do this 




# SIMULATE PRACTICE EFFECTS 
#-------------------------------------------------------------------------------

# Random seed + set seed (recommended to set seed as few times as possible https://www.stata.com/manuals13/rsetseed.pdf)
# seed <- as.integer(Sys.time()) %% 10000
# seed_practice <- read.table(paste0(dir_output,"06_seed_practice.txt"))$V1
# write.csv(seed,file = paste0(dir_simulated_and_support,"seed_effect_simulation.csv"))
seed <- read.csv(paste0(dir_simulated_and_support,"seed_effect_simulation.csv"))$x
set.seed(seed)

# Initialize empty list to store simulated practice effects 
practice_effects <-list()

# Empty list to store intermediate data frames
practices_per_community = list()

spatial_sim_list <- list()

# Empty "track" matrix 
track_matrix <- matrix(nrow = length(sd_spatial)*length(sd_practice)*length(n_practices)*n_simulations, ncol = 5)
colnames(track_matrix) <- c("sd_practice","n_practices","sd_spatial","simulation","nth_data")

# Initialization
np  <- 0
nd <- 0
param_comb <- 0

# Start timer
start_sim <- Sys.time()

# Outer for loop over number of practice per community 
for(n_practices_select in n_practices){
  
  # 
  np=np+1
  
  # Add new practice ids to list 
  practices_per_community[[n_practices_select]] <- nis_practice_og %>% 
    mutate(practice_id = n_practices_select,
           practice_id = paste(nis_id,practice_id,sep="_"))
  
  # Rbind all dataframes with required number of practices 
  practices_per_community_df <- do.call(rbind,practices_per_community)
  
  # Inner loop to change practice variance 
  sp <- 0
  practices_simulated_list <- list()
  
  for(sd_practice_select in sd_practice){
    
    for(sd_spatial_select in sd_spatial){
      
      # Much faster using the n = n_simulations in rst 
      param_comb <- param_comb+1
      
      spatial_sim_list[[param_comb]] <- as_tibble(t(rst(n = n_simulations,
                                                        type = "s",
                                                        Amat = nb_matrix, 
                                                        scale.model = T) * sd_spatial_select)) %>% 
        rowid_to_column("ID") %>% 
        tidyr::pivot_longer(-ID, names_to = "simulation",values_to = "simulated") %>% 
        mutate(n_practices = n_practices_select,
               sd_practice = sd_practice_select,
               sd_spatial = sd_spatial_select,
               simulation = as.integer(gsub(pattern = "V",replacement = "",simulation)))
      
      for(j in 1:n_simulations){
        
        nd <- nd+1
        
        sp <- sp+1
        
        # Simulate practice effect using rnorm with mean 0 and standard deviation stdev_practice
        simRE_pract <- rnorm(n = n_practices_select*n_com, mean = 0, sd = sd_practice_select)
        
        # Add the simulated effects to the dataframe of practice ids 
        practices_per_community_df$simulated <- simRE_pract
        
        # Add dataframe with simulated practice effect to list in order to save
        practices_simulated_list[[sp]] <- practices_per_community_df %>% 
          mutate(n_practices = n_practices_select,
                 sd_practice = sd_practice_select,
                 sd_spatial = sd_spatial_select,
                 simulation = j,
                 nth_data = nd)
        
        # Track matrix 
        track_matrix[nd,"sd_practice"] <- sd_practice_select
        track_matrix[nd,"n_practices"] <- n_practices_select
        track_matrix[nd,"sd_spatial"] <- sd_spatial_select
        track_matrix[nd,"simulation"] <- j
        track_matrix[nd,"nth_data"] <- nd
        
        # Indicate progress in console
        cat("N practices:",n_practices_select,"; SD practice:",sd_practice_select,
            "; SD spatial:", sd_spatial_select,
            "; simulation ",j,"/",n_simulations,"; duration:",round(Sys.time() - start_sim,2),"\n")
        
      }
    }
  }
  # Collect all simulated effects in a list 
  practice_effects[[np]] <- do.call(rbind,practices_simulated_list)
  
} # 150 simulaties x 27 = 4050 takes ≈ 4'

# Create dataframe of all practice effect per scenario 
practice_effects_df <- as_tibble(do.call(rbind,practice_effects)) %>% 
  rename(nis_code = nis, ID = nis_id) %>% 
  mutate(ID = as.integer(ID))

track_matrix <- as_tibble(track_matrix)

spatial_effects_df <- do.call(rbind,spatial_sim_list) %>% 
  left_join(track_matrix, by = c("sd_spatial","sd_practice","n_practices","simulation"))


write.csv(practice_effects_df, file = paste0(dir_simulated_and_support,"simulated_practice_effects.csv"))
write.csv(spatial_effects_df, file = paste0(dir_simulated_and_support,"simulated_spatial_effects.csv"))
write.csv(track_matrix, file = paste0(dir_simulated_and_support,"track_matrix.csv"))

# # Double check 
# table(track_matrix$sd_practice)
# table(track_matrix$n_practices)
# table(track_matrix$sd_spatial)
# table(track_matrix$simulation)
# table(track_matrix$nth_data)
# 
# table(spatial_effects_df$sd_practice)
# table(spatial_effects_df$n_practices)
# table(spatial_effects_df$sd_spatial)
# table(spatial_effects_df$simulation)
# table(spatial_effects_df$nth_data)
# 
# table(practice_effects_df$sd_practice)
# table(practice_effects_df$n_practices)
# table(practice_effects_df$sd_spatial)
# table(practice_effects_df$simulation)
# table(practice_effects_df$nth_data)


# PREP DATA
#-------------------------------------------------------------------------------


# ID for arranged nis codes
demo <- demo %>% group_by(nis) %>% 
  mutate(ID = cur_group_id()) %>% 
  ungroup()

# Preprocess
demo <- demo %>%
  select(nis,ID,sex,age,verhoogde_tegemoetkoming,population) %>%
  rename(nis_code=nis,age_category=age,increased_compensation = verhoogde_tegemoetkoming)

# Constant effects (fixed + spatial)
demo_effects <- demo %>%
  # Change to characters
  mutate_if(is.factor,as.character) %>%
  mutate(ID=as.character(ID),
         increased_compensation = ifelse(increased_compensation,"Yes",'No')) %>%
  # Compute linear predictor
  mutate(alpha_constant = estimates_constant["(Intercept)",]+
           estimates_constant[sex,]+
           estimates_constant[age_category,]+
           estimates_constant[increased_compensation,])

# Orders the rows to link it to the dataframe of simulated cases
data_ar <- demo_effects %>%
  arrange(nis_code,sex,age_category,increased_compensation)



# SIMULATION of DATA 
#-------------------------------------------------------------------------------

# Use simulate effects to simulate the number of cases 

set.seed(seed)

simulated_props <- list()
data_simulated_n_cases <- list()

start <- Sys.time()
for(n_data in 1:nrow(track_matrix)){
  
  sd_practice_select <- track_matrix[[n_data,"sd_practice"]]
  n_practices_select <- track_matrix[[n_data,"n_practices"]]
  sd_spatial_select <- track_matrix[[n_data,"sd_spatial"]]
  simulation_select <- track_matrix[[n_data,"simulation"]]
  
  data_ar_practice <- data_ar %>%
    mutate(n_patients = round(population /
                                n_practices_select,
                              0))
  
  practice_effects_select <- practice_effects_df %>% 
    filter(nth_data == n_data)
  
  spatial_effects_select <- spatial_effects_df %>% 
    filter(nth_data == n_data)
  
  # Compute linear predictor and probability to simulate, using available data
  data_ar_prob <- data_ar_practice %>%
    mutate(ID = as.integer(ID)) %>% 
    left_join(practice_effects_select %>% 
                select(-c("n_practices","sd_practice","simulation","nth_data")),
              by = c("nis_code","ID")) %>% 
    left_join(spatial_effects_select %>% 
                select(-c("n_practices","sd_practice","simulation","nth_data")), 
              by='ID',
              suffix = c(".practice",".spatial")) %>% 
    mutate(lin_pred  = alpha_constant + simulated.practice + simulated.spatial,
           prob_binom = exp(lin_pred) / (1+exp(lin_pred))) %>%
    mutate(age_category=factor(age_category,
                               levels=c("(16,50]","(-1,5]","(5,16]","(50,65]","(65,85]","(85,110]")),
           ID = as.integer(ID))
  
  
  # Get the simulated probabilities (proportions) 
  simulated_props_tmp <- data.frame(sim_props = data_ar_prob$prob_binom) %>% 
    mutate(sim_ncases = NA, nth_data = n_data) %>% 
    rowid_to_column("predictor")
  
  # Simulate number of cases using the proportions; add to simulated proportions data frame
  for(rb in 1:nrow(data_ar_prob)){
    simulated_props_tmp[rb,"sim_ncases"] = rbinom(n = 1,
                                                  size = data_ar_prob$n_patients[rb],
                                                  prob = data_ar_prob$prob_binom[rb])
  }
  
  # simulated_props[[n_data]] <- simulated_props_tmp 
  # Instead of appending to list, save each soimualted dataset as csv file (avoid memory accumulation)
  write.csv(simulated_props_tmp, file = paste0(dir_simulated_and_support,
                                               "simulated_cases/",
                                               "simulated_cases_propportions_",n_data,".csv"))
  
  # data_simulated_n_cases[[n_data]]  <-
  simulated_data <- data_ar_prob %>% 
    mutate(n_cases = simulated_props_tmp$sim_ncases,
           nth_data = n_data) %>% 
    select(-c("population","alpha_constant","simulated.practice","simulated.spatial",
              "lin_pred","prob_binom"))
  write.csv(simulated_data, file = paste0(dir_simulated_and_support,
                                               "simulated_data/",
                                               "simulated_data_",n_data,".csv"))
  
  cat("N data:", n_data,
      "; Time elapsed:", round(Sys.time()-start,2), "\n")
  
} # 4050 datasets = 1.4 hours 

## Code below not needed becasue now each simulated datasets and ncases is saves seperately in a csv file 
# simulated_props_df <- do.call(rbind,simulated_props) %>% 
#   left_join(track_matrix, by="nth_data")
# data_simulated_n_cases_df <- do.call(rbind,data_simulated_n_cases) %>% 
#   left_join(track_matrix, by="nth_data")

# write.csv(simulated_props_df, file = paste0(dir_simulated_and_support, "simulated_proportions.csv"))
# write.csv(data_simulated_n_cases_df, file = paste0(dir_simulated_and_support,"simulated_data.csv")) 





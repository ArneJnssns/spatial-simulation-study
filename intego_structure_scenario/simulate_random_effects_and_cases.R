
# 06/12/2022
# 30/01/2023
# - Simuleer alle data opnieuw met nieuwe schattingen waarbij we gebruik maken 
#   van de PC prios voor spatiaal en praktijk effect 


# Introduction ------------------------------------------------------------

# Simulation of "intego structure" data (instead of "balanced" data)
# Also on GitHub now in the folder ('intego_structure')

rm(list=ls())



# Libraries ---------------------------------------------------------------

library(SUMMER)
library(spdep)
library(dplyr)
library(tibble)
library(stringr)


# READ DATA
#------------------------------------------------------------------------------

# Note that this part is always needed --> soruce a script of make a function?) 

# Working directory and others 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

dir_data_raw <- "../data/downloaded_from_server/"
dir_data_clean <- "../data/clean/"
dir_output <- "../output_model_fit_case/"
# dir_output_large <- "../simulation-study-output-not-git/"

# dir_simulated_and_support <- "../hpc_wf/intego_structure/"
dir_simulated_and_support <- "../../github-repo-data/intego_structure/"

# Source useful scripts 
source("../R/functions_inla.R")
source("../R/functions_geo.R")

# Load the cleaned data 
data_inla <- read.csv(paste0(dir_data_clean,"data_lrti_2019.csv")) %>% select(-X)
map_fl <- st_read(dir_data_clean, layer  = "map_fl")
map_nb <- readRDS(paste0(dir_data_clean,"flanders_nb_matrix.rds"))

# # Demographic data we need to construct our final dataset - NOT NEEDED NOW 
# demo <- read.csv(paste0(dir_data_clean,"demo_data.csv")) %>% 
#   select(-X) %>% 
#   filter(nis %in% unique(map_fl$nis_code))
# 
# # Initial dataframe with one practice per community 
# nis_practice_og <- data.frame(nis=unique(demo$nis)) %>% 
#   arrange(nis) %>% 
#   rownames_to_column("nis_id") %>% 
#   mutate(practice_id = 1)

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

# Two varying parameters simulation

sd_practice <- c(0.1,0.3,0.6) # 0.6 ≈ stdev_practice_est
# n_practices <- c(1,2,3) 

sd_spatial <- c(0.1, 0.3, 0.6) # 0.1 ≈ stdev_spatial_est

n_simulations <- 150 # number of simulations per practice parameter combination 

n_com <- n_distinct(data_inla$nis_code) 
n_practices <- n_distinct(data_inla$practice_id, na.rm=T)


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

# Tible of with column practice_id
practice_id_df <- tibble(practice_id = unique(na.omit(data_inla$practice_id)))

spatial_sim_list <- list()

# Empty "track" matrix 
track_matrix <- matrix(nrow = length(sd_spatial)*length(sd_practice)*n_simulations, ncol = 4)
colnames(track_matrix) <- c("sd_practice","sd_spatial","simulation","nth_data")

# Initialization
nd <- 0
param_comb <- 0

# Start timer
start_sim <- Sys.time()


for(sd_practice_select in sd_practice){
  
  # Use the tibblae to add siulated values to 
  practices_sim_df <- practice_id_df
  
  for(sd_spatial_select in sd_spatial){
    
    # Much faster using the n = n_simulations in rst 
    param_comb <- param_comb+1
    
    spatial_sim_list[[param_comb]] <- as_tibble(t(rst(n = n_simulations,
                                                      type = "s",
                                                      Amat = nb_matrix, 
                                                      scale.model = T) * sd_spatial_select)) %>% 
      rowid_to_column("ID") %>% 
      tidyr::pivot_longer(-ID, names_to = "simulation",values_to = "simulated") %>% 
      mutate(sd_practice = sd_practice_select,
             sd_spatial = sd_spatial_select,
             simulation = as.integer(gsub(pattern = "V",replacement = "",simulation)))
    
    for(j in 1:n_simulations){
      
      nd <- nd+1
      
      
      # Simulate practice effect using rnorm with mean 0 and standard deviation stdev_practice
      simRE_pract <- rnorm(n = n_practices, mean = 0, sd = sd_practice_select)
      
      # Add the simulated effects to the dataframe of practice ids 
      practices_sim_df$simulated <- simRE_pract
      
      # Add dataframe with simulated practice effect to list in order to save
      practice_effects[[nd]] <- practices_sim_df %>% 
        mutate(sd_practice = sd_practice_select,
               sd_spatial = sd_spatial_select,
               simulation = j,
               nth_data = nd)
      
      # Track matrix 
      track_matrix[nd,"sd_practice"] <- sd_practice_select
      # track_matrix[nd,"n_practices"] <- n_practices_select
      track_matrix[nd,"sd_spatial"] <- sd_spatial_select
      track_matrix[nd,"simulation"] <- j
      track_matrix[nd,"nth_data"] <- nd
      
      # Indicate progress in console
      cat("SD practice:",sd_practice_select,
          "; SD spatial:", sd_spatial_select,
          "; simulation ",j,"/",n_simulations,"; duration:",round(Sys.time() - start_sim,2),"\n")
      
    }
  }
} # 10 seconds per parameeter combination - 90"

# Create dataframe of all practice effect per scenario 
practice_effects_df <- as_tibble(do.call(rbind,practice_effects)) 

track_matrix <- as_tibble(track_matrix)

spatial_effects_df <- do.call(rbind,spatial_sim_list) %>% 
  left_join(track_matrix, by = c("sd_spatial","sd_practice","simulation"))


write.csv(practice_effects_df, file = paste0(dir_simulated_and_support,"simulated_practice_effects.csv"))
write.csv(spatial_effects_df, file = paste0(dir_simulated_and_support,"simulated_spatial_effects.csv"))
write.csv(track_matrix, file = paste0(dir_simulated_and_support,"track_matrix.csv"))

# # Double check 
# table(track_matrix$sd_practice)
# table(track_matrix$sd_spatial)
# table(track_matrix$simulation)
# table(track_matrix$nth_data)
# 
# table(spatial_effects_df$sd_practice)
# table(spatial_effects_df$sd_spatial)
# table(spatial_effects_df$simulation)
# table(spatial_effects_df$nth_data)
# 
# table(practice_effects_df$sd_practice)
# table(practice_effects_df$n_practices)
# table(practice_effects_df$simulation)
# table(practice_effects_df$nth_data)


# PREP DATA
#-------------------------------------------------------------------------------

# Constant effects (fixed + spatial)
data_to_use <- data_inla %>%
  # Change to characters
  mutate_if(is.factor,as.character) %>%
  mutate(ID=as.character(ID)) %>%
  # Compute linear predictor
  mutate(alpha_constant = estimates_constant["(Intercept)",]+
           estimates_constant[sex,]+
           estimates_constant[age_category,]+
           estimates_constant[increased_compensation,])

# Orders the rows to link it to the dataframe of simulated cases
data_ar <- data_to_use %>%
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
  sd_spatial_select <- track_matrix[[n_data,"sd_spatial"]]
  simulation_select <- track_matrix[[n_data,"simulation"]]
  
  practice_effects_select <- practice_effects_df %>% 
    filter(nth_data == n_data)
  
  spatial_effects_select <- spatial_effects_df %>% 
    filter(nth_data == n_data)
  
  # Compute linear predictor and probability to simulate, using available data
  data_ar_prob <- data_ar %>%
    mutate(ID = as.integer(ID)) %>% 
    left_join(practice_effects_select %>% 
                select(-c("sd_practice","simulation","nth_data")),
              by = c("practice_id")) %>% 
    left_join(spatial_effects_select %>% 
                select(-c("sd_practice","simulation","nth_data")), 
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
                                                  size = data_ar_prob$npatients[rb],
                                                  prob = data_ar_prob$prob_binom[rb])
  }
  
  # simulated_props[[n_data]] <- simulated_props_tmp 
  # Instead of appending to list, save each soimualted dataset as csv file (avoid memory accumulation)
  write.csv(simulated_props_tmp, file = paste0(dir_simulated_and_support,
                                               "simulated_cases/",
                                               "simulated_cases_proportions_",n_data,".csv"))
  
  # data_simulated_n_cases[[n_data]]  <-
  simulated_data <- data_ar_prob %>% 
    mutate(n_cases = simulated_props_tmp$sim_ncases,
           nth_data = n_data) %>% 
    select(-c("alpha_constant","simulated.practice","simulated.spatial",
              "lin_pred","prob_binom"))
  write.csv(simulated_data, file = paste0(dir_simulated_and_support,
                                          "simulated_data/",
                                          "simulated_data_",n_data,".csv"))
  
  cat("N data:", n_data,
      "; Time elapsed:", round(Sys.time()-start,2), "\n")
  
} # 1350 datasets = 40 minutes 

## Code below not needed becasue now each simulated datasets and ncases is saves seperately in a csv file 
# simulated_props_df <- do.call(rbind,simulated_props) %>% 
#   left_join(track_matrix, by="nth_data")
# data_simulated_n_cases_df <- do.call(rbind,data_simulated_n_cases) %>% 
#   left_join(track_matrix, by="nth_data")

# write.csv(simulated_props_df, file = paste0(dir_simulated_and_support, "simulated_proportions.csv"))
# write.csv(data_simulated_n_cases_df, file = paste0(dir_simulated_and_support,"simulated_data.csv")) 





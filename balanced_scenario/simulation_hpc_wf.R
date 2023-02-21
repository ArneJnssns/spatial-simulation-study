# PC prior (30/01/2023)

# Set path to R_packages on HPC $VSC_DATA directory 
.libPaths("../../R_packages")

# Load packages 
library(dplyr)
library(INLA)
library(tibble)

# Function to run per parameter combination 
fit_inla_on_simulated_data <- function(){
  
  # Command line arguments = parameter combinations(s)
  cat("\nGET PARAMETER VALUES \n")
  args <- commandArgs(trailingOnly = TRUE)
  sd_practice_arg <- args[1]
  n_practices_arg <- args[2]
  sd_spatial_arg <- args[3]
  
  cat("SD practice is",args[1],"\n")
  cat("N practices is",args[2],"\n")
  cat("SD spatial is",args[3],"\n\n")
  
  # Get the functions to extract results from fitted ila object
  source("./extract_and_save_results_from_inla_functions.R")
  
  # Read track matrix data 
  cat("TRACK MATRIX \n")
  track_matrix <- read.csv(file = "track_matrix.csv") %>% 
    select(-X) %>% 
    filter(sd_practice == sd_practice_arg,
           n_practices == n_practices_arg,
           sd_spatial == sd_spatial_arg)
  
  cat("MODEL FORMULA \n\n")
  formula <- n_cases ~ sex + age_category + increased_compensation  +
    f(ID, model="besag",graph="map.gra" ,scale.model=T,
      hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01))))+
    f(practice_id,model="iid",
      hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01))))
  
  
  cat("LOOP OVER SIMULATED DATASETS\n")
  for(n_data in track_matrix$nth_data){
    
    # Read data
    simulated_data <- read.csv(paste0("simulated_data/simulated_data_",n_data,".csv")) %>% 
      select(-X) %>% 
      mutate(age_category=factor(age_category,
                                 levels=c("(16,50]","(-1,5]","(5,16]","(50,65]","(65,85]","(85,110]")),
             ID = as.integer(ID))
    
    # Fit model
    start <- Sys.time()
    inla_result <- inla(formula,
                     family="binomial",
                     data = simulated_data,
                     Ntrials = n_patients, # N Bernouilli experiments
                     num.threads = 1
                     # control.predictor = list(compute = T, link=1), # Compute fitted values (proportions)
                     # control.compute=list(waic=T,dic=T,config=T)
    )
    end <- Sys.time()
    d <- difftime(end,start,units = "mins")
    df_d <- tibble(time_diff = d, nth_data = n_data)
    # save time to fit model
    write.csv(df_d, paste0("./output/time/model_fit/",n_data,".csv"))
    
    # Extract results from model
    start <- Sys.time()
    save_results_inla(inla_result,n_data)
    end <- Sys.time()
    d <- difftime(end, start, units = "mins")
    df_d <- tibble(time_diff = d, nth_data= n_data)
    write.csv(df_d, paste0("./output/time/extract_results/",n_data,".csv"))
    
    
  }
}

fit_inla_on_simulated_data()
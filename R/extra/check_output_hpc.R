
rm(list=ls())

library(magrittr)
library(dplyr)
library(ggplot2)
library(INLA)
library(tibble)

# Directory where this script is located (on github hpc_wf)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get the function to save data from inla objects (developed to be used on the HPC in the 02_... analyses)
getwd()
source("extract_and_save_results_from_inla_functions.R")

# Change working directory to the one not on github (to save multiple larger data files) 
setwd("../../hpc_wf/01_data_simulated_and_support_from_two_varying_practice_parameters")
getwd()

# All files present in directory 
files <- list.files(recursive = T)

# Get names of inla object files 
inla_object_files <- files[grep("inla_object",files)]
## Order according to n_data (use readr::parse_number to quickly get number of n_data from filename )
inla_object_files <- inla_object_files[order(readr::parse_number(inla_object_files))]
# inla_object_files <- list.files("inla_objects")

# Test small for loop --> works 
start_test <- Sys.time()
for(i in inla_object_files[1:2]){
  
  # print(i)
  n_data <- readr::parse_number(i)
  # print(n_data)
  inla_obj <- readRDS(i)
  
  save_results_inla(inla_obj,n_data)
  
  cat("N data:",n_data,"; Elapsed Time:",round(difftime(Sys.time(),start_test,units = "mins"),2),"minutes\n")
}

# ---- 

# The entire loop (900 inla object) --> + 3 hours 
start <- Sys.time()
for(i in inla_object_files[129:length(inla_object_files)]){
  
  # print(i)
  n_data <- readr::parse_number(i)
  # print(n_data)
  inla_obj <- readRDS(i)
  
  save_results_inla(inla_obj,n_data)
  
  cat("N data:",n_data,"; Elapsed Time:",round(difftime(Sys.time(),start,units = "mins"),2),"minutes\n")
}

# Error at inla_object 120 
inla_obj_120 <- readRDS(inla_object_files[120])
summary(inla_obj_120)
## error in 
pmarg_sd <- lapply(inla_obj_120$marginals.hyperpar, function(x) inla.tmarginal(function(tau) tau^(-1/2), x, n = 400))
t = tibble(inla.tmarginal(function(tau) tau^(-1/2), 
                          inla_obj_120$marginals.hyperpar$`Precision for ID`, n = 400))
t_practice <- inla.tmarginal(function(tau) tau^(-1/2),inla_obj_120$marginals.hyperpar$`Precision for practice_id`, n= 400)
t_ID <- inla.tmarginal(function(tau) tau^(-1/2),inla_obj_120$marginals.hyperpar$`Precision for ID`)
sum(is.na(inla_obj_120$marginals.hyperpar$`Precision for ID`))
plot(inla_obj_120$marginals.hyperpar$`Precision for ID`^(-1/2))

# - Problem is in de precision for ID, but cannot find where 

# Error in inla_object_129
inla_obj_129 <- readRDS(inla_object_files[129])
## Error at marginals of precision (hyperpar) --> Inf values --> this is probably the error 
prec_marg_129 <- inla_obj_129$marginals.hyperpar$`Precision for ID`
plot(prec_marg_129)
## NEED TO AVOID JUST STOPPING THE FOR LOOP OF COURSE --> How ? 
## - if this will be on the HPC, a lot of simulation won't be done 


# ----





# Extract the log_file from all inla objects (the correct way to extrcat info from objects)
hessian_log_list <- list()
i <- 0
s_loop <- Sys.time()
for(f in inla_object_files){
  
  s_in <- Sys.time()
  
  i <- i+1
  
  # Object number (quick way is readr::parse_number to get number from string)
  nth_data <- readr::parse_number(f)
  
  # Load the inla object (few seconds per object)  
  obj <- readRDS(f)
  
  # Get the log_file
  hessian_log <- paste0(obj$logfile[grep("Hessian",obj$logfile)],collapse=" ; ")
  
  # Add to list 
  hessian_log_list[nth_data] <- hessian_log
  
  # Keep trakc of time 
  e <- Sys.time() 
  td_loop <- round(difftime(e,s_loop,units = "mins"),2)
  td <- round(difftime(e,s_in,units = "secs"),2)
  
  if(td > 60){
    
    cat("\n")
    cat("Object:",i,"; nth data:",nth_data,"; Time:",td,"sec ; Total time:",td_loop,"min\n")
    cat("\n")
    
  }else{
    cat("Object:",i,"; nth data:",nth_data,"; Time:",td,"sec ; Total time:",td_loop,"min\n")
    
  }
} # 19' 


for( i in 1:length(hessian_log_list)){
  print(hessian_log_list[[i]])
}
hessian_log_list[[1]]

find_negative_hessian <- function(string){
  
  return(stringr::str_detect(string,"trouble"))
  
}

which(sapply(hessian_log_list,find_negative_hessian)==T)




# # Load all objects in list --> Fails after certain time (memory issues) 
# inla_list <- list()
# s_loop <- Sys.time()
# for(f in inla_object_files){
#   s_in <- Sys.time()
#   
#   # Object number 
#   i <- as.integer(stringr::str_remove(
#     stringr::str_remove(f,"inla_object_"),
#     ".rds"))
#   
#   # Read object
#   inla_list[[i]] <- readRDS(f)
#   
#   # Keep trakc of time 
#   e <- Sys.time() 
#   td_loop <- round(difftime(e,s_loop,units = "mins"),2)
#   td <- round(difftime(e,s_in,units = "mins"),2)
#   
#   if(td >= 1){
#     
#     cat("\n")
#     cat("Object:",i,"; Time:",td,"min ; Total time:",td_loop,"min\n")
#     cat("\n")
#     
#   }else{
#     cat("Object:",i,"; Time:",td,"min ; Total time:",td_loop,"min\n")
#     
#   }
#   
# }

# Load one object as example 
inla_obj_1 <- readRDS("inla_object_100.rds")
summary(inla_obj_1)
test <- inla_obj_1$summary.hyperpar
plot(inla_obj_1$marginals.hyperpar$`Precision for practice_id`)

# Summary of hyperpars for all objects 
hyperpar_sum_list <- list()
mode_status_list <- list()
fixed_sum_list <- list()
random_sum_ID_list <- list()
random_sum_practice_list <- list()

i <- 0
s_loop <- Sys.time()
for(f in inla_object_files){
  
  s_in <- Sys.time()
  
  i <- i+1
  
  # Object number (quick way is readr::parse_number to get number from string)
  nth_data <- readr::parse_number(f)
  
  # Load the inla object (few seconds per object)  
  obj <- readRDS(f)
  
  # Get the hyperparamter summary 
  hyperpar_sum <- obj$summary.hyperpar
  hyperpar_sum$nth_data <- nth_data
  hyperpar_sum$effect <- rownames(hyperpar_sum)
  
  hyperpar_sum_list[[nth_data]] <- hyperpar_sum
  
  # Get mode.status 
  mode_status_list[[nth_data]] <- obj$mode$mode.status
  
  # Get summary of fixed efects 
  fixed_sum <- obj$summary.fixed
  fixed_sum$nth_data <- nth_data
  fixed_sum$effect <- rownames(fixed_sum)
  
  fixed_sum_list[[nth_data]] <- fixed_sum
  
  # Summary of random effects
  random_sum_ID <- obj$summary.random$ID
  random_sum_practice <- obj$summary.random$practice_id
  
  random_sum_ID$nth_data <- nth_data
  random_sum_practice$nth_data <- nth_data
  
  random_sum_ID_list[[nth_data]] <- random_sum_ID
  random_sum_practice_list[[nth_data]] <- random_sum_practice
  
  # Keep trakc of time 
  e <- Sys.time() 
  td_loop <- round(difftime(e,s_loop,units = "mins"),2)
  td <- round(difftime(e,s_in,units = "secs"),2)
  

    cat("Object:",i,"; nth data:",nth_data,"; Time:",td,"sec ; Total time:",td_loop,"min\n")
    
} # 23 minutes only hyperpar; 20' for all

# Put in data frame 
hyperpar_sum_df <- do.call(rbind,c(hyperpar_sum_list,make.row.names=F))
fixed_sum_df <- do.call(rbind,c(fixed_sum_list,make.row.names=F))
random_sum_ID_df <- do.call(rbind,c(random_sum_ID_list,make.row.names=F))
random_sum_practice_df <- do.call(rbind,c(random_sum_practice_list,make.row.names=F))


# Mode.status paramater
which(mode_status_list > 0)
which(mode_status_list > 1)

# Precision / standard deviation spatial process
prec_spatial <- hyperpar_sum_df %>% 
  filter(effect == "Precision for ID")

# Load the track_matrix 
track_matrix <- read.csv("../track_matrix.csv") %>% 
  select(-X)

prec_spatial <- prec_spatial %>% 
  left_join(track_matrix, by="nth_data") %>% 
  mutate(sd = 1/sqrt(mean))

ggplot(prec_spatial, aes(x=as.factor(sd_practice),y= 1/sqrt(mean)))+
  geom_jitter(size=1.2,alpha=0.2,width = 0.2)+
  facet_wrap(~as.factor(n_practices))+
  theme_bw()+
  labs(x="SD practice",
       y="1/sqrt(mean_precision) = sd_spatial",
       title="N practices")

# Estimates spatial effect 
## Load simulated 
spatial_sim <- read.csv("../simulated_spatial_effects.csv") %>% 
  select(-X)
spatial_eff <- random_sum_ID_df %>% 
  left_join(spatial_sim, by = c("ID","nth_data")) %>% 
  mutate(ae = abs(simulated-mean)) %>% 
  group_by(sd_practice,n_practices) %>% 
  summarize(mae = mean(ae))

ggplot(spatial_eff, aes(x=as.factor(sd_practice),y=mae, shape = as.factor(n_practices)))+
  geom_point(size=2)+
  theme_bw()+
  labs(x = "SD practice",
       y = "Mean Absolute Error",
       title = "Spatial effect error")+
  scale_shape_discrete("N practices")


# Check the run.pbs.o file 
o_file <- read.delim("../run.pbs.o51085434",header=F)
o_file_time_passed <- o_file[grep("TIME passed",o_file$V1),]
o_file_long <- o_file_time_passed[order(readr::parse_number(o_file_time_passed),decreasing = T)]
sum(readr::parse_number(o_file_time_passed))


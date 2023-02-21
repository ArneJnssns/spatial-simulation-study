

# Introduction ------------------------------------------------------------

# This script generates a matrix of parameter values that we will use 
# to run the code on the HPC

rm(list=ls())
cwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cwd)

# # Total parameter matrix 
# sd_practice <- c(0.1,0.3,0.6)
# n_practices <- c(1,2,3)
# sd_spatial <- c(0.1,0.3,0.6)
# 
# parameter_matrix <- expand.grid(sd_practice = sd_practice,
#                                 n_practices = n_practices,
#                                 sd_spatial = sd_spatial)
# write.table(parameter_matrix, file = "parameters.txt", sep = ",")

# Current try-out selection of parameters 
sd_practice <- c(0.1,0.3,0.6)
# n_practices <- c(1,2,3)
sd_spatial <- c(0.1,0.3,0.6)

parameter_matrix <- expand.grid(sd_practice = sd_practice,
                                # n_practices = n_practices,
                                sd_spatial = sd_spatial)
write.table(parameter_matrix, 
            file = "../../github-repo-data/intego_structure/parameters.txt", 
            sep = ",", 
            row.names = FALSE)

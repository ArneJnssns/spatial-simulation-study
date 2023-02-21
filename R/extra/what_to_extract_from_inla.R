# Old code 

#
# What to extract from inla objects? 
# - in order to save the data of interest on the HPC instead of all inla objects 
# - inla object also contain the data on which the fit was done (lot of memory for large datasets)
#

#
# Reference: slides of inla workshop received from pavlos + https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html
#

rm(list=ls())

library(tidyverse)
library(INLA)

# Set wd to directory of script and then relative to this the output 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../hpc_wf/output")

# Investigate one inla object 
inla_obj <- readRDS("inla_object_1.rds")

# View possible objects in inla_obj
# View(inla_obj)


# Random effects 
names(inla_obj)[stringr::str_detect(names(inla_obj), "random")]
inla_obj$model.random
str(inla_obj$summary.random)
str(inla_obj$marginals.random)
inla_obj$size.random

## Summary 
save_summary_random <- function(inla_obj){
  for(i in names(inla_obj$summary.random)){
    # return(assign(paste0("summary_",i), inla_obj$summary.random[[i]]))
    write.csv(inla_obj$summary.random[[i]], file = paste0("summary_",i,".csv"))
  }
}
save_summary_random(inla_obj)

## Marginals 

### Working function to obtain summary measures from (posterior) marginals 
get_summary_measures_pmarg <- function(marginal){
  
  # summary measures with zmarginal 
  marg_sum <- as_tibble(inla.zmarginal(marginal, silent = T))
  
  # HPD interval
  marg_hpd <- inla.hpdmarginal(0.95,marginal)
  
  # Add to summary 
  marg_sum$hpd.lower <- marg_hpd[[1]]
  marg_sum$hpd.higher <- marg_hpd[[2]]
  marg_sum$exp <- inla.emarginal(function(x) x,marginal)
  marg_sum$mode <- inla.mmarginal(marginal)
  
  return(marg_sum)
  
}

### Working function to save marginals of random effects and their summary measures --> this is function that takes the longest
save_pmarginals_random <- function(inla_obj){
  
  for(i in names(inla_obj$marginals.random)){
    
    # Temp save marginals (+ summary) of random effect i 
    marginals <- inla_obj$marginals.random[[i]]
    marginals_sum <- lapply(marginals,get_summary_measures_pmarg)
    
    # Correct variable names 
    names(marginals) <- inla_obj$summary.random[[i]]$ID
    names(marginals_sum) <- inla_obj$summary.random[[i]]$ID
    
    # Create dataframe 
    # assign(paste0("pmarg_",i),data.table::rbindlist(lapply(marginals,
    #                                                        as.data.frame),
    #                                                 idcol="ID") %>%
    #          mutate(ID = gsub("index.","",ID)))
    df_marg <- data.table::rbindlist(lapply(marginals,as.data.frame),
                                     idcol="ID") %>%
      mutate(ID = gsub("index.","",ID))
    df_marg_sum <- data.table::rbindlist(lapply(marginals_sum,as.data.frame),
                                     idcol="ID") %>%
      mutate(ID = gsub("index.","",ID))
    
    # save
    write.csv(df_marg, file = paste0("pmarg_",i,".csv"))
    write.csv(df_marg_sum, file = paste0("pmarg_sum_",i,".csv"))
    
  }
}
save_pmarginals_random(inla_obj)


# Hyperparameters (precision (and standard deviation)) 
names(inla_obj)[stringr::str_detect(names(inla_obj), "hyper")]
inla_obj$summary.hyperpar
inla_obj$marginals.hyperpar$`Precision for ID`
inla_obj$internal.summary.hyperpar
inla_obj$internal.marginals.hyperpar$`Log precision for ID`
inla_obj$joint.hyper
inla_obj$nhyper
inla_obj$all.hyper$random

## Summary: working function 
save_summary_hyperpar <- function(inla_obj){
  
  df_summary_hyper <- inla_obj$summary.hyperpar %>% 
    rownames_to_column("var")
  
  write.csv(df_summary_hyper, file = paste0("summary_hyperpar",".csv"))
}
save_summary_hyperpar(inla_obj)

## Marginals 

### precision (working function)
save_pmarginals_hyperpar_precision <- function(inla_obj){
  
  # Posterior marginal of precisions
  pmarg_prec <- inla_obj$marginals.hyperpar
  
  # Summary measures 
  pmarg_prec_sum <- lapply(pmarg_prec, get_summary_measures_pmarg)
  
  # Create dataframe 
  df_marg <- data.table::rbindlist(lapply(pmarg_prec,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  df_sum <- data.table::rbindlist(lapply(pmarg_prec_sum,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  # Save
  write.csv(df_marg, file = paste0("pmarg_hyperpar_precision",".csv"))
  write.csv(df_sum, file = paste0("pmarg_hyperpar_precision_summary",".csv"))
  
}
save_pmarginals_hyperpar_precision(inla_obj)

### standard deviation (working function)
save_pmarginals_hyperpar_sd <- function(inla_obj){
  
  # Transform posterior marginals to standard deviation (set n = 400)
  pmarg_sd <- lapply(inla_obj$marginals.hyperpar, function(x) inla.tmarginal(function(tau) tau^(-1/2), x, n = 400))
  
  # Summary measures 
  pmarg_sd_sum <- lapply(pmarg_sd, get_summary_measures_pmarg)
  
  # Create data frame 
  df_marg <- data.table::rbindlist(lapply(pmarg_sd,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  df_sum <- data.table::rbindlist(lapply(pmarg_sd_sum,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  # Save 
  write.csv(df_marg, file = paste0("pmarg_hyperpar_sd",".csv"))
  write.csv(df_sum, file = paste0("pmarg_hyperpar_sd_summary",".csv"))
  
}
save_pmarginals_hyperpar_sd(inla_obj)


## Internal representation (log-scale)
### Summary: working function 
save_summary_hyper_internal <- function(inla_obj){
  
  df_summary_hyper_internal <- inla_obj$internal.summary.hyperpar %>% 
    rownames_to_column("var")
  
  write.csv(df_summary_hyper_internal, file = paste0("summary_hyperpar_internal",".csv"))
}
save_summary_hyper_internal(inla_obj)

### precision 
save_pmarginals_hyperpar_precision_internal <- function(inla_obj){
  
  # Posterior marginal of precisions
  pmarg_prec <- inla_obj$internal.marginals.hyperpar
  
  # Summary measures 
  pmarg_prec_sum <- lapply(pmarg_prec, get_summary_measures_pmarg)
  
  # Create dataframe 
  df_marg <- data.table::rbindlist(lapply(pmarg_prec,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  df_sum <- data.table::rbindlist(lapply(pmarg_prec_sum,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  # Save
  write.csv(df_marg, file = paste0("pmarg_hyperpar_precision_internal",".csv"))
  write.csv(df_sum, file = paste0("pmarg_hyperpar_precision_summary_internal",".csv"))
  
}
save_pmarginals_hyperpar_precision_internal(inla_obj)

### standard deviation 
save_pmarginals_hyperpar_sd_internal <- function(inla_obj){
  
  # Transform posterior marginals to standard deviation (set n = 400)
  pmarg_sd <- lapply(inla_obj$internal.marginals.hyperpar, function(x) inla.tmarginal(function(tau) tau^(-1/2), x, n = 400))
  
  # Summary measures 
  pmarg_sd_sum <- lapply(pmarg_sd, get_summary_measures_pmarg)
  
  # Create data frame 
  df_marg <- data.table::rbindlist(lapply(pmarg_sd,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  df_sum <- data.table::rbindlist(lapply(pmarg_sd_sum,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  # Save 
  write.csv(df_marg, file = paste0("pmarg_hyperpar_sd_internal",".csv"))
  write.csv(df_sum, file = paste0("pmarg_hyperpar_sd_summary_internal",".csv"))
  
}
save_pmarginals_hyperpar_sd_internal(inla_obj)


# Fixed effects 
names(inla_obj)[stringr::str_detect(names(inla_obj), "fixed")]
inla_obj$names.fixed
inla_obj$summary.fixed
inla_obj$marginals.fixed

## Summary 
save_summary_fixed <- function(inla_obj){
  
  df_summary_fixed <- inla_obj$summary.fixed %>% 
    rownames_to_column("var")
  
  write.csv(df_summary_fixed, file = paste0("summary_fixed",".csv"))
}
save_summary_fixed(inla_obj)

## Marginals 
save_pmarginals_fixed <- function(inla_obj){
  
  # Posterior marginal of precisions
  pmarg_fixed <- inla_obj$marginals.fixed
  
  # Summary measures 
  pmarg_fixed_sum <- lapply(pmarg_fixed, get_summary_measures_pmarg)
  
  # Create dataframe 
  df_marg <- data.table::rbindlist(lapply(pmarg_fixed,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  df_sum <- data.table::rbindlist(lapply(pmarg_fixed_sum,as.data.frame),idcol="var") %>%
    mutate(var = gsub("Precision for ","",var))
  
  # Save
  write.csv(df_marg, file = paste0("pmarg_fixed",".csv"))
  write.csv(df_sum, file = paste0("pmarg_fixed_summary",".csv"))
  
}
save_pmarginals_fixed(inla_obj)


# Fitted values 
#tested on first model fitted in 02_fit_spatial_model.R --> set return.marginals.predictor = T
inla_obj = model_besag_iid
## Summary 
save_summary_fitted <- function(inla_obj){
  
  df_summary_fitted <- inla_obj$summary.fitted.values %>% 
    rownames_to_column("var")
  
  write.csv(df_summary_fitted, file = paste0("summary_fitted",".csv"))
}
save_summary_fitted(inla_obj)

## Marginals --> fitted value are in order of teh corresponding simulated data ! --> takes a lot of time, unnecessary? not that imortant to get these marginals? 
save_pmarginals_fitted <- function(inla_obj){
  
  # Posterior marginal of precisions
  pmarg_fitted <- inla_obj$marginals.fitted.values
  
  # Summary measures 
  pmarg_fitted_sum <- lapply(pmarg_fitted, get_summary_measures_pmarg)
  
  # Create dataframe 
  df_marg <- data.table::rbindlist(lapply(pmarg_fitted,as.data.frame),idcol="var") 
    # mutate(var = gsub("Precision for ","",var))
  
  df_sum <- data.table::rbindlist(lapply(pmarg_fitted_sum,as.data.frame),idcol="var") 
    # mutate(var = gsub("Precision for ","",var))
  
  # Save
  write.csv(df_marg, file = paste0("pmarg_fitted",".csv"))
  write.csv(df_sum, file = paste0("pmarg_fitted_summary",".csv"))
  
}
save_pmarginals_fitted(inla_obj)


# mode.status 
save_mode_status <- function(inla_obj){
  df <- tibble(mode.status = inla_obj$mode$mode.status)
  write.csv(df, file = paste0("mode_status",".csv"))
}
save_mode_status(inla_obj)


# Log file 
# logfile = paste0(inla_obj$logfile,collapse = "")
# paste0(inla_obj$logfile[grepl("Hessian",inla_obj$logfile)],collapse = " ; ")
t  = tibble(logfile = paste0(inla_obj$logfile,collapse = " ; "))
save_logfile <- function(inla_obj){
  
  df <- tibble(logfile = paste0(inla_obj$logfile,collapse = " ; "))
  write.csv(df, file = paste0("logfile",".csv"))
  
}
# save_logfile(inla_obj)

# ok paramter 
t = tibble(ok = inla_obj$ok)
save_ok <- function(inla_obj){
  df <- tibble(ok = inla_obj$ok)
  write.csv(df,file = paste0("ok",".csv"))
  
}
# save_ok(inla_obj)



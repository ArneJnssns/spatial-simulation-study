# Function to extract and save results from inla object 
# - Used on HPC worker framework 

# Taken from what_to_extract_from_inla.R

# Overall function that saves everything we want to extract fro minla object 
save_results_inla <- function(inla_obj, nth_data){
  
  # cat("EXTRACT AND SAVE DATA FROM INLA OBJECT \n")
  
  # library(tibble)
  
  # Parameters to set location(s) 
  dir_o <- "./output/"
  
  
  # Summary of the random effects 
  save_summary_random <- function(){
    
    # first-level subfolder 
    dir_m <- "random/"
    
    # highest level folder
    dir_h <- "summary/"
    
    for(i in names(inla_obj$summary.random)){
      # return(assign(paste0("summary_",i), inla_obj$summary.random[[i]]))
      
      # Variable name 
      dir_v <- paste0(i,"/")
      
      write.csv(inla_obj$summary.random[[i]] %>% 
                  mutate(nth_data = nth_data), 
                file = paste0(dir_o,dir_m,dir_v,dir_h,nth_data,".csv"))
    }
  }
  save_summary_random <- purrr::possibly(save_summary_random,otherwise = NA)
  save_summary_random()
  
  
  # Get Summary measures from (posterior) marginals
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
  
  
  # Marginals and their summary of the random effects 
  save_pmarginals_random <- function(){
    
    # first-level subfolder 
    dir_m <- "random/"
    
    for(i in names(inla_obj$marginals.random)){
      
      # Variable name 
      dir_v <- paste0(i,"/")
      
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
        mutate(ID = gsub("index.","",ID)) %>% mutate(nth_data = nth_data)
      df_marg_sum <- data.table::rbindlist(lapply(marginals_sum,as.data.frame),
                                           idcol="ID") %>%
        mutate(ID = gsub("index.","",ID)) %>% mutate(nth_data = nth_data)
      
      # save
      write.csv(df_marg, file = paste0(dir_o,dir_m,dir_v,"pmarg/",
                                       nth_data,".csv"))
      write.csv(df_marg_sum, file = paste0(dir_o,dir_m,dir_v,"summary_pmarg/",
                                           nth_data,".csv"))
      
    }
  }
  save_pmarginals_random <- purrr::possibly(save_pmarginals_random,otherwise = NA)
  save_pmarginals_random()
  
  
  # Summary of the hyperparameters (precision of random effects)
  save_summary_hyperpar <- function(){
    
    dir_h <- paste0(dir_o,"hyperpar/precision/summary/")
    
    df_summary_hyper <- inla_obj$summary.hyperpar %>% 
      rownames_to_column("var") %>% mutate(nth_data = nth_data)
    
    write.csv(df_summary_hyper, file = paste0(dir_h,nth_data,".csv"))
  }
  save_summary_hyperpar <- purrr::possibly(save_summary_hyperpar,otherwise = NA)
  save_summary_hyperpar()

  
    # Marginals and their summary of hyperparameters (precision level)
  save_pmarginals_hyperpar_precision <- function(){
    
    dir_v <- paste0(dir_o,"hyperpar/precision/")
    
    # Posterior marginal of precisions
    pmarg_prec <- inla_obj$marginals.hyperpar
    
    # Summary measures 
    pmarg_prec_sum <- lapply(pmarg_prec, get_summary_measures_pmarg)
    
    # Create dataframe 
    df_marg <- data.table::rbindlist(lapply(pmarg_prec,as.data.frame),idcol="var") %>%
      mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
    
    df_sum <- data.table::rbindlist(lapply(pmarg_prec_sum,as.data.frame),idcol="var") %>%
      mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
    
    # Save
    write.csv(df_marg, file = paste0(dir_v,"pmarg/",nth_data,".csv"))
    write.csv(df_sum, file = paste0(dir_v,"summary_pmarg/",nth_data,".csv"))
    
  }
  save_pmarginals_hyperpar_precision <- purrr::possibly(save_pmarginals_hyperpar_precision,otherwise = NA)
  save_pmarginals_hyperpar_precision()
  
  
  # Marginals and their summary of hyperparameters (standard deviation level)
  save_pmarginals_hyperpar_sd <- function(){
    
    dir_v <- paste0(dir_o,"hyperpar/standard_deviation/")
    
    # Transform posterior marginals to standard deviation (set n = 400)
    pmarg_sd <- lapply(inla_obj$marginals.hyperpar, function(x) inla.tmarginal(function(tau) tau^(-1/2), x, n = 400))
    
    # Summary measures 
    pmarg_sd_sum <- lapply(pmarg_sd, get_summary_measures_pmarg)
    
    # Create data frame 
    df_marg <- data.table::rbindlist(lapply(pmarg_sd,as.data.frame),idcol="var") %>%
      mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
    
    df_sum <- data.table::rbindlist(lapply(pmarg_sd_sum,as.data.frame),idcol="var") %>%
      mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
    
    # Save 
    write.csv(df_marg, file = paste0(dir_v,"pmarg/",nth_data,".csv"))
    write.csv(df_sum, file = paste0(dir_v,"summary_pmarg/",nth_data,".csv"))
    
  }
  save_pmarginals_hyperpar_sd <- purrr::possibly(save_pmarginals_hyperpar_sd,otherwise = NA)
  save_pmarginals_hyperpar_sd()
  
  
  # Summary of internal representation of hyperparameters 
  # save_summary_hyper_internal <- function(){
  #   
  #   dir_h <- paste0(dir_o,"hyperpar/precision/internal/summary/")
  #   
  #   df_summary_hyper_internal <- inla_obj$internal.summary.hyperpar %>% 
  #     rownames_to_column("var") %>% mutate(nth_data = nth_data)
  #   
  #   write.csv(df_summary_hyper_internal, file = paste0(dir_h,nth_data,".csv"))
  # }
  # save_summary_hyper_internal <- purrr::possibly(save_summary_hyper_internal,otherwise = NA)
  # save_summary_hyper_internal()
  
  
  # Internal marginals and their summary of hyperparameters on precision level
  # save_pmarginals_hyperpar_precision_internal <- function(){
  #   
  #   dir_v <- paste0(dir_o,"hyperpar/precision/internal/")
  #   
  #   # Posterior marginal of precisions
  #   pmarg_prec <- inla_obj$internal.marginals.hyperpar
  #   
  #   # Summary measures 
  #   pmarg_prec_sum <- lapply(pmarg_prec, get_summary_measures_pmarg)
  #   
  #   # Create dataframe 
  #   df_marg <- data.table::rbindlist(lapply(pmarg_prec,as.data.frame),idcol="var") %>%
  #     mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
  #   
  #   df_sum <- data.table::rbindlist(lapply(pmarg_prec_sum,as.data.frame),idcol="var") %>%
  #     mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
  #   
  #   # Save
  #   write.csv(df_marg, file = paste0(dir_v,"pmarg/",nth_data,".csv"))
  #   write.csv(df_sum, file = paste0(dir_v,"summary_pmarg/",nth_data,".csv"))
  #   
  # }
  # save_pmarginals_hyperpar_precision_internal <- purrr::possibly(save_pmarginals_hyperpar_precision_internal,otherwise = NA)
  # save_pmarginals_hyperpar_precision_internal()
  
  
  # Internal marginals and their summaries of hyperparameters on standard deviation level 
  # save_pmarginals_hyperpar_sd_internal <- function(){
  #   
  #   dir_v <- paste0(dir_o,"hyperpar/standard_deviation/internal/")
  #   
  #   # Transform posterior marginals to standard deviation (set n = 400)
  #   pmarg_sd <- lapply(inla_obj$internal.marginals.hyperpar, function(x) inla.tmarginal(function(tau) tau^(-1/2), x, n = 400))
  #   
  #   # Summary measures 
  #   pmarg_sd_sum <- lapply(pmarg_sd, get_summary_measures_pmarg)
  #   
  #   # Create data frame 
  #   df_marg <- data.table::rbindlist(lapply(pmarg_sd,as.data.frame),idcol="var") %>%
  #     mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
  #   
  #   df_sum <- data.table::rbindlist(lapply(pmarg_sd_sum,as.data.frame),idcol="var") %>%
  #     mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
  #   
  #   # Save 
  #   write.csv(df_marg, file = paste0(dir_v,"pmarg/",nth_data,".csv"))
  #   write.csv(df_sum, file = paste0(dir_v,"summary_pmarg/",nth_data,".csv"))
  #   
  # }
  # save_pmarginals_hyperpar_sd_internal <- purrr::possibly(save_pmarginals_hyperpar_sd_internal,otherwise = NA)
  # save_pmarginals_hyperpar_sd_internal()
  
  
  # Summary of fixed effects 
  save_summary_fixed <- function(){
    
    dir_h <- paste0(dir_o,"fixed/summary/")
    
    df_summary_fixed <- inla_obj$summary.fixed %>% 
      rownames_to_column("var") %>% mutate(nth_data = nth_data)
    
    write.csv(df_summary_fixed, file = paste0(dir_h,nth_data,".csv"))
  }
  save_summary_fixed <- purrr::possibly(save_summary_fixed, otherwise = NA)
  save_summary_fixed()
  
  
  # Marginals and their summary of fixed effects 
  save_pmarginals_fixed <- function(){
    
    dir_v <- paste0(dir_o,"fixed/")
    
    # Posterior marginal of precisions
    pmarg_fixed <- inla_obj$marginals.fixed
    
    # Summary measures 
    pmarg_fixed_sum <- lapply(pmarg_fixed, get_summary_measures_pmarg)
    
    # Create dataframe 
    df_marg <- data.table::rbindlist(lapply(pmarg_fixed,as.data.frame),idcol="var") %>%
      mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
    
    df_sum <- data.table::rbindlist(lapply(pmarg_fixed_sum,as.data.frame),idcol="var") %>%
      mutate(var = gsub("Precision for ","",var)) %>% mutate(nth_data = nth_data)
    
    # Save
    write.csv(df_marg, file = paste0(dir_v,"pmarg/",nth_data,".csv"))
    write.csv(df_sum, file = paste0(dir_v,"summary_pmarg/",nth_data,".csv"))
  }
  save_pmarginals_fixed <- purrr::possibly(save_pmarginals_fixed, otherwise = NA)
  save_pmarginals_fixed()
  
  
  # Summary of fitted values 
  save_summary_fitted <- function(){
    
    dir_h <- paste0(dir_o,"fitted_values/summary/")
    
    df_summary_fitted <- inla_obj$summary.fitted.values %>% 
      rownames_to_column("var") %>% mutate(nth_data = nth_data)
    
    write.csv(df_summary_fitted, file = paste0(dir_h,nth_data,".csv"))
  }
  save_summary_fitted <- purrr::possibly(save_summary_fitted, otherwise = NA)
  save_summary_fitted()
  
  # # Marginals and their summary of fitted values (not used because of memory usage)
  # save_pmarginals_fitted <- function(inla_obj){
  #   
  #   # Posterior marginal of precisions
  #   pmarg_fitted <- inla_obj$marginals.fitted.values
  #   
  #   # Summary measures 
  #   pmarg_fitted_sum <- lapply(pmarg_fitted, get_summary_measures_pmarg)
  #   
  #   # Create dataframe 
  #   df_marg <- data.table::rbindlist(lapply(pmarg_fitted,as.data.frame),idcol="var") 
  #   # mutate(var = gsub("Precision for ","",var))
  #   
  #   df_sum <- data.table::rbindlist(lapply(pmarg_fitted_sum,as.data.frame),idcol="var") 
  #   # mutate(var = gsub("Precision for ","",var))
  #   
  #   # Save
  #   write.csv(df_marg, file = paste0("pmarg_fitted",".csv"))
  #   write.csv(df_sum, file = paste0("pmarg_fitted_summary",".csv"))
  #   
  # }
  
  
  # Mode status parameter indicating any convergence problems 
  save_mode_status <- function(){
    
    dir_h <- paste0(dir_o,"other/mode_status/")
    
    df <- tibble(mode.status = inla_obj$mode$mode.status) %>% mutate(nth_data = nth_data)
    write.csv(df, file = paste0(dir_h,nth_data,".csv"))
  }
  save_mode_status <- purrr::possibly(save_mode_status, otherwise = NA)
  save_mode_status()
  
  
  # Log file to easliy retrieve possible 'problem' models 
  save_logfile <- function(){
    
    dir_h <- paste0(dir_o,"other/logfile/")
    
    df <- tibble(logfile = paste0(inla_obj$logfile,collapse = " ; ")) %>% mutate(nth_data = nth_data)
    write.csv(df, file = paste0(dir_h,nth_data,".csv"))
    
  }
  save_logfile <- purrr::possibly(save_logfile, otherwise = NA)
  save_logfile()
  
  
  # ok parameter indiciating possible issues with the model
  save_ok <- function(){
    
    dir_h <- paste0(dir_o,"other/ok/")
    
    df <- tibble(ok = inla_obj$ok) %>% mutate(nth_data = nth_data)
    write.csv(df,file = paste0(dir_h,nth_data,".csv"))
    
  }
  save_ok <- purrr::possibly(save_ok, otherwise = NA)
  save_ok()
}
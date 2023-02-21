
# Test script om commandline R MET input parameters te gebruiken 
#https://swcarpentry.github.io/r-novice-inflammation/05-cmdline/index.html

# Print session info
sessionInfo()

# Use command line arguments
args <- commandArgs(trailingOnly = TRUE) # Only provided arguments 
cat(args,sep = "\n")

# Create function to run using command line arguments
main <- function(){

  args <- commandArgs(trailingOnly = TRUE)
  name1 <- args[1]
  name2 <- args[2]

  output <- paste0(name1," loves ",name2)

  cat(output)

}

main()

library(dplyr)
library(INLA)

# Function to read simulated data 
read_simulated_data <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  sd_practice_arg <- args[1]
  n_practices_arg <- args[2]
  
  cat("READ DATA \n")
  data <- read.csv(paste0("/Users/u0121893/Library/CloudStorage/OneDrive-KULeuven/",
                          "PhD/Research/Simulation/Spatial/simulation-study-git/",
                          "simulated_data.csv")) %>% 
    select(-X) %>% 
    filter(sd_practice == sd_practice_arg,
           n_practices == n_practices_arg) %>% 
    mutate(age_category=factor(age_category,
                               levels=c("(16,50]","(-1,5]","(5,16]","(50,65]","(65,85]","(85,110]")),
           ID = as.integer(ID))
  
  write.csv2(data,"./test_filter_data.csv")
  
  formula <- n_cases ~ sex + age_category + increased_compensation  +
    f(ID, model="besag",graph="map.gra" ,scale.model=T)+
    f(practice_id,model="iid")
  
  
  cat("START ITERATING OVER SIMULATIONS \n")
  for(i in 1:2){
    
    cat("FIT model on simulation ",i,"\n")
    
    data_to_fit <- data %>% filter(simulation == i)
    nth_data <- unique(data_to_fit$nth_data)
    
    write.csv2(data_to_fit, paste0("./data_to_fit_",nth_data,".csv"))
    
    s <- Sys.time()
    
    inla.out <- inla(formula,
                     family="binomial",
                     data = data_to_fit,
                     Ntrials = n_patients # N Bernouilli experiments
                     # control.predictor = list(compute = T, link=1), # Compute fitted values (proportions)
                     # control.compute=list(waic=T,dic=T,config=T)
    )
    
    cat("TIME passed: ", Sys.time() - s, "\n")
    
    saveRDS(inla.out,paste0("./inla_cli_",nth_data,".rds"))
    
    
  }
  
  
}

read_simulated_data()


# library(sqldf)
# data_sql <- read.csv.sql(paste0("/Users/u0121893/Library/CloudStorage/OneDrive-KULeuven/",
#                                 "PhD/Research/Simulation/Spatial/simulation-study-git/",
#                                 "simulated_data.csv"),
#                          sql = "select * from file")


# Created: 30/11/2022

# Introduction ------------------------------------------------------------

# This script reproduces the code from the "sensitivity_se.R" script in 
# ".../spatial/code/" (starting line 648)

# Thomas made a remark in the manuscript that there was an essential part missing 
# in the case study section. Namely, the part where we fitted the model on data 
# coming from a random subsample of practices and where sometime saw a spatial effect 
# (however with large variation) and sometimes not. 
# This was the motivation to start a simulation study. 




# Prepare environment -----------------------------------------------------

# Get the current dir 
dir <- dirname(rstudioapi::getActiveDocumentContext()[[2]])

# Set dir 
setwd(dir)

# Check 
getwd()



# Libraries ---------------------------------------------------------------

library(sf)
library(INLA)


# Get data  -------------------------------------------------------------------

# LRTI 
data <- read.csv("../data/clean/data_lrti_2019.csv") %>% 
  select(-X)

# Map of Flanders 
map_flanders <- st_read(dsn  = "../data/clean", layer = "map_fl")


# Get some functions ------------------------------------------------------

source("functions_geo.R")
source("functions_inla.R")


# Fit the model  ----------------------------------------------------------

formula <- ncases ~ sex + age_category + increased_compensation  + 
  f(ID, model="besag", graph="../data/clean/map.gra", scale.model=T) + 
  f(practice_id, model="iid")

model <- inla(formula,
              family = "binomial",
              data = data,
              Ntrials = npatients, # N Bernouilli experiments 
              control.predictor = list(link=T),
              control.compute=list(waic=T,dic=T,config=T)
)

summary(model)

# Estimates 
fe <- get_fixed_effects_inla(model)
re <- get_random_effects_inla(model)

# Figures
plot_fixed_effects(fe)
plot_random_effects(re,n_re = 2, plot_map = map_flanders)



# Random subset of practices  ---------------------------------------------

##  Define a function to automate the model fit on random subsets ----

fit_model_on_random_subset <- function(practice_sub,
                                       model_data,
                                       formula){
  
  # practice_sub = random sample of practices 
  # model_data = data after subseting for the sample of practices 
  # formulae = model to fit 
  
  data_model <- model_data %>% 
    filter(practice_id %in% practice_sub) 
  
  inla.out <- inla(formula,
                   family="binomial",
                   data = data_model ,
                   Ntrials = npatients, # N Bernouilli experiments 
                   control.predictor = list(link=1),
                   control.compute=list(waic=T,dic=T,config=T)
  )
  
  fe <- get_fixed_effects_inla(inla.out, OR=F)
  re <- get_random_effects_inla(inla.out, OR=F)
  
  re <- do.call(rbind,re) #%>% rename(Param=ID)
  
  estimates <- rbind(fe,re)
  
  #predictions <- get_predicted_incidence_inla(inla.out,data_model)
  
  
  return(list(estimates = estimates))#predictions=predictions))
  
  
}


## Initialize ---- 

# estimates
results_simul <- data.frame()
# predictions
# predictions_simul <- data.frame()
# Remove NA nis row
data_use <- data %>% filter(!is.na(practice_id))
# Sort all unique practices --> also do this in orevious analysis to be able to compare
practices <- sort(as.character(unique(data_use$practice_id)))
# 1 - Proportion of practices to sample
sample_prop <- 0
# 20 iterations
simul_subset_practice <- 20

# Execute
for(k in 1:simul_subset_practice){
  
  cat("Simulation ",k,"/",simul_subset_practice,"\n")
  set.seed(k)
  practice_sub<- practices[runif(length(practices))>sample_prop]
  
  
  out <- fit_model_on_random_subset(practice_sub,data_use,formula)
  
  results_simul <- rbind(results_simul,
                         out$estimates %>%
                           mutate(iteration=k))
  
  # predictions_simul <- rbind(predictions_simul,
  #                            out$predictions %>% 
  #                              mutate(iteration=k))
  
}

summary(results_simul)
names(results_simul)
rownames(results_simul)

## Spatial effect ----
# OR
#spatial <- results_simul[grep("ID",rownames(results_simul)),]
# Logit
spatial <- results_simul[results_simul$param%in%1:300,]

# Get the west-east ordering 
coordinates <- st_coordinates(st_centroid(map_flanders))
west_east <- tibble(ID = as.character(map_flanders$ID[order(test[,"X"])]),
                    index = 1:300)
spatial <- spatial %>% 
  left_join(west_east, by = c("param"="ID"))

# Save CSV 
write.csv(spatial,"~/Desktop/spatial_re_subset_90%_logit.csv")

# Plot posterior means
ggplot(data = spatial, aes(x=index, y = mean, color = as.factor(iteration)))+
  geom_point()

ggplot(data = spatial, aes(x=index, y = mean, group = index))+
  geom_boxplot()+
  theme_bw()

# compute Mena + 95%CI from posterior means 
spatial_ci <- spatial %>% 
  group_by(param,index) %>% 
  summarize(mean_of_mean = mean(mean),
            sd_of_mean = sd(mean)/sqrt(20),
            tscore = qt(p = 0.05/2, df=19, lower.tail = F),
            margin = tscore*sd_of_mean,
            lower = mean_of_mean-margin,
            upper = mean_of_mean+margin)

ggplot(data = spatial_ci, aes(x=index, y = mean_of_mean, group = index))+
  geom_point()+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  theme_bw()
  


## Practice effect ----
# OR
# practice <- results_simul[grep("practice",rownames(results_simul)),]
# Logit 
practice <- results_simul[grep("-",results_simul$param),]
write.csv(practice,"~/Desktop/practice_re_subset_90%_logit.csv")
ggplot(data = practice, aes(x=param, y = mean, group = param))+
  geom_boxplot() +
  theme(axis.text.x = element_blank())

## Fixed covariates ----
# OR
# fixed<- results_simul[!grepl("practice|ID",rownames(results_simul)),]
# Logit
fixed<- results_simul[!results_simul$param %in% 1:300 & !grepl("-",results_simul$param),]
write.csv(fixed,"~/Desktop/fe_subset_90%_logit.csv")
ggplot(data = fixed, aes(x=param, y = mean, group = param))+
  geom_boxplot() 


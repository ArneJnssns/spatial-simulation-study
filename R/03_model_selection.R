################################################################################
# MODEL SELECTION
# - fit some models and compare dic, waic, cpo 
# - Created/updated: 13-01-2022
################################################################################

# We keep it it simple by not using any interaction terms 
# - It was noticed that the models with interaction terms performed better but that is not our goal here? 
# - All simulations were done on a modle including only fixed effects 


rm(list=ls())

# Libraries
library(dplyr)
library(lubridate)
library(tibble)
library(tidyr)
library(stringr)

library(INLA)
library(rgdal)
library(spdep)

library(ggplot2)
library(RColorBrewer)
library(classInt)

# Avoid scientific notation
options(scipen=999)

# READ DATA 
#------------------------------------------------------------------------------

# Working directory and others 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

dir_data_raw <- "../data/downloaded_from_server/"
dir_data_clean <- "../data/clean/"
dir_output <- "../output_model_fit_case/"
# dir_output_large <- "../../github-repo-data/"

# Source useful scripts 
source("../R/functions_inla.R")
source("../R/functions_geo.R")

# Load the cleaned data 
data_inla <- read.csv(paste0(dir_data_clean,"data_lrti_2019.csv")) %>% select(-X)
map_fl <- st_read(dir_data_clean, layer  = "map_fl")

# Prepare for modelling 
data_inla_model <- data_inla %>% 
  mutate_at(c("sex","age_category","increased_compensation"), factor) %>% 
  mutate(age_category = relevel(age_category,"(16,50]"))


# MODEL SELECTON
#-------------------------------------------------------------------------------

s <- Sys.time()

# 1) Fixed effects 
formula1 <- ncases ~ sex + age_category + increased_compensation

inla1 <- inla(formula1,
                  family="binomial",
                  data = data_inla_model,
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  # verbose=TRUE,
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla1)
# all(inla1$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla1$cpo$cpo),na.rm=T))
sum1 <-get_model_fit_statistics_inla(inla1)


# 2) Fixed effects + practice_id as fixed effect 
formula2 <- ncases ~ sex + age_category + increased_compensation + practice_id

inla2 <- inla(formula2,
                  family="binomial",
                  data = data_inla_model,
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla2)
# all(inla2$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla2$cpo$cpo),na.rm=T))
sum2 <-get_model_fit_statistics_inla(inla2)


# 3) nis ID as random Gaussian
formula3 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model="iid") #constr = 0)

inla3 <- inla(formula3,
                  family="binomial",
                  data = data_inla_model,
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla3)
# all(inla3$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla3$cpo$cpo),na.rm=T))
sum3 <-get_model_fit_statistics_inla(inla3)


# 4) BYM
formula4 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model="bym", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)

inla4 <- inla(formula4,
                  family="binomial",
                  data = data_inla_model,
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla4)
# all(inla4$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla4$cpo$cpo),na.rm=T))
sum4 <-get_model_fit_statistics_inla(inla4)

# 5) not BYM but separetely:  BESAG (ICAR) + IID
formula5 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
  f(ID2,model="iid")

inla5 <- inla(formula5,
                  family="binomial",
                  data = data_inla_model %>% mutate(ID2=ID),
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=TRUE,config=T))

# summary(inla5)
# all(inla5$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla5$cpo$cpo),na.rm=T))
sum5 <-get_model_fit_statistics_inla(inla5)


# 6) NIS =  BESAG (ICAR) + Random Gaussian PRACTICE ID
formula6 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
  f(practice_id,model="iid")

inla6 <- inla(formula6,
                  family="binomial",
                  data = data_inla_model,
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla6)
# all(inla6$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla6$cpo$cpo),na.rm=T))
sum6 <-get_model_fit_statistics_inla(inla6)

# 7) NIS =  BESAG (ICAR) + random spatial +  Random Gaussian PRACTICE ID
formula7 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
  f(ID2,model="iid")+
  f(practice_id,model="iid")

inla7 <- inla(formula7,
                  family="binomial",
                  data = data_inla_model%>% mutate(ID2=ID),
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla7)
# all(inla7$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla7$cpo$cpo),na.rm=T))
sum7 <-get_model_fit_statistics_inla(inla7)


# 8) + interaction nis and practice

u <- unique(data_inla_model[,c("nis_code","practice_id")])
u$interaction <- rep((1:nrow(u)))
data_inla_model <- merge(data_inla_model,u,by=c("nis_code","practice_id"))

formula8 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
  f(ID2,model="iid")+
  f(interaction,model="iid")

inla8 <- inla(formula8,
                  family="binomial",
                  data = data_inla_model%>% mutate(ID2=ID),
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla8)
# all(inla8$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla8$cpo$cpo),na.rm=T))
sum8 <-get_model_fit_statistics_inla(inla8)


# 9) interaction without iid

formula9 <- ncases ~ sex + age_category + increased_compensation + 
  f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
  f(interaction,model="iid")

inla9 <- inla(formula9,
                  family="binomial",
                  data = data_inla_model%>% mutate(ID2=ID),
                  Ntrials = npatients, # N Bernouilli experiments 
                  # control.predictor = list(compute=T),
                  control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla9)
# all(inla9$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla9$cpo$cpo),na.rm=T))
sum9 <-get_model_fit_statistics_inla(inla9)


# 10) Only practice

formula10 <- ncases ~ sex + age_category + increased_compensation + 
  f(practice_id,model="iid")

inla10 <- inla(formula10,
                   family="binomial",
                   data = data_inla_model,
                   Ntrials = npatients, # N Bernouilli experiments 
                   # control.predictor = list(compute=T),
                   control.compute=list(waic=T,dic=T,cpo=T,config=T))

# summary(inla10)
# all(inla10$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla10$cpo$cpo),na.rm=T))
sum10 <-get_model_fit_statistics_inla(inla10)

# # 11) BESAG + random practice + interaction
# formula11 <- ncases ~ sex + age_category*increased_compensation +
#   f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
#   # f(ID2,model="iid")+
#   f(practice_id,model="iid")
# 
# inla11 <- inla(formula11,
#                   family="binomial",
#                   data = data_inla_model%>% mutate(ID2=ID),
#                   Ntrials = npatients, # N Bernouilli experiments 
#                   # control.predictor = list(compute=T),
#                   control.compute=list(waic=T,dic=T,cpo=T,config=T))
# 
# summary(inla11)
# all(inla11$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla11$cpo$cpo),na.rm=T))
# sum11 <-get_model_fit_statistics_inla(inla11)

# # 12) BESAG + random practice + interactions
# formula12 <- ncases ~ sex:age_category + sex*increased_compensation + age_category*increased_compensation +
#   f(ID, model="besag", graph=paste0(dir_data_clean,"map.gra"),scale.model=T)+
#   # f(ID2,model="iid")+
#   f(practice_id,model="iid")
# 
# inla12 <- inla(formula12,
#                    family="binomial",
#                    data = data_inla_model%>% mutate(ID2=ID),
#                    Ntrials = npatients, # N Bernouilli experiments 
#                    # control.predictor = list(compute=T),
#                    control.compute=list(waic=T,dic=T,cpo=T,config=T))
# 
# summary(inla12)
# all(inla12$cpo$failure == 0,na.rm = T)
# cat(-sum(log(inla12$cpo$cpo),na.rm=T))
# sum12 <-get_model_fit_statistics_inla(inla12)

elapsed_time <- Sys.time() - s
print(paste0("Elapsed time: ",elapsed_time))


# COLLECT DIC WAIC.. FOR ALL MODELS
#-------------------------------------------------------------------------------


model_fit <- eval(as.name("sum1"))
for(i in 2:10){
  
  var.name <- paste0("sum",i)
  print(var.name)
  model_fit <- rbind(model_fit,
    eval(as.name(var.name))
  )
}

model_fit <- model_fit %>% 
  mutate(sim = 1:nrow(model_fit)) %>% 
  select(sim,DIC,1:5)


precision_list <- list()
for(i in 1:10){
  
  inla_i <- paste0("inla",i)
  
  # precision_ID <- summary(eval(as.name(inla_i)))$hyperpar[["Precision for ID","mean"]]
  # precision_practice <- summary(eval(as.name(inla_i)))$hyperpar[["Precision for practice_id","mean"]]
  # precision_ID2 <- summary(eval(as.name(inla_i)))$hyperpar[["Precision for ID2","mean"]]
  # 
  # if(is.null(precision_ID)) model_fit[i,"precision_ID"] = -1 else model_fit[i,"precision_ID"] = precision_ID
  # if(is.null(precision_ID)) model_fit[i,"precision_practice"] = -1 else model_fit[i,"precision_practice"] = precision_practice
  # if(is.null(precision_ID)) model_fit[i,"precision_ID2"] = -1 else model_fit[i,"precision_ID2"] = precision_ID2

  randoms <- names(eval(as.name(inla_i))$summary.random)
  
  if(!is.null(randoms)){
    
    precision_list[[i]] <- summary(eval(as.name(inla_i)))$hyperpar %>% 
      rownames_to_column("precision") %>% 
      mutate(precision = str_remove(precision,"Precision for"),value = paste0(mean,",",sd)) %>% 
      select(precision,value) %>% 
      pivot_wider(names_from = c("precision"), values_from = "value") %>% 
      mutate(sim = i)
    
  }

}


precision <- bind_rows(precision_list)

model_fit_precision <- model_fit %>% 
  left_join(precision, by="sim") %>% 
  arrange(DIC) 

write.csv(model_fit_precision,paste0(dir_output,"03_model_selection_stats_and_random_effect_precision.csv"))


library(xtable)
?xtable
print(xtable(model_fit_precision,digits = 4),include.rownames = F)


# Save workspace 
# save.image(paste0(dir_output_large,"03_workspace.RData"))


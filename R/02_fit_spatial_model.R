################################################################################
# Fit models with a spatial component (those we tried initially + when we include practice_id)
# _ a kind of test script to learn working with inla and get to know the models
# - Created on 13-01-2022
# - Updated 24-01-2022
#
# Note that now Herstappe and Voeren aren't anymore (change in results wrt to initial powerpoint estimates probable)
# -> UPDATE 07-04-2022: with Herstappe and Voeren! 
#
# 30/01/2023
# - pas alles aan met niewe PC prior voor zowel spatiaal als praktijk effect 
# - PC prior met U = 1 en alpha = 0.01
################################################################################

rm(list=ls())

library(INLA)
library(tidyverse)
library(sf)

# Working directory and others 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

dir_data_raw <- "../data/downloaded_from_server/"
dir_data_clean <- "../data/clean/"
dir_output <- "../output_model_fit_case/"

# Source useful scripts 
source("../R/functions_inla.R")
source("../R/functions_geo.R")

# Load the cleaned data 
data_inla <- read.csv(paste0(dir_data_clean,"data_lrti_2019.csv")) %>% dplyr::select(-X)
map_fl <- st_read(dir_data_clean, layer  = "map_fl")


# Prepare for modelling 

## Data aggregated according to communities 
data_inla_model_aggr <- data_inla %>% 
  group_by(nis_code, ID, sex, age_category, increased_compensation) %>% 
  summarize(ncases = sum(ncases),
            npatients = sum(npatients)) %>% 
  mutate_at(c("sex","age_category","increased_compensation"), factor) %>% 
  mutate(age_category = relevel(age_category,"(16,50]")) %>% 
  mutate(ID2 = ID)

## Data inclduing practice_id variable
data_inla_model <- data_inla %>% 
  mutate_at(c("sex","age_category","increased_compensation"), factor) %>% 
  mutate(age_category = relevel(age_category,"(16,50]")) %>% 
  mutate(ID2 = ID)

# PC prior 
prec.prior = list(prec = list(prior = "pc.prec", param = c(1,0.01)))

# Model with BESAG + IID (~BYM)
formula_besag_iid <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "besag", 
    graph = paste0(dir_data_clean,"map.gra"), 
    scale.model = T,
    hyper = prec.prior) +
  f(ID2, 
    model="iid",
    hyper = prec.prior)

s <- Sys.time()
model_besag_iid <- inla(formula_besag_iid,
                        data = data_inla_model,
                        family="binomial",
                        Ntrials = npatients,
                        control.compute = list(dic = T, waic = T, cpo = T, config = T,return.marginals.predictor=T),
                        control.predictor = list(link = 1))
Sys.time() - s # 21''

summary(model_besag_iid)
model_besag_iid$mode$mode.status

s <- Sys.time()
model_besag_iid_aggr <- inla(formula_besag_iid,
                        data = data_inla_model_aggr,
                        family="binomial",
                        Ntrials = npatients,
                        control.compute = list(dic = T, waic = T, cpo = T, config = T),
                        control.predictor = list(link = 1))
Sys.time() - s # 11''

summary(model_besag_iid_aggr)
model_besag_iid_aggr$mode$mode.status

# Model BYM (Prior niet aangepast hier)
formula_bym <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "bym", graph = paste0(dir_data_clean,"map.gra"), scale.model = T) 

s <- Sys.time()
model_bym <- inla(formula_bym,
                        data = data_inla_model,
                        family="binomial",
                        Ntrials = npatients,
                        control.compute = list(dic = T, waic = T, cpo = T, config = T),
                        control.predictor = list(link = 1))
Sys.time() - s # 18''

summary(model_bym)
model_bym$mode$mode.status

s <- Sys.time()
model_bym_aggr <- inla(formula_bym,
                             data = data_inla_model_aggr,
                             family="binomial",
                             Ntrials = npatients,
                             control.compute = list(dic = T, waic = T, cpo = T, config = T),
                             control.predictor = list(link = 1))
Sys.time() - s # 11''

summary(model_bym_aggr)


# Model with BESAG + PRACTICE (our selected model which we use to simulate)
formula_besag_practice <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "besag", graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
    hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) +
  f(practice_id, model="iid",
    hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01))))

s <- Sys.time()
model_besag_practice <- inla(formula_besag_practice,
                  data = data_inla_model,
                  family="binomial",
                  Ntrials = npatients,
                  control.compute = list(dic = T, waic = T, cpo = T, config = T),
                  control.predictor = list(link = 1))
Sys.time() - s # 25''

summary(model_besag_practice)
model_besag_practice$mode$mode.status

# Model with BESAG + IID + PRACTICE 
formula_besag_iid_practice <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "besag", graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
    hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) +
  f(ID2, model = "iid",
    hyper =  list(prec = list(prior = "pc.prec", param = c(1,0.01)))) +
  f(practice_id, model="iid",
    hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01))))

s <- Sys.time()
model_besag_iid_practice <- inla(formula_besag_iid_practice,
                             data = data_inla_model,
                             family="binomial",
                             Ntrials = npatients,
                             control.compute = list(dic = T, waic = T, cpo = T, config = T),
                             control.predictor = list(link = 1))
Sys.time() - s # 22''

summary(model_besag_iid_practice)


# Model with bym + PRACTICE (Prior niet aangepast hier, hoe dit doen voor bym?)
formula_bym_practice <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "bym", graph = paste0(dir_data_clean,"map.gra"), scale.model = T) +
  f(practice_id, model="iid")

s <- Sys.time()
model_bym_practice <- inla(formula_bym_practice,
                                 data = data_inla_model,
                                 family="binomial",
                                 Ntrials = npatients,
                                 control.compute = list(dic = T, waic = T, cpo = T, config = T),
                                 control.predictor = list(link = 1))
Sys.time() - s # 18''

summary(model_bym_practice)


# Plot Random Effects (Fixed are all similar)

## BESAG + IID 
re_besag_iid <- get_random_effects_inla(model_besag_iid, OR = F)
pdf(file = paste0(dir_output,"02_random_effect_besag_iid.pdf"))
plot_random_effects(re_estimates = re_besag_iid, plot_map = map_fl)
dev.off()

## BYM (ID vector is length 2: 1:ncom = u+v & ncom+1 : 2*ncom = u (besag))
## - https://inla.r-inla-download.org/r-inla.org/doc/latent/bym.pdf
ncom <- length(unique(data_inla$nis_code))
re_bym_u_v <- model_bym$summary.random$ID[1:ncom,]
re_bym_u <- model_bym$summary.random$ID[(ncom+1):(2*ncom),] %>% 
  mutate(ID = 1:ncom)
re_bym_v <- re_bym_u_v
re_bym_v$mean <- re_bym_u_v$mean - re_bym_u$mean

plot_map <- map_fl %>% 
  left_join(re_bym_u %>% select(ID,mean), by  ="ID") %>% 
  left_join(re_bym_v %>% select(ID,mean), by = "ID", suffix = c(".u",".v")) %>% 
  left_join(re_bym_u_v %>% select(ID,mean), by = "ID")

u_plot <- ggplot(data = plot_map, aes(fill = mean.u))+
  theme_void()+
  geom_sf() + 
  scale_fill_continuous("Spatially Structured",type="viridis")

v_plot <- ggplot(data = plot_map, aes(fill = mean.v))+
  theme_void()+
  geom_sf() + 
  scale_fill_continuous("Spatially Unstructured",type="viridis")

u_v_plot <- ggplot(data = plot_map, aes(fill = mean))+
  theme_void()+
  geom_sf() + 
  scale_fill_continuous("Spatially Sum",type="viridis")

library(patchwork)
pdf(file = paste0(dir_output,"02_random_effect_bym.pdf"))
u_plot / v_plot / u_v_plot
dev.off()

## BESAG + IID + Practice 
re_besag_iid_practice <- get_random_effects_inla(model_besag_iid_practice, OR = F)
pdf(file = paste0(dir_output,"02_random_effect_besag_iid_practice.pdf"))
plot_random_effects(re_besag_iid_practice, plot_map = map_fl,n_re=3)
dev.off()

## Besag + practice 
re_besag_practice <- get_random_effects_inla(model_besag_practice, OR= T)
pdf(file = paste0(dir_output,"02_random_effect_besag_practice.pdf"))
plot_random_effects(re_besag_practice, plot_map = map_fl)
dev.off()


# Fitted values: how to comput p per community (integrate over fixe covariates? How?) 
data_to_plot <- model_bym
plot(data_inla_model$ncases/data_inla_model$npatients, data_to_plot$summary.fitted.values$mean)
plot(data_inla_model$ncases, data_to_plot$summary.fitted.values$mean*data_inla_model$npatients)
abline(a=0,b=1)




# Beetje op de stappen vooruit lopen maar hier slaan we point estimates op van het resulterende gekozen model,
# i.e. BESAG + PRACTICE (omdat IID niet veel bijbrengt ondanks lagere WAIC)
# Zie model_selection resultaten


# Plot posterior of intercept 
plot(model_besag_practice$marginals.fixed[[1]], type = "l", 
     xlab = expression(alpha), ylab = "density")

# Get effect, save and show
fe_inla <- get_fixed_effects_inla(model_besag_practice,OR=T)
write.csv(fe_inla, file = paste0(dir_output,"02_fixed_OR.csv"))
pdf(file = paste0(dir_output,"02_fixed_OR.pdf"))
plot_fixed_effects(fe_inla, OR = T)+
  theme_bw()
dev.off()

re_inla <- get_random_effects_inla(model_besag_practice,OR=T)
for(df in names(re_inla)){
  write.csv(re_inla[[df]],file = paste0(dir_output,"02_",df,"_OR.csv"))
}
# write.csv(fe_inla, file = paste0(dir_output,"/02_fixed_OR.csv"))
pdf(file = paste0(dir_output,"02_random_OR.pdf"))
plot_random_effects(re_inla,plot_map = map_fl)
dev.off()

# Logit level estimates
fe_inla <- get_fixed_effects_inla(model_besag_practice,OR=F)
write.csv(fe_inla, file = paste0(dir_output,"02_fixed.csv"))
pdf(file = paste0(dir_output,"02_fixed.pdf"))
source("./R/functions_inla.R")
plot_fixed_effects(fe_inla, OR = F)
dev.off()

re_inla <- get_random_effects_inla(model_besag_practice, OR = F)
for(df in names(re_inla)){
  write.csv(re_inla[[df]],file = paste0(dir_output,"02_",df,".csv"))
}
pdf(file = paste0(dir_output,"02_random.pdf"))
# pdf(file = "~/Desktop/random_logit.pdf")
plot_random_effects(re_inla,plot_map = map_fl)
dev.off()

# Precisions of random effects 
precisions <- summary(model_besag_practice)$hyperpar
write.csv(precisions, file = paste0(dir_output,"02_precisions.csv"))

# Model fit statistics 
model_fit_statistics <- get_model_fit_statistics_inla(model_besag_practice)
write.csv(model_fit_statistics, file = paste0(dir_output,"02_model_fit_statistics.csv"))

# Fitted values 
fitted_values <- model_besag_practice$summary.fitted.values
write.csv(fitted_values, file = paste0(dir_output,"02_fitted_values.csv"))

df_fitted <- data_inla %>% 
  mutate(proportion = ncases/npatients,
         fitted_proportion = model_besag_practice$summary.fitted.values$mean,
         fitted_ncases = fitted_proportion*npatients)

ggplot(df_fitted, aes(x = proportion, y = fitted_proportion))+
  geom_point()+
  geom_abline()+
  theme_bw()+
  labs(x = "Proportion/Incidence", y = "Fitted proportion")
ggsave(filename = paste0(dir_output,"02_fitted_proportions.pdf"))

ggplot(df_fitted, aes(x = ncases, y = fitted_ncases))+
  geom_point()+
  geom_abline()+
  theme_bw()+
  labs(x = "Number of cases", y = "Fitted nnumber of cases")
ggsave(filename = paste0(dir_output,"02_fitted_ncases.pdf"))






#Run onderstaande een andere keer, duurt te lang nu 
# library(microbenchmark)
# mbm <- microbenchmark(
#   
#   all = inla(formula_inla,
#        data = data_inla,
#        family="binomial",
#        Ntrials = npatients,
#        control.compute = list(dic = T, waic = T, cpo = T, config = T),
#        control.predictor = list(compute=TRUE, link = 1)),
#   
#   wo_fitted_values = inla(formula_inla,
#                           data = data_inla,
#                           family="binomial",
#                           Ntrials = npatients,
#                           control.compute = list(dic = T, waic = T, cpo = T, config = T)),
#   
#   wo_config = inla(formula_inla,
#                    data = data_inla,
#                    family="binomial",
#                    Ntrials = npatients,
#                    control.compute = list(dic = T, waic = T, cpo = T),
#                    control.predictor = list(compute=TRUE, link = 1)),
#   
#   wo_config_fitted_values = inla(formula_inla,
#                                  data = data_inla,
#                                  family="binomial",
#                                  Ntrials = npatients,
#                                  control.compute = list(dic = T, waic = T)),
#   times = 10
# )
# print(paste0("\nElapsed time: ", Sys.time - s))









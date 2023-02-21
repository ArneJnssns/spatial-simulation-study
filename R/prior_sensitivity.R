
# 03/01/2023
# 13/01/2023
# - Meeting met Thomas en Pieter 
# - Prior sensitivity: onderzoeken of een vagere prior hetzelfde resultaat geeft 
# - Pas sensitivity analyse aan: maak nieuwe sectie met juiste manier van analyse
# 30/01/2023
# - Na overleg op 27/01: gebrukk PC.prior en herdoe alle simulateis en analyses 
# - Hier probeer ik allerlei priors en probeerde ik ook de PC prior beter te begrijpen wat 
#   nu wel het geval is geloof ik 
# - We gebruiken nu PC.prec met U = 1 en alpha = 0.01 

# Introduction ------------------------------------------------------------

# PRIOR SENSITIVITY ANALYSIS OF VARIANCE PARAMETERS 
# - spatial process precision (variance)
# - practice process precision (variance)

# Intital lines of code taken from 02_fit_spatial_model.R where we fit the 
# original models including the besag + practice_id model

rm(list=ls())


# Libraries ---------------------------------------------------------------

library(INLA)
library(tidyverse)
library(sf)


# Working directory -------------------------------------------------------

# Working directory and others 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()

dir_data_raw <- "../data/downloaded_from_server/"
dir_data_clean <- "../data/clean/"
dir_output <- "../output_model_fit_case/"



# Scripts -----------------------------------------------------------------

# Source useful scripts 
source("../R/functions_inla.R")
source("../R/functions_geo.R")


# Get data ----------------------------------------------------------------


# Load the cleaned data 
data_inla <- read.csv(paste0(dir_data_clean,"data_lrti_2019.csv")) %>% dplyr::select(-X)
map_fl <- st_read(dir_data_clean, layer  = "map_fl")


# Prepare data for model --------------------------------------------------

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



# Original model (besag + practice iid) --------------------------------------------

# With new prior 
prec.prior <- list(prec = list(prior = "pc.prec", param = c(1,0.01)))

# Model with BESAG + PRACTICE (our selected model which we use to simulate)
formula_besag_practice <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "besag", graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
    hyper = prec.prior) +
  f(practice_id, model="iid", hyper = prec.prior)

s <- Sys.time()
model_besag_practice <- inla(formula_besag_practice,
                             data = data_inla_model,
                             family="binomial",
                             Ntrials = npatients,
                             control.compute = list(dic = T, waic = T, cpo = T, config = T),
                             control.predictor = list(link = 1))
Sys.time() - s # 55''

summary(model_besag_practice)
model_besag_practice$mode$mode.status

## Print table of log-odds or odds for overleaf/latex ----
library(xtable)
xtable(model_besag_practice$summary.fixed %>% select(mean,sd) %>% mutate_all(round,3))

get_odds_fixed <- function(marginal){
  m.var <- inla.tmarginal(function(x) exp(x),marginal)
  m_df <- data.frame(inla.zmarginal(m.var)) %>% 
    select(mean,sd,lower = 'quant0.025', upper = 'quant0.975')
  return(m_df)
}

odds_fixed <- lapply(model_besag_practice$marginals.fixed,get_odds_fixed)

odds_fixed <- do.call(rbind,odds_fixed)


get_variance_hyper_inla <- function(model, n_re = 1){
  m <- model$internal.marginals.hyperpar[[n_re]]
  m.var <- inla.tmarginal(function(x) sqrt(1/exp(x)),m)
  m_df <- data.frame(inla.zmarginal(m.var))
  return(m_df)
}

sd_car <- get_variance_hyper_inla(model_besag_practice, n_re=1) %>% 
  select(mean,sd,lower = 'quant0.025', upper = 'quant0.975')
rownames(sd_car) = "sigma_spatial"
sd_practice<- get_variance_hyper_inla(model_besag_practice, n_re=2) %>% 
  select(mean,sd,lower = 'quant0.025', upper = 'quant0.975')
rownames(sd_practice) = "sigma_practice"

original_estimates <- bind_rows(odds_fixed,
                                sd_car,
                                sd_practice) %>% mutate_all(round,3)

rownames(original_estimates) <- c("$\\hat{\\beta}_0$",
                                  "$\\hat{\\beta}_1$",
                                  "$\\hat{\\beta}_2$",
                                  "$\\hat{\\beta}_3$",
                                  "$\\hat{\\beta}_4$",
                                  "$\\hat{\\beta}_5$",
                                  "$\\hat{\\beta}_6$",
                                  "$\\hat{\\beta}_7$",
                                  "$\\hat{\\sigma}_u$",
                                  "$\\hat{\\sigma}_{practice}$"
                                  )

xtable(original_estimates, caption = "Posterior estimates", label = "original_estimates")

print(xtable(original_estimates,caption = "Posterior estimates", label = "original_estimates"), 
      only.contents=TRUE, include.rownames=T,
      include.colnames=T, floating=F, sanitize.rownames.function = identity)

# Plot Random Effects (Fixed are all similar)
re_besag_practice <- get_random_effects_inla(model_besag_practice, OR= T)
plot_random_effects(re_besag_practice, plot_map = map_fl)


# Plot posterior of intercept 
plot(model_besag_practice$marginals.fixed[[1]], type = "l", 
     xlab = expression(alpha), ylab = "density")

# Get effect, save and show
fe_inla <- get_fixed_effects_inla(model_besag_practice,OR=T)
plot_fixed_effects(fe_inla, OR = T)+
  theme_bw()

re_inla <- get_random_effects_inla(model_besag_practice)
plot_random_effects(re_inla,plot_map = map_fl)

# Logit level estimates
fe_inla <- get_fixed_effects_inla(model_besag_practice,OR=F)
plot_fixed_effects(fe_inla, OR = F)

re_inla <- get_random_effects_inla(model_besag_practice, OR = F)
plot_random_effects(re_inla,plot_map = map_fl)

# Precisions of random effects 
precisions <- summary(model_besag_practice)$hyperpar

# Model fit statistics 
model_fit_statistics <- get_model_fit_statistics_inla(model_besag_practice)

# Fitted values 
fitted_values <- model_besag_practice$summary.fitted.values

df_fitted <- data_inla %>% 
  mutate(proportion = ncases/npatients,
         fitted_proportion = model_besag_practice$summary.fitted.values$mean,
         fitted_ncases = fitted_proportion*npatients)

ggplot(df_fitted, aes(x = proportion, y = fitted_proportion))+
  geom_point()+
  geom_abline()+
  theme_bw()+
  labs(x = "Proportion/Incidence", y = "Fitted proportion")

ggplot(df_fitted, aes(x = ncases, y = fitted_ncases))+
  geom_point()+
  geom_abline()+
  theme_bw()+
  labs(x = "Number of cases", y = "Fitted nnumber of cases")


# Investigate INLA (hyper)priors ------------------------------------------------------

# Possible priors 
names(inla.models()$prior)

# Documentation for a specific prior, variance of Gamma(a,b) = a/b2
inla.doc("loggamma")

# Default prior fixed effects
inla.set.control.fixed.default()

inla.doc('iid')
inla.doc("besag")

names(inla.models()$latent)
names(inla.models()$latent$besag$hyper)
inla.models()$latent$besag$hyper$theta$name
inla.models()$latent$besag$hyper$theta$short.name
# Default hyperprior: loggamma, param = c(1, 0.00005), initial = 4
inla.models()$latent$besag$hyper$theta
inla.models()$latent$iid$hyper$theta

# Define new prior on hyper param
prec.prior <- list(prec = list(prior = "loggamma", param = c(0.01,0.01)))

# Example 1 
formula_ex1 <- ncases ~ sex + age_category + increased_compensation +
  f(ID, model = "besag", graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
    hyper = prec.prior) +
  f(practice_id, model="iid")

s <- Sys.time()
model_ex1 <- inla(formula_ex1,
                  data = data_inla_model,
                  family="binomial",
                  Ntrials = npatients,
                  control.compute = list(dic = T, waic = T, cpo = T, config = T),
                  control.predictor = list(link = 1))
Sys.time() - s # 55''

summary(model_ex1)
summary(model_besag_practice)
model_ex1$mode$mode.status
re_ex1 <- get_random_effects_inla(model_ex1, OR= F)
plot_random_effects(re_ex1, plot_map = map_fl)


# Sensitivity analysis: CORRECT -----------------------------------------------------

# Based on what was discussed on 13/01 during meeting with Thomas and Pieter 

# The prior we used is the default in INLA
# - Gamma(a,b), variance = a/b2 

# Sensitivity analysis by setting prior that are more vague. Suggestions: 
# - Jeffrey prior, variance = ?
# - Penalized Complexity prior, variance = ? 

names(inla.models()$prior)
inla.doc("loggamma") # Variance = a/b^2
inla.doc("jeffreystdf") # No documentation? how to implement? 
inla.doc("pc.prec") # Variance = ? param = c(u,alpha)
inla.doc("flat")
inla.doc("logflat")
inla.doc("normal")
inla.doc("pc.prec")
inla.doc("pc")

## GAMMA prior ----

# mean = a/b
# variance = a/b2

# Default pior
1/0.00005
prec.gamma <- rgamma(n=1000, shape = 1, rate = 0.00005)
hist(prec.gamma, prob = T)
lines(density(prec.gamma))

# More variance
prec.gamma <- rgamma(n=1000, shape = 1, rate = 0.000001)
hist(prec.gamma, prob = T)
lines(density(prec.gamma))

## PC.PREC prior ----

# Don't kow how this works exactly? Do not completely understand, it penalizes complex 
# Gaussian random fields. 
# I do not succes in plotting it
# Can we compute variance? Seems not? 

?inla.pc.dprec()

prec.pc = inla.pc.rprec(n=1000, u = 1, alpha=0.1)
hist(prec.pc,prob=T)
lines(density(prec.pc))

# This is how to plot the prior 
alpha = 0.01
u = 10
tau_u <- 1/u^2
lambda = -log(alpha)/u
prec.pc.prior <- function(tau){
  (lambda / 2)*(tau^(-3/2))*exp(-lambda*(tau^(-1/2)))
}

tau <- seq(0.1,50,0.1)
tau.pc <- prec.pc.prior(tau)
plot(tau,tau.pc)


## Normal ----
sd.nor <- rnorm(n=1000, mean = 1, sd = 1)
sd.nor <- sd.nor[sd.nor>=0]
prec.nor <- 1/sd.nor^2
hist(prec.nor,prob=T)
lines(density(prec.nor))# Does not make sense since negative valeus cannot be? 


UN.prior = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
"

# 25 (brecario) or 5 (gelman github)
HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 5; 
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"

# List of hyper priors CAR 
prior.list.car = list(
  # Default inla: mean = 1/0.00005, var = 1/0.00005^2
  # default = list(prec = list(prior = "loggamma", param = c(1, 0.00005))),
  
  # My own: with a = 1, different mean
  # loggamma1 = list(prec = list(prior = "loggamma", param = c(1, 0.00001))),
  
  # Mean = same as default but more variance
  # loggamma2 = list(prec = list(prior = "loggamma", param = c(0.2, 0.00001))),
  
  # PC prior 1
  # pc1 = list(prec = list(prior = "pc.prec", param = c(1,0.5))),
  
  # PC prior 2: http://www.leg.ufpr.br/~elias/cursos/montpellier2019/INLA_pc-montpellier.pdf
  # pc = list(prec = list(prior = "pc.prec", param = c(0.2/0.31,0.01))),
  # pc2 = list(prec = list(prior = "pc.prec", param = c(0.1,0.01))),
  pc = list(prec = list(prior = "pc.prec", param = c(1,0.01))),
  # pc3 = list(prec = list(prior = "pc.prec", param = c(10,0.01))),
  # pc4 = list(prec = list(prior = "pc.prec", param = c(100,0.01))),
  # pc5 = list(prec = list(prior = "pc.prec", param = c(1000,0.01))),
  
  # Gamma with mean 1 and variance = 1000
  gamma = list(prec = list(prior = "loggamma", param = c(0.001, 0.001))),
  
  # Uniform prior, valid? 
  # uniform = list(prec = list(prior = UN.prior)),
  
  # Half Cauchy
  half_cauchy = list(prec = list(prior = HC.prior))
  
  # PC with more flexibility 
  # pc2 =list(prec = list(prior = "pc.prec", param = c(10,0.01)))
  

  
  # Truncated normal (>0)
  # truncated_normal = list(prec = list(prior = "logtnormal", param = c(0,10^-6)))
  
  # logflat
  # flat = list(prec = list(prior = "logflat"))
  
)

car.models <- lapply(prior.list.car, function(tau.prior) {
  inla(ncases ~ sex + age_category + increased_compensation +
         f(ID, model = "besag",graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
           hyper = tau.prior) + 
         f(practice_id, model="iid",
           hyper = tau.prior),
       data = data_inla_model,
       family="binomial",
       Ntrials = npatients,
       control.compute = list(dic = T, waic = T, cpo = T, config = T),
       control.predictor = list(link = 1))
})

# Check summaries 
summary(car.models[[1]])
summary(car.models[[2]])
summary(car.models[[3]])
summary(car.models[[4]])
# summary(car.models[[5]])
# summary(car.models[[6]])


# re1<- get_random_effects_inla(car.models[[1]], OR= F)
# plot_random_effects(re1, plot_map = map_fl)
# re2<- get_random_effects_inla(car.models[[2]], OR= F)
# plot_random_effects(re2, plot_map = map_fl)
# re3<- get_random_effects_inla(car.models[[3]], OR= F)
# plot_random_effects(re3, plot_map = map_fl)
# re4<- get_random_effects_inla(car.models[[4]], OR= F)
# plot_random_effects(re4, plot_map = map_fl)
# re5<- get_random_effects_inla(car.models[[5]], OR= F)
# plot_random_effects(re5, plot_map = map_fl)

# Get the variance estimates 

get_variance_hyper_inla <- function(model, n_re = 1){
  m <- model$internal.marginals.hyperpar[[n_re]]
  m.var <- inla.tmarginal(function(x) sqrt(1/exp(x)),m)
  m_df <- data.frame(inla.zmarginal(m.var))
  return(m_df)
}

ID.var.car <- lapply(car.models,get_variance_hyper_inla, n_re =1)
ID.var.car <- do.call(rbind,ID.var.car)
ID.var.car <- round(ID.var.car,3) %>% 
  mutate(variance_ID = paste0(round(mean,2)," (",round(sd,2),")")) %>% 
  rownames_to_column("hyperprior") %>% 
  select(hyperprior,variance_ID)

pr.var.car <- lapply(car.models,get_variance_hyper_inla, n_re =2)
pr.var.car <- do.call(rbind,pr.var.car)
pr.var.car <- round(pr.var.car,3)%>% 
  mutate(variance_practice = paste0(round(mean,2)," (",round(sd,2),")")) %>% 
  rownames_to_column("hyperprior") %>% 
  select(hyperprior,variance_practice)

# Get mean and sd of fixed covariates
get_mean_sd <- function(model){
  return(model$summary.fixed %>% rownames_to_column("covariate"))
}

# test <- lapply(car.models, function(x) lapply(x$marginals.fixed,get_odds_fixed))
# test2 <- do.call(Map,c(rbind,test))
# test3 <- do.call(rbind,test2)
# test2 <- do.call(rbind,test)
# 
# test3 <- transpose(test) %>% map(bind_rows)

odds_fixed_1 <- lapply(car.models$pc$marginals.fixed,get_odds_fixed)
odds_fixed_1 <- do.call(rbind,odds_fixed_1) %>% 
  as.data.frame() %>% 
  mutate(hyperprior= "pc") %>% 
  rownames_to_column("coef")
odds_fixed_2 <- lapply(car.models$gamma$marginals.fixed,get_odds_fixed)
odds_fixed_2 <- do.call(rbind,odds_fixed_2) %>% 
  mutate(hyperprior = "gamma") %>% 
  rownames_to_column("coef")
odds_fixed_3 <- lapply(car.models$half_cauchy$marginals.fixed,get_odds_fixed)
odds_fixed_3 <- do.call(rbind,odds_fixed_3) %>% 
  mutate(hyperprior = "half_cauchy")%>% 
  rownames_to_column("coef")

# cov_fix.car <- lapply(car.models, FUN = get_mean_sd)
cov_fix.car <- bind_rows(odds_fixed_1,odds_fixed_2,odds_fixed_3) %>% 
  mutate(mean = round(mean,2),
         sd = round(sd,2),
         mean = paste0(mean," (",sd,")")) %>%   
  select(coef,mean,hyperprior) %>% 
  pivot_wider(names_from = coef, values_from = mean)

var.car <- cov_fix.car %>% 
  left_join(ID.var.car) %>% 
  left_join(pr.var.car)

test <- var.car %>% pivot_longer(cols = 2:11) %>% 
  pivot_wider(names_from = "hyperprior", values_from = "value") %>% 
  select(-name) %>% 
  as.data.frame()


# Create xtable overleaf 
rownames(test) <- c("$\\hat{\\beta}_0$",
                                  "$\\hat{\\beta}_1$",
                                  "$\\hat{\\beta}_2$",
                                  "$\\hat{\\beta}_3$",
                                  "$\\hat{\\beta}_4$",
                                  "$\\hat{\\beta}_5$",
                                  "$\\hat{\\beta}_6$",
                                  "$\\hat{\\beta}_7$",
                                  "$\\hat{\\sigma}_u$",
                                  "$\\hat{\\sigma}_{practice}$"
)

xtable(test, caption = "Posterior estimates", label = "tb:priorsens:estimates")

print(xtable(test,caption = "Posterior estimates", label = "original_estimates"), 
      only.contents=TRUE, include.rownames=T,
      include.colnames=T, floating=F, sanitize.rownames.function = identity)



# Now adapt iid hyper prior

# List of hyper priors IID
prior.list.iid <- prior.list.car

iid.models <- lapply(prior.list.iid, function(tau.prior) {
  inla(ncases ~ sex + age_category + increased_compensation +
         f(ID, model = "besag",graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
           hyper = list(prec = list(prior = "pc.prec", param = c(1,0.01)))) + 
         f(practice_id, model="iid", hyper = tau.prior),
       data = data_inla_model,
       family="binomial",
       Ntrials = npatients,
       control.compute = list(dic = T, waic = T, cpo = T, config = T),
       control.predictor = list(link = 1))
})

summary(iid.models[[1]])
summary(iid.models[[2]])
summary(iid.models[[3]])
summary(iid.models[[4]])
summary(iid.models[[5]])

# Get Variance of hyper parameters latent effect
ID.var.iid <- lapply(iid.models,get_variance_hyper_inla, n_re = 1)
ID.var.iid <- do.call(rbind,ID.var.iid)
ID.var.iid <- round(ID.var.iid,3) %>% 
  mutate(variance_ID = paste0(mean," (",sd,")")) %>% 
  rownames_to_column("hyperprior") %>% 
  select(hyperprior,variance_ID)

pr.var.iid <- lapply(iid.models,get_variance_hyper_inla, n_re = 2)
pr.var.iid <- do.call(rbind,pr.var.iid)
pr.var.iid <- round(pr.var.iid,3) %>% 
  mutate(variance_practice = paste0(mean," (",sd,")")) %>% 
  rownames_to_column("hyperprior") %>% 
  select(hyperprior,variance_practice)

# Get mean and sd of fixed covariates
get_mean_sd <- function(model){
  return(model$summary.fixed %>% rownames_to_column("covariate"))
}

cov_fix.iid <- lapply(iid.models, FUN = get_mean_sd)
cov_fix.iid <- do.call(rbind,cov_fix.iid) %>% 
  rownames_to_column("hyperprior") %>% 
  mutate(hyperprior = gsub("\\.[0-9]$","",hyperprior),
         mean = round(mean,3),
         sd = round(sd,3),
         value = paste0(mean," (",sd,")")) %>% 
  select(hyperprior, covariate,value) %>% 
  pivot_wider(names_from = covariate, values_from = value)

var.iid <- cov_fix.iid %>% 
  left_join(ID.var.iid) %>% 
  left_join(pr.var.iid)





# Sensitivity analysis: OLD -----------------------------------------------------

# Based on code from 
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-priors.html#sec:sensitivity (Gomez-Rubio)
# Paper Oana 2022
# Paper Maren 2022
# Paper Thomas 2012

# Uniform prior (Gomez-Rubio)
UN.prior = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
"

# Half Normal (Gomez-Rubio)
HN.prior = "expression:
  tau0 = 0.001;
  sigma = exp(-theta/2);
  log_dens = log(2) - 0.5 * log(2 * pi) + 0.5 * log(tau0);
  log_dens = log_dens - 0.5 * tau0 * sigma^2;
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);  
"

# Half Cauchy (Gomez-Rubio)
HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 25;
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"

# List of hyper priors CAR 
prior.list.car = list(
  # Default inla
  default = list(prec = list(prior = "loggamma", param = c(1, 0.00005))),
  
  # My own: with a = 1
  loggamma1 = list(prec = list(prior = "loggamma", param = c(1, 0.001))),
  loggamma2 = list(prec = list(prior = "loggamma", param = c(1, 0.01))),
  loggamma3 = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
  
  # Oana Petrof 2022
  loggamma4 = list(prec = list(prior = "loggamma", param = c(0.5,0.05))),
  loggamma5 = list(prec = list(prior = "loggamma", param = c(0.1,0.1))),
  
  # Gomez-Rubio
  # halfnormal = list(prec = list(prior = HN.prior)),
  # halfcauchy = list(prec = list(prior = HC.prior)),
  # uniform = list(prec = list(prior = UN.prior)),
  
  # Maren, 2022
  loggamma6 = list(prec = list(prior = "loggamma", param = c(0.01,0.01))),
  loggamma7 = list(prec = list(prior = "loggamma", param = c(0.001,0.001))),
  
  # A/B = mean ≈ 1 due to scale.model? 
  loggamma8 = list(prec = list(prior = "loggamma", param = c(0.5,0.5)))
  
)

car.models <- lapply(prior.list.car, function(tau.prior) {
  inla(ncases ~ sex + age_category + increased_compensation +
         f(ID, model = "besag",graph = paste0(dir_data_clean,"map.gra"), scale.model = T,
           hyper = tau.prior) + 
         f(practice_id, model="iid"),
       data = data_inla_model,
       family="binomial",
       Ntrials = npatients,
       control.compute = list(dic = T, waic = T, cpo = T, config = T),
       control.predictor = list(link = 1))
})

# Check summaries 
summary(car.models[[1]])
summary(car.models[[2]])
summary(car.models[[3]])
summary(car.models[[4]])
summary(car.models[[5]])
summary(car.models[[6]])
summary(car.models[[7]])
summary(car.models[[8]])
summary(car.models[[9]])
summary(car.models[[10]])
summary(car.models[[11]])
summary(car.models[[12]])

re_ex1 <- get_random_effects_inla(car.models[[4]], OR= F)
plot_random_effects(re_ex1, plot_map = map_fl)

# A Quick view gives:
# - Precision for ID changes, but that is to be expected since we change its prior? 
# - Other estimates are ver y similar! 
# - So, there is no sensitivyt problem if we focus on the hyper prior of the 
# spatial vairance parameter? 


# Get the mean and sd of each covariate per hyperprior 

# Fixed covariates
get_mean_sd <- function(model){
  return(model$summary.fixed %>% rownames_to_column("covariate"))
}

cov_fix <- lapply(car.models, FUN = get_mean_sd)
cov_fix <- do.call(rbind,cov_fix) %>% 
  rownames_to_column("hyperprior") %>% 
  mutate(hyperprior = gsub("\\.[0-9]$","",hyperprior),
         mean = round(mean,3),
         sd = round(sd,3),
         value = paste0(mean," (",sd,")")) %>% 
  select(hyperprior, covariate,value) %>% 
  pivot_wider(names_from = covariate, values_from = value)

get_mean_sd_random <- function(model){
  
  return(model$summary.hyperpar%>% rownames_to_column("covariate"))
  
}

# Hyperparameters of random covariates 
cov_ran <- lapply(car.models, FUN = get_mean_sd_random)
cov_ran <- do.call(rbind,cov_ran) %>% 
  rownames_to_column("hyperprior") %>% 
  mutate(hyperprior = gsub("\\.[0-9]$","",hyperprior),
         mean = round(mean,3),
         sd = round(sd,3),
         value = paste0(mean," (",sd,")")) %>% 
  select(hyperprior, covariate,value) %>% 
  pivot_wider(names_from = covariate, values_from = value)

# Combine in one table 
table_sens <- left_join(cov_fix,cov_ran, by="hyperprior")

# write.table(table_sens, file = "~/Desktop/table_prior_sensitivity.txt", sep=";", row.names = F, quote = F)





# Now adapt iid hyper prior

# List of hyper priors IID
prior.list.iid <- prior.list.car

iid.models <- lapply(prior.list.iid, function(tau.prior) {
  inla(ncases ~ sex + age_category + increased_compensation +
         f(ID, model = "besag",graph = paste0(dir_data_clean,"map.gra"), scale.model = T) + 
         f(practice_id, model="iid", hyper = tau.prior),
       data = data_inla_model,
       family="binomial",
       Ntrials = npatients,
       control.compute = list(dic = T, waic = T, cpo = T, config = T),
       control.predictor = list(link = 1))
})

summary(iid.models[[1]])
summary(iid.models[[2]])
summary(iid.models[[3]])
summary(iid.models[[4]])
summary(iid.models[[5]])
summary(iid.models[[6]])
summary(iid.models[[7]])
summary(iid.models[[8]])
summary(iid.models[[9]])
# summary(iid.models[[10]])
# summary(iid.models[[11]])
# summary(iid.models[[12]])

# Changing IID precision hyper parameter: Quick view on model estimates
# - Precision practice id remains quite similar 
# - Precision for ID changes a bit, grootte orde blijft hetzeflde 85-115
# - Estimates of fixed effects remains similar (no estimates when loggama(0.5,0.005)))

# Get the mean and sd of each covariate per hyperprior 

# Fixed covariates
get_mean_sd <- function(model){
  return(model$summary.fixed %>% rownames_to_column("covariate"))
}

cov_fix_iid <- lapply(iid.models, FUN = get_mean_sd)
cov_fix_iid <- do.call(rbind,cov_fix_iid) %>% 
  rownames_to_column("hyperprior") %>% 
  mutate(hyperprior = gsub("\\.[0-9]$","",hyperprior),
         mean = round(mean,3),
         sd = round(sd,3),
         value = paste0(mean," (",sd,")")) %>% 
  select(hyperprior, covariate,value) %>% 
  pivot_wider(names_from = covariate, values_from = value)

get_mean_sd_random <- function(model){
  
  return(model$summary.hyperpar%>% rownames_to_column("covariate"))
  
}

# Hyperparameters of random covariates 
cov_ran_iid <- lapply(iid.models, FUN = get_mean_sd_random)
cov_ran_iid <- do.call(rbind,cov_ran_iid) %>% 
  rownames_to_column("hyperprior") %>% 
  mutate(hyperprior = gsub("\\.[0-9]$","",hyperprior),
         mean = round(mean,3),
         sd = round(sd,3),
         value = paste0(mean," (",sd,")")) %>% 
  select(hyperprior, covariate,value) %>% 
  pivot_wider(names_from = covariate, values_from = value)

# Combine in one table 
table_sens_iid <- left_join(cov_fix_iid,cov_ran_iid, by="hyperprior")

# write.table(table_sens_iid, file = "~/Desktop/table_prior_sensitivity_iid.txt", sep=";", row.names = F, quote = F)








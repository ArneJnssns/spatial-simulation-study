# Investigate an inla object

# Specifically, run the BESAG + practice model in 02_....R and look at the resulting 
# inla object 

# Goede bron: https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html


inla_obj <- model_besag_practice

# default control parameters 
inla.set.control.fixed.default()

# Summary statistics of fitted values - mean, sd, CI, median, mode of each row in data 
inla_obj$summary.fitted.values

# Summary statistics of fixed effects 
inla_obj$summary.fixed

# Summary statistics of random effects - per entity per random effect 
inla_obj$summary.random

# Summary statistics of linear predictors  
inla_obj$summary.linear.predictor

# Posterior marginals of fitted values
inla_obj$marginals.fitted.values
plot(inla_obj$marginals.fitted.values$fitted.Predictor.00001)

# Posterior marginals of the fixed effects 
inla_obj$marginals.fixed
plot(inla_obj$marginals.fixed$`age_category(-1,5]`)

# Posterior marginals of random effects 
inla_obj$marginals.random
plot(inla_obj$marginals.random$practice_id$index.1)
plot(inla_obj$marginals.random$ID$index.2)

# Posterior marginals of linear predictors 
inla_obj$marginals.linear.predictor
plot(inla_obj$marginals.linear.predictor$Predictor.1)

# Posterior marginal of hyper parameters - precisions of random effects
inla_obj$marginals.hyperpar
plot(inla_obj$marginals.hyperpar$`Precision for ID`)
plot(inla_obj$marginals.hyperpar$`Precision for practice_id`)

inla_obj$model.random

inla_obj$ok

inla_obj$cpu.used

inla_obj$cpo$cpo


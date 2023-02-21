
model3 <- inla(formula_besag_iid,
               data = data_inla_model,
               family="binomial",
               Ntrials = npatients,
               control.compute = list(dic = T, waic = T, cpo = T, config = F,
                                      hyperpar=T, # default = TRUE
                                      return.marginals = T), # default = TRUE 
               control.predictor = list(link = 1))


model1 <- inla(formula_besag_iid,
                        data = data_inla_model,
                        family="binomial",
                        Ntrials = npatients,
                        control.compute = list(dic = T, waic = T, cpo = T, config = F),
                        control.predictor = list(link = 1))
model2 <- inla(formula_besag_iid,
               data = data_inla_model,
               family="binomial",
               Ntrials = npatients,
               control.compute = list(dic = T, waic = T, cpo = T, config = F,
                                      hyperpar=F),
               control.predictor = list(link = 1))

model3 <- inla(formula_besag_iid,
               data = data_inla_model,
               family="binomial",
               Ntrials = npatients,
               control.compute = list(dic = T, waic = T, cpo = T, config = F,
                                      hyperpar=F,
                                      return.marginals = F),
               control.predictor = list(link = 1))

model4 <- inla(formula_besag_iid,
               data = data_inla_model,
               family="binomial",
               Ntrials = npatients,
               control.compute = list(dic = T, waic = T, cpo = T, config = F,
                                      hyperpar=F,
                                      return.marginals = F,
                                      return.marginals.predictor=T),
               control.predictor = list(link = 1))


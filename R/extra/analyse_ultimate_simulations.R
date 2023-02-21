# Old code to test analysis of very first simulations 

rm(list=ls())

library(tidyverse)
library(patchwork)
library(sf)

# ANALYSE simulations results 

# Working directory
wd <- "/Users/u0121893/OneDrive - KU Leuven/PhD/Research/Simulation/Spatial/Code"
setwd(wd)

# 04012022 work space containing all data for these simulation: created with "balanced_simulation....R" on GitHub
load("./workspace_ultimate_simulation_040122.RData")
# load("./workspace_ultimate_simulation_050122.RData")


# Quick check of data 
#-------------------------------------------------------------------------------

# Simulated practice effect 

## seed 
seed_practice

## effect dataframe
str(practice_effects_df)

## unique values of parameters
practice_unique_params <- lapply(practice_effects_df, unique)

# Simulated spaital/munis effect
seed_spatial
str(spatial_effects_df)
spatial_unique_params <- lapply(spatial_effects_df, unique)

# Simulated LRTI cases in last iteration
seed_simulation
simulated_n_cases_df_all <- do.call(rbind,simulated_n_cases_list)
str(simulated_n_cases_df_all)
simulated_cases_unique_params <- lapply(simulated_n_cases_df_all, unique)

# Estimates obtained from simulated data
str(estimates)
estimates_unique_params <- lapply(estimates, unique)


# Analysis of simulated effects
#-------------------------------------------------------------------------------

# Spatial effects histograms
ggplot(spatial_effects_df, aes(x=simulated_spatial_effect, 
                               fill = as.factor(multipl_factor)))+
  geom_density(alpha=0.6)+
  labs(x = "Simulated spatial effect",
       y = "Density",
       title = "Spatial proceses")+
  scale_fill_discrete("Sd multiplier")+
  theme_bw()

# Spatial effects histograms
ggplot(practice_effects_df %>% 
         filter(n_practices == 1), 
       aes(x=simulated_practice_effect, 
           fill = as.factor(round(multipl_factor,2))))+
  geom_density(alpha=0.6)+
  labs(x = "Simulated practice effect",
       y = "Density",
       title = "Practice effect distributions")+
  scale_fill_discrete("Sd multiplier", 
                      labels = c("0.33"="1/3", "0.5" = "1/2"))+
  theme_bw()



# Analysis of estimates
#-------------------------------------------------------------------------------

# Parameter labels 
labels_fixed <- estimates$param[1:8]
labels_nis <- as.character(1:300)

# Estimates per type of effect 
est_spatial <- estimates %>% 
  filter(param %in% labels_nis)
est_pract <- estimates %>% 
  filter(!param %in% c(labels_fixed,labels_nis))
est_fixed <- estimates %>% 
  filter(param %in% labels_fixed)

# Fixed effect estimates versus input 
est_fixed.in <- model_estimates.server %>% 
  slice(1:8) %>% 
  mutate(param = case_when(
    parameter == "intercept" ~ "(Intercept)",
    parameter == "M" ~ "sexM",
    parameter == "(-1,5]" ~ "age.category(-1,5]",
    parameter == "(5,16]" ~ "age.category(5,16]",
    parameter == "(50,65]" ~ "age.category(50,65]",
    parameter == "(65,85]" ~ "age.category(65,85]",
    parameter == "(85,110]" ~ "age.category(85,110]",
    parameter == "Yes" ~ "increased_compensationYes"
  ))

est_fixed <- est_fixed %>% 
  left_join(est_fixed.in, by = "param", suffix = c(".out",".in"))

# Plot fixed effects estimates and input 
spatial_mf_select <- 1
spatial_real_select <- 4

ggplot(est_fixed %>% filter(spatial_multipl_factor == spatial_mf_select, 
                            spatial_realization == spatial_real_select), 
       aes(x = param, y= mean.out))+
  geom_jitter(alpha=0.6)+
  geom_point(aes(y = mean.in,color="red"), alpha=0.8)+
  facet_grid(as.factor(n_practices)~as.factor(practice_multipl_factor),scales="free")+
  coord_flip()+
  theme_bw()+
  labs(x="Community: West-East",
       y="Estimate",
       title = paste0("Spatial SD multiplier ",spatial_mf_select," and pattern ",spatial_real_select))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_discrete(guide="none")

# Order the municipalities from West to East based on most west X coordinates 
coords_center_t <- st_centroid(map_fl) %>% 
  arrange(NISCODE) %>% 
  mutate(ID=1:300)
coords_center <- st_coordinates(coords_center_t$geometry) 
coords_center_t$X <- coords_center[,"X"]
coords_center_t$Y <- coords_center[,"Y"]

coords_center_t <- as_tibble(coords_center_t) %>% 
  arrange(X) %>% 
  mutate(index=row_number()) %>% 
  select(index, ID, nis_code = NISCODE, name = NAAM, X, Y)  

est_spatial <- est_spatial %>% 
  mutate(ID = as.integer(param)) %>% 
  left_join(coords_center_t %>% 
              select(ID,index),
            by="ID") 

# Add simulated spatial effect to estimates 
est_spatial <- est_spatial %>% 
  left_join(spatial_effects_df %>% mutate(ID = as.integer(ID)),
            by = c("ID","spatial_multipl_factor" = "multipl_factor", "spatial_realization" = "realization")) %>% 
  rename(mean.out = mean, mean.in = simulated_spatial_effect) %>% 
  mutate(practice_multipl_factor = case_when(
    practice_multipl_factor == 1/3 ~ "1/3",
    practice_multipl_factor == 1/2 ~ "1/2",
    TRUE ~ "1"
  ),
  practice_multipl_factor = factor(practice_multipl_factor, levels = c("1","1/2","1/3")))

# Plot of spatial effect: estimated vs simulated 
spatial_mf_select <- 1
spatial_real_select <- 1

ggplot(est_spatial %>% filter(spatial_multipl_factor == spatial_mf_select, 
                              spatial_realization == spatial_real_select) %>% 
         filter(index %in% 1:12), 
       aes(x = as.factor(index), y = mean.out))+
  geom_boxplot()+
  # geom_jitter(alpha=0.6, width=0.25)+
  geom_point(aes(y = mean.in,color="red"), alpha=0.8)+
  facet_grid(as.factor(n_practices)~as.factor(practice_multipl_factor),scales="free")+
  theme_bw()+
  labs(x="Community: West-East",
       y="Estimate",
       title = paste0("Spatial SD multiplier ",spatial_mf_select," and pattern ",spatial_real_select))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_discrete(guide="none")

# Scatter plot
ggplot(t <- est_spatial %>% 
         filter(spatial_multipl_factor == 0.5,
                # n_practices==2,
                spatial_realization == 3),
       aes(x=mean.in,y=mean.out))+
  geom_point()+
  geom_abline(slope=1,intercept=0)

# Mean Absolute Error (MAE) and Mean Squared Error (MSE) over the 15 estimates vs input value
mse_spatial_df <- est_spatial %>% 
  group_by(n_practices, practice_multipl_factor, spatial_multipl_factor) %>% 
  summarise(mse = mean((mean.in-mean.out)^2),
            mae = mean(abs(mean.in - mean.out)))

ggplot(mse_spatial_df,aes(x=as.factor(spatial_multipl_factor),
                          y=mae,
                          color=as.factor(practice_multipl_factor),
                          pch=as.factor(n_practices)),
       group = as.factor(n_practices))+
  theme_bw()+
  geom_point(alpha=0.6,size=3, position = position_dodge(.4))+ # aes(color = as.factor(sp_realization)))+
  # facet_wrap(~as.factor(spatial_multipl_factor))+
  labs(x="Spatial SD multiplier", y = "Mean Absolute Error",title = "Estimated patterns and simulated patterns")+
  scale_shape_discrete("Practices/community:")+
  scale_color_discrete("Practice SD multiplier:")+
  theme(legend.position="right")

mse_spatial_df_per_pattern <- est_spatial %>% 
  group_by(n_practices, practice_multipl_factor, spatial_multipl_factor,spatial_realization) %>% 
  summarise(mse = mean((mean.in-mean.out)^2),
            mae = mean(abs(mean.in - mean.out)))

ggplot(mse_spatial_df_per_pattern,
       aes(x=as.factor(spatial_multipl_factor),
           y=mae,
           color=as.factor(practice_multipl_factor),
           pch=as.factor(n_practices)),
       group = as.factor(n_practices))+
  theme_bw()+
  geom_point(alpha=0.6,size=3, position = position_dodge(.4))+ # aes(color = as.factor(sp_realization)))+
  facet_wrap(~as.factor(spatial_realization))+
  labs(x="Spatial SD multiplier", y = "Mean Absolute Error",title = "Estimated patterns and simulated patterns")+
  scale_shape_discrete("Practices/community:")+
  scale_color_discrete("Practice SD multiplier:")+
  theme(legend.position="right")


# Standardize estimates and inputs in order to compare mae fairly 
est_spatial_stand <- est_spatial %>% 
  group_by(spatial_multipl_factor) %>%
  mutate(mean_mean.in = mean(mean.in),
         mean_mean.out = mean(mean.out),
         sd_mean.in = sd(mean.in),
         sd_mean.out = sd(mean.out),
         mean.out = (mean.out - mean_mean.out) / sd_mean.out,
         mean.in = (mean.in - mean_mean.in) / sd_mean.in)

spatial_mf_select <- 2
spatial_real_select <- 1

p <- ggplot(est_spatial %>% filter(spatial_multipl_factor == spatial_mf_select, 
                                   spatial_realization == spatial_real_select) %>% 
              filter(index %in% 1:300), 
            aes(x = as.factor(index), y = mean.out))+
  geom_jitter(alpha=0.6, width=0.25)+
  geom_point(aes(y = mean.in,color="red"), alpha=0.8)+
  facet_grid(as.factor(n_practices)~as.factor(practice_multipl_factor),scales="free")+
  theme_bw()+
  labs(x="Community: West-East",
       y="Estimate",
       title = paste0("Spatial SD multiplier ",spatial_mf_select," and pattern ",spatial_real_select))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_discrete(guide="none")

p_stand <- ggplot(est_spatial_stand %>% filter(spatial_multipl_factor == spatial_mf_select, 
                                               spatial_realization == spatial_real_select) %>% 
                    filter(index %in% 1:300), 
                  aes(x = as.factor(index), y = mean.out))+
  geom_jitter(alpha=0.6, width=0.25)+
  geom_point(aes(y = mean.in,color="red"), alpha=0.8)+
  facet_grid(as.factor(n_practices)~as.factor(practice_multipl_factor),scales="free")+
  theme_bw()+
  labs(x="Community: West-East",
       y="Estimate",
       title = paste0("Spatial SD multiplier ",spatial_mf_select," and pattern ",spatial_real_select))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_discrete(guide="none")

p/p_stand

sd(est_spatial$mean.out)
sd(est_spatial_stand$mean.out)


# Mean Absolute Error (MAE) and Mean Squared Error (MSE) over the 15 estimates vs input value
mse_spatial_df_stand <- est_spatial_stand %>% 
  group_by(n_practices, practice_multipl_factor, spatial_multipl_factor) %>% 
  summarise(mse = mean((mean.in-mean.out)^2),
            mae = mean(abs(mean.in - mean.out)))

ggplot(mse_spatial_df_stand,
       aes(x=as.factor(spatial_multipl_factor),
           y=mae,
           color=as.factor(practice_multipl_factor),
           pch=as.factor(n_practices)),
       group = as.factor(n_practices))+
  theme_bw()+
  geom_point(alpha=0.6,size=3, position = position_dodge(.4))+ # aes(color = as.factor(sp_realization)))+
  # facet_wrap(~as.factor(spatial_multipl_factor))+
  labs(x="Spatial SD multiplier", y = "Mean Absolute Error",title = "Estimated patterns and simulated patterns")+
  scale_shape_discrete("Practices/community:")+
  scale_color_discrete("Practice SD multiplier:")+
  theme(legend.position="right")


est_spatial_stand_per_pattern <- est_spatial %>% 
  group_by(practice_multipl_factor, n_practices,spatial_multipl_factor, spatial_realization) %>% 
  mutate(mean_mean.in = mean(mean.in),
         mean_mean.out = mean(mean.out),
         sd_mean.in = sd(mean.in),
         sd_mean.out = sd(mean.out),
         mean.out = (mean.out - mean_mean.out) / sd_mean.out,
         mean.in = (mean.in - mean_mean.in) / sd_mean.in)


mse_spatial_df_per_pattern_stand <- est_spatial_stand_per_pattern %>% 
  group_by(n_practices, practice_multipl_factor, spatial_multipl_factor,spatial_realization) %>% 
  summarise(mse = mean((mean.in-mean.out)^2),
            mae = mean(abs(mean.in - mean.out)))

ggplot(mse_spatial_df_per_pattern_stand,
       aes(x=as.factor(spatial_multipl_factor),
           y=mse,
           color=as.factor(practice_multipl_factor),
           pch=as.factor(n_practices)),
       group = as.factor(n_practices))+
  theme_bw()+
  geom_point(alpha=0.6,size=3, position = position_dodge(.4))+ # aes(color = as.factor(sp_realization)))+
  facet_wrap(~as.factor(spatial_realization))+
  labs(x="Spatial SD multiplier", y = "Mean Squared Error",title = "Estimated patterns and simulated patterns")+
  scale_shape_discrete("Practices/community:")+
  scale_color_discrete("Practice SD multiplier:")+
  theme(legend.position="right")


# Map
spatial_mf_select <- 2
spatial_real_select <- 1
n_practices_select <- 1
practice_mf_select <- "1"
simulation_select <- 1

est_map <- map_fl %>% 
  left_join(est_spatial %>% 
              filter(spatial_multipl_factor == spatial_mf_select, 
                     spatial_realization == spatial_real_select,
                     n_practices == n_practices_select,
                     practice_multipl_factor == practice_mf_select,
                     simulation == simulation_select), 
            by = "ID")

p_in <- ggplot(est_map)+
  geom_sf(aes(fill = mean.in))+
  scale_fill_viridis_c()+
  theme_bw()
p_out <- ggplot(est_map)+
  geom_sf(aes(fill = mean.out))+
  scale_fill_viridis_c()+
  theme_bw()

p_in / p_out

# Plot distributions
test <- est_spatial %>% 
  pivot_longer(cols = c("mean.in","mean.out"))
ggplot(test %>% 
         filter(spatial_multipl_factor == spatial_mf_select, 
                spatial_realization == spatial_real_select,
                n_practices == n_practices_select,
                practice_multipl_factor == practice_mf_select), 
       aes(x=value,fill = name))+
  geom_density(alpha=0.3)





# Practice effects 

# Add simulated practice effect to estimates 
est_pract <- est_pract %>% 
  left_join(practice_effects_df,
            by = c("param"="practice_id",
                   "practice_multipl_factor" = "multipl_factor", 
                   "n_practices", 
                   "simulation")) %>% 
  rename(mean.out = mean, mean.in = simulated_practice_effect) %>% 
  mutate(practice_multipl_factor = case_when(
    practice_multipl_factor == 1/3 ~ "1/3",
    practice_multipl_factor == 1/2 ~ "1/2",
    TRUE ~ "1"
  ),
  practice_multipl_factor = factor(practice_multipl_factor, levels = c("1","1/2","1/3")))

ggplot(est_pract %>% filter(simulation==1), 
       aes(x = as.factor(param), y = mean.out))+
  geom_jitter(alpha=0.6, width=0.25)+
  geom_point(aes(y = mean.in,color="red"), alpha=0.8)+
  facet_grid(as.factor(n_practices)~as.factor(practice_multipl_factor),scales="free")+
  theme_bw()+
  labs(x="Community: West-East",
       y="Estimate",
       title = paste0("Spatial SD multiplier ",spatial_mf_select," and pattern ",spatial_real_select))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_discrete(guide="none")

ggplot(t <- est_pract %>% 
         filter(practice_multipl_factor == "1",
                n_practices==1,
                simulation == 5),
       aes(x=mean.in,y=mean.out))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  labs(x = "Practice simulated", y = "Practice effect estimated", title = "1 SD, # practice = 1, simulation 5")

ggplot(t <-est_pract %>% 
         filter(practice_multipl_factor == "1",
                n_practices==1,
                simulation == 5) %>% 
         filter(param %in% unique(est_pract$param)[1:20]), 
       aes(x=param)) + 
  geom_boxplot(aes(y = mean.out), width = 0.4)+
  geom_point(aes(y = mean.in), color = "red")+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(x = "Practice", y = "Estimates", title = "Input (red) versus estimates (boxplot)")

ggplot(t <-est_pract  %>% 
         filter(simulation == param %in% unique(est_pract$param)[1:40]), 
       aes(x=param)) + 
  geom_boxplot(aes(y = mean.out), width = 0.4)+
  geom_point(aes(y = mean.in), color = "red")+
  facet_grid(practice_multipl_factor ~ as.factor(n_practices))+
  theme_bw()+
  theme(axis.text.x = element_blank())

test <- est_pract %>% 
  group_by(n_practices, practice_multipl_factor, simulation) %>% 
  summarise(mse = mean((mean.in-mean.out)^2),
            mae = mean(abs(mean.in - mean.out)))


ggplot(test,aes(x=as.factor(practice_multipl_factor),
                y=mse,
                color=as.factor(simulation),
                pch=as.factor(n_practices)),
       group = as.factor(n_practices))+
  theme_bw()+
  geom_point(alpha=0.6,size=3, position = position_dodge(.4))+ # aes(color = as.factor(sp_realization)))+
  # facet_wrap(~as.factor(spatial_multipl_factor))+
  labs(x="Practice SD multiplier", y = "Mean Squared Error",title = "Estimated patterns and simulated patterns")+
  scale_shape_discrete("Practices/community:")+
  scale_color_discrete("Spatial SD multiplier:")+
  theme(legend.position="right")












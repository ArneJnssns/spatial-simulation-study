# Analysis of Random effects from simulation intego_structure

# 20/12.2022

rm(list=ls())


# Load libraries  ---------------------------------------------------------

library(tidyverse)
# library(vroom)
library(readr)
library(data.table)
library(patchwork)


# Set working directory ---------------------------------------------------

# W.r.t. path where this script is saved 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)

dir_scenario <- "../../github-repo-data/intego_structure/"


# track matrix  -----------------------------------------------------------


# Used to track over all nth_data the parameter combinations 
# - can be used to select a limited number of csv files and test analysis code 
#   It takes very long to read only 1000, thus wait for 4050 --> solution? 
track_matrix <- read.csv(paste0(dir_scenario,"track_matrix.csv")) %>% select(-X)


# Standard deviations (precisions - hyperparameter) -----------------------


# ## Read all, merge, and save into one file ----
# # Can we read all files at once? -> takes like 20' 
setwd(paste0(dir_scenario,"output/hyperpar/standard_deviation/summary_pmarg"))
getwd()
# 
#Read all datasets and merge into one table
# df <- list.files() %>%
#   lapply(read_csv) %>%
#   bind_rows()
# 
# df <- df %>%
#   select(-...1)
# 
# # Save resulting datasets
# saveRDS(df,"all.rds")

## Read final table ---- 
df_sd <- readRDS("all.rds")

### Split in spatial and practice ----
sd_id <- df_sd[df_sd$var=="ID",]
sd_pr <- df_sd[df_sd$var=="practice_id",]


### Add to track_matrix to get corresponding parameter values and NA values ----
sd_id <- track_matrix %>% left_join(sd_id, by="nth_data")
sd_pr <- track_matrix %>% left_join(sd_pr, by="nth_data")

## Investigate models who could not compute sds ----

# NA values ID
sd_id_nan <- sd_id %>% 
  group_by(sd_spatial, sd_practice) %>% 
  summarize(n_nan = sum(is.na(mean))) %>% 
  ggplot(mapping = aes(x=as.factor(sd_practice), y = n_nan))+
  geom_jitter(width=.2,height=0,alpha=.6,aes(size=n_nan))+
  facet_wrap(~as.factor(sd_spatial))+
  scale_size_continuous(guide="none")+
  scale_color_discrete(name="Number of practices")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="SD PRACTICE",
       y = "Number of failures",
       title = "SD SPATIAL")

# NA values practice 
sd_pr_nan <- sd_pr %>% 
  group_by(sd_spatial, sd_practice) %>% 
  summarize(n_nan = sum(is.na(mean))) %>% 
  ggplot(mapping = aes(x=as.factor(sd_practice), y = n_nan))+
  geom_jitter(width=.2,height=0,alpha=.6,aes(size=n_nan))+
  facet_wrap(~as.factor(sd_spatial))+
  scale_size_continuous(guide="none")+
  scale_color_discrete(name="Number of practices")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="SD PRACTICE",
       y = "Number of failures",
       title = "SD SPATIAL")

# Plots combined using pachwork
sd_id_nan / sd_pr_nan # Same message 

sd_id_nan

# Same but better (?) figure ?
sd_id %>% 
  group_by(sd_spatial, sd_practice) %>% 
  summarize(n_nan = sum(is.na(mean))) %>% 
  ggplot(mapping = aes(x=as.factor(sd_practice), y = n_nan))+
  # geom_label(aes(color = as.factor(sd_spatial)), position = position_dodge(width=.3))+
  geom_jitter(width=.3,height=0,alpha=.6,size=4,
              aes(#size=n_nan,
                pch = as.factor(sd_spatial)
                ))+
  scale_color_discrete(name="Number of practices")+
  scale_shape_discrete(name="SD SPATIAL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="SD PRACTICE",
       y = "Number of failures")

# Another version
# sd_id %>% 
#   group_by(sd_spatial, sd_practice) %>% 
#   summarize(n_nan = sum(is.na(mean))) %>% 
#   ggplot(mapping = aes(x=as.factor(sd_practice), y = n_nan, label = as.factor(n_practices)))+
#   geom_label(aes(color = as.factor(sd_spatial)), position = position_dodge(width=.3))+
#   # geom_jitter(width=.3,height=0,alpha=.6,size=6,
#   #             aes(#size=n_nan,
#   #               color=as.factor(n_practices),
#   #               pch = as.factor(sd_spatial)
#   #             ))+
#   scale_color_discrete(name="SD SPATIAL")+
#   theme_bw()+
#   theme(panel.grid = element_blank())+
#   labs(x="SD PRACTICE",
#        y = "Number of failures")


## Distributions of SD estimates  ----

### Spatial ----
sd_spatial_labeller <- c(
  "0.1" = expression(sigma[u]*"= 0.1"),
  "0.3" = expression(sigma[u]*"= 0.3"),
  "0.6" = expression(sigma[u]*"= 0.6")
)

# sd_spatial_labeller <- c(
#   "0.1" = parse(expression(sigma)),
#   "0.3" = expression(beta),
#   "0.6" = expression(alpha)
# )

ggplot(data = sd_id %>% 
         mutate(sd_spatial_label = factor(sd_spatial,
                                          levels = c(0.1,0.3,0.6),
                                          labels = c(expression(bold(sigma[u]*" = 0.1")),
                                                     expression(bold(sigma[u]*" = 0.3")),
                                                     expression(bold(sigma[u]*" = 0.6"))))
                ),
       mapping = aes(x=as.factor(sd_practice), 
                     y = mean))+
  geom_boxplot()+
  geom_hline(aes(yintercept = sd_spatial), size=1)+
  facet_wrap(~sd_spatial_label, scales = "free",  
             labeller = label_parsed)+
  labs(x=expression(bold(sigma[practice])),
       y=expression(hat(sigma)[u]),
       #title=c(expression(delta*"e"))
       )+
  scale_color_discrete("c")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        panel.grid = element_blank())

### Practice ----
ggplot(data = sd_pr, aes(x=as.factor(sd_spatial), y = mean))+
  geom_boxplot()+
  geom_hline(aes(yintercept = sd_practice), size=1)+
  facet_wrap(~as.factor(sd_practice), scales = "free")+
  labs(x="SD SPATIAL",
       y="Estimated SD PRACTICE",
       title="SD PRACTICE")+
  scale_color_discrete("N practices \nper community")+
  theme_bw()


## Summary measure ----

### in conf.int or hpd in this case of estimate ----
test <- sd_id %>% 
  mutate(in_hpd = case_when(
    sd_spatial >= hpd.lower & sd_spatial <= hpd.higher ~ 1,
    TRUE ~ 0
  ))

test <- test %>% group_by(sd_spatial, sd_practice, n_practices) %>% 
  summarize(n_in_hpd = sum(in_hpd),
            n_nin_hpd = sum(in_hpd==0),
            n_total = n(),
            n_p = n_in_hpd/n_total,
            n_failed = 150-n_total)

test %>% 
  ungroup() %>% 
  mutate_at(vars(sd_spatial, sd_practice, n_practices), as.factor) %>% 
  ggplot(aes(x = sd_practice, y = n_p, color = n_practices))+
  geom_point()+
  facet_wrap(~sd_spatial, nrow = 1)+
  labs(x="SD practice",
       y="mean absolute error",
       title="Error between simulated and estimated community effects in different scenarios") + 
  scale_color_discrete("N practices \nper community")+
  theme_bw()




# Fitted values -----------------------------------------------------------

## All csv files ----
setwd("../../../fitted_values/summary")
getwd()

# s <- Sys.time()
# df <- list.files() %>% 
#   lapply(fread) %>% 
#   rbindlist()
# e <- Sys.time()
# e-s
# 
# arrow::write_parquet(df,"all.parquet")

p_f <- arrow::read_parquet("all.parquet")

## Simulated probs ----
setwd("../../../simulated_cases")
getwd()

# 9' 
# s <- Sys.time()
# df <- list.files() %>%
#   lapply(fread) %>%
#   rbindlist()
# e <- Sys.time()
# e-s
# df <- df %>% select(-V1)
# arrow::write_parquet(df,"all.parquet")

p_s <- arrow::read_parquet("all.parquet")

colnames(p_f)
colnames(p_s)
# p_f$predictor <- readr::parse_number(gsub("\\.","",p_f$var)) # unneccessry step (V1 = predictor) 

## Simulated (true) valeus and estimates values ----
p_all <- p_s %>% 
  select(-sim_ncases) %>% 
  left_join(p_f %>% 
              select(-var), 
            by = c("predictor" = "V1","nth_data"))
colnames(p_all)

## Summary measures ----
test <- p_all %>% 
  group_by(nth_data) %>% 
  summarize(rmse = sqrt(mean((sim_props-mean)^2)),
            in_ci = sum(sim_props >= `0.025quant` & sim_props <= `0.975quant`),
            mean_s = mean(sim_props),
            mean_f = mean(mean))

test <- test %>% 
  left_join(track_matrix, by = "nth_data")

test %>% 
  ungroup() %>% 
  mutate_at(vars(sd_spatial, sd_practice, n_practices), as.factor) %>% 
  ggplot(aes(x = sd_practice, y = mean_s, color = n_practices))+
  geom_boxplot()+
  facet_wrap(~sd_spatial, nrow = 1)+
  labs(x="SD practice",
       y="mean absolute error",
       title="Error between simulated and estimated community effects in different scenarios") + 
  scale_color_discrete("N practices \nper community")+
  theme_bw()

## Summary measures on "standardized" data ---
test2 <- p_all %>% 
  left_join(track_matrix, by = "nth_data")
colnames(test2)

### Simple linear regression ----
# Makes more sense than just computing rmse? 
test2_simple_regr <- test2 %>% 
  select(sd_spatial,sd_practice,n_practices,simulation,sim_props,mean) %>% 
  nest(data=c("mean","sim_props")) %>% 
  mutate(slope = map(data, ~coef(lm(mean ~ sim_props, data = .))[["sim_props"]]),
         intercept = map(data, ~coef(lm(mean ~ sim_props, data = .))[["(Intercept)"]]),
         R2 = map(data, ~cor(.$mean,.$sim_props,use="complete.obs")^2),
         rmse = map(data, ~sqrt(mean((.$sim_props-.$mean)^2,na.rm=T)))) %>%
  select(-data) %>% 
  unnest(cols = c(-sd_spatial,-sd_practice,-n_practices,-simulation))
  
ggplot(data = test2_simple_regr, 
       mapping=aes(x=as.factor(sd_practice), y=R2, color=as.factor(n_practices)))+
  geom_boxplot()+
  facet_wrap(~as.factor(sd_spatial))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="SD PRACTICE",
       title = "SD SPATIAL")+
  scale_color_discrete("Number of practices")

### standardize and compute rmse ----
test2 <- test2 %>% 
  group_by(sd_spatial,sd_practice) %>% 
  mutate(mean_s = mean(sim_props),
         sd_s = sqrt(var(sim_props)/n()),
         mean_f = mean(mean),
         sd_f = sqrt(var(mean)/n()),
         sim_props = (sim_props-mean_s)/sd_s,
         mean = (mean-mean_s)/sd_s)

test2 <- test2 %>% 
  group_by(nth_data) %>% 
  summarize(rmse = sqrt(mean((sim_props-mean)^2)),
            in_ci = sum(sim_props >= `0.025quant` & sim_props <= `0.975quant`),
            mean_s = mean(sim_props),
            mean_f = mean(mean))

test2 <- test2 %>% 
  left_join(track_matrix, by = "nth_data")

test2 %>% 
  ungroup() %>% 
  mutate_at(vars(sd_spatial, sd_practice, n_practices), as.factor) %>% 
  ggplot(aes(x = sd_practice, y = rmse, color = n_practices))+
  geom_boxplot()+
  facet_wrap(~sd_spatial, nrow = 1)+
  labs(x="SD practice",
       y="mean absolute error",
       title="Error between simulated and estimated community effects in different scenarios") + 
  scale_color_discrete("N practices \nper community")+
  theme_bw()

p_all %>% 
  filter(nth_data==3000) %>% 
  ggplot(aes(x=sim_props,y=mean))+
  geom_point()+
  stat_smooth()


# Spatial effects --------------------------------------------------------------


# Can we read all files at once? -> takes like 20' 
setwd(paste0("/Users/u0121893/Library/CloudStorage/OneDrive-KULeuven/phd/projects/",
             "simulation/spatial/github-repo-data/intego_structure/",
             "output/random/ID/summary"))
getwd()

# s <- Sys.time()
# df <- list.files() %>%
#   lapply(read_csv) %>%
#   bind_rows()
# e <- Sys.time()
# e-s # 8'
# 
# df <- df %>%
#   select(-...1)
# 
# # Save resulting datasets
# saveRDS(df,"all.rds")

# Estimates
sp_all <- readRDS("all.rds")

# True values (simulated)
setwd("../../../..")
getwd()
sp_sim <- data.table::fread(file="simulated_spatial_effects.csv",
                                       showProgress = T,
                                       check.names = T) %>% 
  select(-V1) 

sp_test <- sp_all %>% 
  left_join(sp_sim, by=c("ID","nth_data")) 

sp_test2 <- sp_test %>% 
  group_by(sd_spatial,sd_practice,simulation) %>% 
  summarize(mae = mean(abs(mean-simulated)))


ggplot(data = sp_test2, aes(x=as.factor(sd_practice), y = mae))+
  geom_boxplot()+
  # geom_hline(aes(yintercept = simula), size=1)+
  facet_wrap(~as.factor(sd_spatial), scales = "fixed")+
  labs(x="SD PRACTICE",
       y="MAE",
       title="Boxplot of MAE spatial effect (N=150)")+
  theme_bw()


sp_test3 <- sp_test %>% 
  mutate(mf = case_when(
    sd_spatial == "0.1" ~ 1,
    sd_spatial == "0.3" ~ 1/3,
    TRUE ~ 1/6
  ),
  simulated_new = simulated*mf,
  mean_new = mean*mf) %>% 
  group_by(sd_spatial,sd_practice,simulation) %>% 
  summarize(mae = mean(abs(mean_new-simulated_new)))


# Plot of Boxplots of MAEs for spatial effect after standardizing on sigma_spatial 
ggplot(data = sp_test3 %>% 
         mutate(sd_spatial_label = factor(sd_spatial,
                                          levels = c(0.1,0.3,0.6),
                                          labels = c(expression(bold(sigma[u]*" = 0.1")),
                                                     expression(bold(sigma[u]*" = 0.3")),
                                                     expression(bold(sigma[u]*" = 0.6"))))
         ), 
       mapping = aes(x=as.factor(sd_practice), 
                     y = mae))+
  geom_boxplot()+
  # geom_hline(aes(yintercept = simula), size=1)+
  facet_wrap(~sd_spatial_label, scales = "fixed", 
             labeller = label_parsed)+
  labs(x=expression(bold(sigma[practice])),
       y=expression(mae[u[i]]),
       #title="Boxplot of MAE spatial effect (N=150)"
       )+
  scale_color_discrete("c")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        panel.grid = element_blank())





# Practice effects --------------------------------------------------------------


# Can we read all files at once? -> takes like 20' 
setwd(paste0("/Users/u0121893/Library/CloudStorage/OneDrive-KULeuven/phd/projects/",
             "simulation/spatial/github-repo-data/intego_structure/",
             "output/random/practice_id/summary"))
getwd()

# s <- Sys.time()
# df <- list.files() %>%
#   lapply(read_csv) %>%
#   bind_rows()
# e <- Sys.time()
# e-s # 26'
# 
# df <- df %>%
#   select(-...1)
# 
# # Save resulting datasets
# saveRDS(df,"all.rds")

# Estimates
pr_all <- readRDS("all.rds")

# True values (simulated)
setwd("../../../..")
getwd()
pr_sim <- data.table::fread(file="simulated_practice_effects.csv",
                            showProgress = T,
                            check.names = T) %>% 
  select(-V1) 

pr_test <- pr_all %>%
  rename(practice_id = ID) %>% 
  left_join(pr_sim, by=c("practice_id","nth_data")) 

pr_test2 <- pr_test %>% 
  group_by(sd_spatial,sd_practice,n_practices,simulation) %>% 
  summarize(mae = mean(abs(mean-simulated)))


ggplot(data = pr_test2, aes(x=as.factor(sd_practice), y = mae, color=as.factor(n_practices)))+
  geom_boxplot()+
  # geom_hline(aes(yintercept = simula), size=1)+
  facet_wrap(~as.factor(sd_spatial), scales = "free")+
  labs(x="SD PRACTICE",
       y="MAE",
       title="Boxplot of MAE spatial effect (N=150)")+
  scale_color_discrete("N practices \nper community")+
  theme_bw()


pr_test3 <- pr_test %>% 
  mutate(mf = case_when(
    sd_practice == "0.1" ~ 1,
    sd_practice == "0.3" ~ 1/3,
    TRUE ~ 1/6
  ),
  simulated_new = simulated*mf,
  mean_new = mean*mf) %>% 
  group_by(sd_spatial,sd_practice,n_practices,simulation) %>% 
  summarize(mae = mean(abs(mean_new-simulated_new)))

ggplot(data = pr_test3, aes(x=as.factor(sd_practice), y = mae, color=as.factor(n_practices)))+
  geom_boxplot()+
  # geom_hline(aes(yintercept = simula), size=1)+
  facet_wrap(~as.factor(sd_spatial))+
  labs(x="SD PRACTICE",
       y="MAE",
       title="Boxplot of MAE spatial effect (N=150)")+
  scale_color_discrete("N practices \nper community")+
  theme_bw()





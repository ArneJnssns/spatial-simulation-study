# 20/12/2022


# Introduction ------------------------------------------------------------

# Script dat de outputs van twee simulaties analyseert
# 1) balanced: 27 parameter combinaties 
# 2) unbalanced (intego): 9 parameter combinaties 


rm(list=ls())

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
getwd()


# Get data ----------------------------------------------------------------

## Unbalanced ----

# Track matrix 
track_unbalanced <- read.csv(paste0("../../github-repo-data/intego_structure/",
                                    "track_matrix.csv")) %>% select(-X)
# Posterior summary (spatial patterns)
sp_unbalanced <- readRDS(paste0("../../github-repo-data/intego_structure/",
                              "output/random/ID/summary/","all.rds"))
# Spatial sigma 
sp_sig_unbalanced <- readRDS(paste0("../../github-repo-data/intego_structure/",
               "output/hyperpar/standard_deviation/summary_pmarg/","all.rds"))

# Simulated spatial patterns
sp_unbalanced_sim <- data.table::fread(file=paste0("../../github-repo-data/intego_structure/",
                                       "simulated_spatial_effects.csv"),
                                       showProgress = T,
                                       check.names = T) %>% 
  select(-V1)

sp_unbalanced_all <- sp_unbalanced %>% 
  left_join(sp_unbalanced_sim, by=c("ID","nth_data"))

## Balanced ----
# Track matrix
track_balanced <- read.csv(paste0("../../github-repo-data/balanced_scenario/",
                                  "track_matrix.csv")) %>% select(-X)

# Posterior summary (spatial patterns)
sp_balanced <- readRDS(paste0("../../github-repo-data/balanced_scenario/",
                              "output/random/ID/summary/","all.rds"))

# Spatial sigma 
sp_sig_balanced <- readRDS(paste0("../../github-repo-data/balanced_scenario/",
                                    "output/hyperpar/standard_deviation/summary_pmarg/","all.rds"))

# Simulated spatial patterns
sp_balanced_sim <- data.table::fread(file=paste0("../../github-repo-data/balanced_scenario/",
                                                   "simulated_spatial_effects.csv"),
                                       showProgress = T,
                                       check.names = T) %>% 
  select(-V1)

# Check waarom sp_balanced_sim meer entries heeft dan sp_balanced
# - Zal zoals ervoor komen door een paar gefaalde modellen 

sp_balanced_all <- sp_balanced %>% 
  left_join(sp_balanced_sim, by=c("ID","nth_data")) 


# Spatial pattern (posterior means) ---------------------------------------

## Compute mean absolute error  ----

# Balanced
sp_balanced_mae <- sp_balanced_all %>% 
  mutate(mf = case_when(
    sd_spatial == "0.1" ~ 1,
    sd_spatial == "0.3" ~ 1/3,
    TRUE ~ 1/6
  ),
  # Standardize 
  simulated_new = simulated*mf,
  mean_new = mean*mf) %>% 
  group_by(sd_spatial,sd_practice,n_practices,simulation) %>% 
  summarize(mae = mean(abs(mean_new-simulated_new)))

# Unbalanced
sp_unbalanced_mae <- sp_unbalanced_all %>% 
  mutate(mf = case_when(
    sd_spatial == "0.1" ~ 1,
    sd_spatial == "0.3" ~ 1/3,
    TRUE ~ 1/6
  ),
  # Standardize
  simulated_new = simulated*mf,
  mean_new = mean*mf) %>% 
  group_by(sd_spatial,sd_practice,simulation) %>% 
  summarize(mae = mean(abs(mean_new-simulated_new)))

## Plot of MAEs ----

# Balanced
ggplot(data = sp_balanced_mae %>% 
         mutate(sd_spatial_label = factor(sd_spatial,
                                          levels = c(0.1,0.3,0.6),
                                          labels = c(expression(bold(sigma[u]*" = 0.1")),
                                                     expression(bold(sigma[u]*" = 0.3")),
                                                     expression(bold(sigma[u]*" = 0.6"))))
         ), 
       mapping = aes(x=as.factor(sd_practice), 
                     y = mae,
                     color= as.factor(n_practices)))+
  geom_boxplot()+
  # geom_hline(aes(yintercept = simula), size=1)+
  facet_wrap(~sd_spatial_label, scales = "fixed", 
             labeller = label_parsed)+
  labs(x=expression(bold(sigma[practice])),
       y=expression(mae[u[i]]),
       title="Balanced"
  )+
  scale_color_discrete("c")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        panel.grid = element_blank())

# Unbalanced
ggplot(data = sp_unbalanced_mae %>% 
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
       title="Unbalanced (intego structure)"
  )+
  scale_color_discrete("c")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        panel.grid = element_blank())


## Combine balanced and unbalanced ----

sp_mae <- rbind(sp_balanced_mae %>% mutate(scen = "balanced"),
                sp_unbalanced_mae %>% mutate(scen = "unbalanced")) %>% 
  ungroup() %>% 
  mutate(n_practices = case_when(
    !is.na(n_practices) ~ paste0("Balanced: c = ",n_practices,sep = ""),
    TRUE ~ "Unbalanced"
  ))

c <- c("Balanced: c = 1" = "skyblue4",
       "Balanced: c = 2" = "skyblue3",
       "Balanced: c = 3" = "skyblue2",
       "Unbalanced" = "palegreen3")

ggplot(data = sp_mae %>% 
         mutate(sd_spatial_label = factor(sd_spatial,
                                          levels = c(0.1,0.3,0.6),
                                          labels = c(expression(bold(sigma[u]*" = 0.1")),
                                                     expression(bold(sigma[u]*" = 0.3")),
                                                     expression(bold(sigma[u]*" = 0.6")))),
         ), 
       mapping = aes(x=as.factor(sd_practice), 
                     y = mae,
                     color =  n_practices,
                     # linetype = scen
                     ))+
  geom_boxplot()+
  # geom_hline(aes(yintercept = simula), size=1)+
  facet_wrap(~sd_spatial_label, scales = "fixed", 
             labeller = label_parsed)+
  labs(x=expression(bold(sigma[practice])),
       y=expression(mae[u[i]]),
       #title="Boxplot of MAE spatial effect (N=150)"
  )+
  scale_color_manual("",values = c)+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        #panel.grid = element_blank()
        )+
  scale_y_continuous(n.breaks = 10)

# ggplot(data = sp_mae %>% 
#          mutate(sd_spatial_label = factor(sd_spatial,
#                                           levels = c(0.1,0.3,0.6),
#                                           labels = c(expression(bold(sigma[u]*" = 0.1")),
#                                                      expression(bold(sigma[u]*" = 0.3")),
#                                                      expression(bold(sigma[u]*" = 0.6")))),
#                 sd_practice_label = factor(sd_practice,
#                                            levels = c(0.1,0.3,0.6),
#                                            labels = c(expression(bold(sigma[practice]*" = 0.1")),
#                                                       expression(bold(sigma[practice]*" = 0.3")),
#                                                       expression(bold(sigma[practice]*" = 0.6"))))
#          ), 
#        mapping = aes(x=n_practices, 
#                      y = mae,
#                      color =  scen,
#                      # linetype = scen
#        ))+
#   geom_boxplot()+
#   # geom_hline(aes(yintercept = simula), size=1)+
#   facet_grid(sd_practice_label~sd_spatial_label, scales = "fixed", 
#              labeller = label_parsed)+
#   labs(x="",
#        y=expression(mae[u[i]]),
#        #title="Boxplot of MAE spatial effect (N=150)"
#   )+
#   scale_color_manual("", values = c("balanced" = "skyblue3", "unbalanced" = "palegreen3"))+
#   theme_bw()+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=11),
#         #axis.text = element_text(size=10),
#         axis.title.x = element_text(size=11),
#         legend.title = element_text(size=11, face="bold"),
#         #panel.grid = element_blank()
#   )
#   # scale_y_continuous(n.breaks = 10)


# Spatial variation (SD/sigma) ---------------------------------------

## Get spatial sigma from tables ----

# Balanced 
sd_id_balanced <- sp_sig_balanced[sp_sig_balanced$var=="ID",]
sd_pr_balanced <- sp_sig_balanced[sp_sig_balanced$var=="practice_id",]

# Unbalanced 
sd_id_unbalanced <- sp_sig_unbalanced[sp_sig_unbalanced$var=="ID",]
sd_pr_unbalanced <- sp_sig_unbalanced[sp_sig_unbalanced$var=="practice_id",]

## Add parameters with track matrix ----
sd_id_balanced <- track_balanced %>% left_join(sd_id_balanced, by="nth_data")
sd_id_unbalanced <- track_unbalanced %>% left_join(sd_id_unbalanced, by="nth_data")
sd_pr_balanced <- track_balanced %>% left_join(sd_pr_balanced, by="nth_data")
sd_pr_unbalanced <- track_unbalanced %>% left_join(sd_pr_unbalanced, by="nth_data")

## Distribution of SD spatial ----

sd_spatial_labeller <- c(
  "0.1" = expression(sigma[u]*"= 0.1"),
  "0.3" = expression(sigma[u]*"= 0.3"),
  "0.6" = expression(sigma[u]*"= 0.6")
)

### Balanced ----
ggplot(data = sd_id_balanced %>% 
         mutate(sd_spatial_label = factor(sd_spatial,
                                          levels = c(0.1,0.3,0.6),
                                          labels = c(expression(bold(sigma[u]*" = 0.1")),
                                                     expression(bold(sigma[u]*" = 0.3")),
                                                     expression(bold(sigma[u]*" = 0.6"))))
         ),
       mapping = aes(x=as.factor(sd_practice), 
                     y = mean,
                     color = as.factor(n_practices)))+
  geom_boxplot()+
  geom_hline(aes(yintercept = sd_spatial), size=1)+
  facet_wrap(~sd_spatial_label, scales = "free",  
             labeller = label_parsed)+
  labs(x=expression(bold(sigma[practice])),
       y=expression(hat(sigma)[u]),
       title="Balanced"
  )+
  scale_color_discrete("c")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        panel.grid = element_blank())

### Unbalanced ----
ggplot(data = sd_id_unbalanced %>% 
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
       title="Unbalanced (intego structure)"
  )+
  scale_color_discrete("c")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        panel.grid = element_blank())

### Combined balanced and unbalanced ---- 

sd_id_all <- bind_rows(sd_id_balanced %>% mutate(scen = "balanced"),
                    sd_id_unbalanced %>% mutate(scen = "unbalanced")) %>% 
  ungroup() %>% 
  mutate(n_practices = case_when(
    !is.na(n_practices) ~ paste0("Balanced: c = ",n_practices,sep = ""),
    TRUE ~ "Unbalanced"
  ))

ggplot(data = sd_id_all %>% 
         mutate(sd_spatial_label = factor(sd_spatial,
                                          levels = c(0.1,0.3,0.6),
                                          labels = c(expression(bold(sigma[u]*" = 0.1")),
                                                     expression(bold(sigma[u]*" = 0.3")),
                                                     expression(bold(sigma[u]*" = 0.6"))))
         ),
       mapping = aes(x=as.factor(sd_practice), 
                     y = mean,
                     color = n_practices))+
  geom_boxplot()+
  geom_hline(aes(yintercept = sd_spatial), size=1)+
  facet_wrap(~sd_spatial_label, scales = "free",  
             labeller = label_parsed)+
  labs(x=expression(bold(sigma[practice])),
       y=expression(hat(sigma)[u]),
       title=""
  )+
  scale_color_manual("", values = c)+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=11),
        #axis.text = element_text(size=10),
        axis.title.x = element_text(size=11),
        legend.title = element_text(size=11, face="bold"),
        # panel.grid = element_blank()
        )

# # In a grid
# ggplot(data = sd_id_all %>% 
#          mutate(sd_spatial_label = factor(sd_spatial,
#                                           levels = c(0.1,0.3,0.6),
#                                           labels = c(expression(bold(sigma[u]*" = 0.1")),
#                                                      expression(bold(sigma[u]*" = 0.3")),
#                                                      expression(bold(sigma[u]*" = 0.6")))),
#                 sd_practice_label = factor(sd_practice,
#                                            levels = c(0.1,0.3,0.6),
#                                            labels = c(expression(bold(sigma[practice]*" = 0.1")),
#                                                       expression(bold(sigma[practice]*" = 0.3")),
#                                                       expression(bold(sigma[practice]*" = 0.6"))))
#          ), 
#        mapping = aes(x=n_practices, 
#                      y = mean,
#                      color =  scen,
#                      # linetype = scen
#        ))+
#   geom_boxplot()+
#   geom_hline(aes(yintercept = sd_spatial), size=.5)+
#   facet_grid(sd_practice_label~sd_spatial_label, scales = "fixed", 
#              labeller = label_parsed)+
#   labs(x=expression(bold(c)),
#        y=expression(mae[u[i]]),
#        #title="Boxplot of MAE spatial effect (N=150)"
#   )+
#   scale_color_manual("", values = c("balanced" = "skyblue3", "unbalanced" = "palegreen3"))+
#   theme_bw()+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=11),
#         #axis.text = element_text(size=10),
#         axis.title.x = element_text(size=11),
#         legend.title = element_text(size=11, face="bold"),
#         #panel.grid = element_blank()
#   )
# # scale_y_continuous(n.breaks = 10)



# Test normal distriutnio

n1 <- rnorm(n=1000,mean=0,sd=1)
n2 <- rnorm(n=1000,mean=0,sd=3)
n3 <- rnorm(n=1000,mean=0,sd=10)

hist(n3)
hist(n2, add=T, col="blue")
hist(n1,add=T,col="green")
hist(n2/3,add=T,col="red")
hist(n3/10, col="red",add=T)

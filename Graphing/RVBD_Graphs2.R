#load library
library(ggplot2)
library(RMark)
library(patchwork)
library(dplyr)
#setup working directories
repo <- 'C:/Users/rcscott/VTADS'
data <- paste(repo,"/Data", sep = "")
graphs <- paste(repo,"/Graphing", sep = "")
# Load necessary models
setwd(data)

BD_mods <- readRDS("BD_mods.RDS")
RV_mods <- readRDS("RV_mods.RDS")

load("BDMarkData.RData")
load("RVMarkData.RData")

load("TempDataOccMod.RData")
range(TemData2022_OccMod[,3:5],na.rm=T)
range(TemData2024_OccMod[,3:5],na.rm=T)

#Create new data for prediction

alpha <- 1:7 #number of possible species
beta <- seq(from = 0, to = 0.5, by = 0.1)
temp <- seq(from = 63, to = 80, by = 1)

temp_df <- data.frame(temp1 = temp)
alpha_df <- data.frame(alpha = alpha)
beta_df <- data.frame(beta = beta)

### BD

# Okay so to start lets look at the mods again
View(BD_mods$model.table)
# Null model is the top model, but it "only" received 64% of the model
# weight. I think mod avg could be worth exploring
# I think I'd like to look at whether there a trend in the following
# Theta ~ temp, Theta ~ alpha, Theta ~ beta, p ~ temp, p ~ alpha, p ~ beta


# Theta ~ temp
BD_ThetaTemp <- covariate.predictions(model = BD_mods,
                                   data = temp_df,
                                   indices = c(2,14,26))
BD_ThetaTemp_df <- BD_ThetaTemp$estimates %>% 
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

BD_ThetaTemp_plot <- ggplot(BD_ThetaTemp_df, mapping = aes(x = covdata,
                                                     y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Temp") + 
  ylab("Prevalence")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
BD_ThetaTemp_plot

# Theta ~ alpha
BD_ThetaAlpha <- covariate.predictions(model = BD_mods,
                                   data = alpha_df,
                                   indices = c(2,14,26))
BD_ThetaAlpha_df <- BD_ThetaAlpha$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

BD_ThetaAlpha_plot <- ggplot(BD_ThetaAlpha_df, mapping = aes(x = covdata,
                                                     y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Alpha") + 
  ylab("Prevalence")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
BD_ThetaAlpha_plot

# Theta ~ beta
BD_ThetaBeta <- covariate.predictions(model = BD_mods,
                                    data = beta_df,
                                    indices = c(2,14,26))
BD_ThetaBeta_df <- BD_ThetaBeta$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

BD_ThetaBeta_plot <- ggplot(BD_ThetaBeta_df, mapping = aes(x = covdata,
                                                       y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Beta") + 
  ylab("Prevalence")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
BD_ThetaBeta_plot

# p ~ temp
BD_pTemp <- covariate.predictions(model = BD_mods,
                                  data = temp_df,
                                  indices = c(38,62,86))

BD_pTemp_df <- BD_pTemp$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

BD_pTemp_plot <- ggplot(BD_pTemp_df, mapping = aes(x = covdata,
                                                           y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Temperature") + 
  ylab("Detection")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
BD_pTemp_plot

# p ~ Alpha

BD_pAlpha <- covariate.predictions(model = BD_mods,
                                  data = alpha_df,
                                  indices = c(38,62,86))

BD_pAlpha_df <- BD_pAlpha$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

BD_pAlpha_plot <- ggplot(BD_pAlpha_df, mapping = aes(x = covdata,
                                                   y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Alpha") + 
  ylab("Detection")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
BD_pAlpha_plot

# p ~ beta
BD_pBeta <- covariate.predictions(model = BD_mods,
                                  data = beta_df,
                                  indices = c(38,62,86))

BD_pBeta_df <- BD_pBeta$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

BD_pBeta_plot <- ggplot(BD_pBeta_df, mapping = aes(x = covdata,
                                                   y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Beta") + 
  ylab("Detection")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
BD_pBeta_plot

### RV Plots
View(RV_mods$model.table)

# Okay so I want to plot theta ~ temp,alpha, beta
# and p ~ temp, alpha, beta

# Theta ~ temp
RV_ThetaTemp <- covariate.predictions(model = RV_mods,
                                      data = temp_df,
                                      indices = c(2,14,26))
RV_ThetaTemp_df <- RV_ThetaTemp$estimates %>% 
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

RV_ThetaTemp_plot <- ggplot(RV_ThetaTemp_df, mapping = aes(x = covdata,
                                                           y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Temperature") + 
  ylab("Prevalence")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
RV_ThetaTemp_plot

# Theta ~ alpha
RV_ThetaAlpha <- covariate.predictions(model = RV_mods,
                                       data = alpha_df,
                                       indices = c(2,14,26))
RV_ThetaAlpha_df <- RV_ThetaAlpha$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

RV_ThetaAlpha_plot <- ggplot(RV_ThetaAlpha_df, mapping = aes(x = covdata,
                                                             y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Alpha") + 
  ylab("Prevalence")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
RV_ThetaAlpha_plot

# Theta ~ beta
RV_ThetaBeta <- covariate.predictions(model = RV_mods,
                                      data = beta_df,
                                      indices = c(2,14,26))
RV_ThetaBeta_df <- RV_ThetaBeta$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

RV_ThetaBeta_plot <- ggplot(RV_ThetaBeta_df, mapping = aes(x = covdata,
                                                           y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Beta") + 
  ylab("Prevalence")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
RV_ThetaBeta_plot

# p ~ temp
RV_pTemp <- covariate.predictions(model = RV_mods,
                                  data = temp_df,
                                  indices = c(38,62,86))

RV_pTemp_df <- RV_pTemp$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

RV_pTemp_plot <- ggplot(RV_pTemp_df, mapping = aes(x = covdata,
                                                   y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Temperature") + 
  ylab("Detection")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
RV_pTemp_plot

# p ~ Alpha

RV_pAlpha <- covariate.predictions(model = RV_mods,
                                   data = alpha_df,
                                   indices = c(38,62,86))

RV_pAlpha_df <- RV_pAlpha$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

RV_pAlpha_plot <- ggplot(RV_pAlpha_df, mapping = aes(x = covdata,
                                                     y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Alpha") + 
  ylab("Detection")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
RV_pAlpha_plot

# p ~ beta
RV_pBeta <- covariate.predictions(model = RV_mods,
                                  data = beta_df,
                                  indices = c(38,62,86))

RV_pBeta_df <- RV_pBeta$estimates %>%
  group_by(covdata) %>%
  summarise(estimate = mean(estimate), se = mean(se)) %>%
  mutate(lcl = estimate - se) %>%
  mutate(ucl = estimate + se)

RV_pBeta_plot <- ggplot(RV_pBeta_df, mapping = aes(x = covdata,
                                                   y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  #scale_colour_brewer(palette = "Set1") +
  xlab("Beta") + 
  ylab("Detection")+
  #geom_rug(data = CoOccurrence2, mapping = aes(x = as.numeric(scale(mean_water_temp)), 
  #                                             y = NULL, color = NULL), sides = "b")+
  theme_classic()+
  ylim(c(0,1))
RV_pBeta_plot


# Stitch all the plots together
BD_plots <- (BD_ThetaTemp_plot + BD_ThetaAlpha_plot + BD_ThetaBeta_plot) / 
  (BD_pTemp_plot + BD_pAlpha_plot + BD_pBeta_plot) + plot_annotation(title = "Bd")
BD_plots

RV_plots <- (RV_ThetaTemp_plot + RV_ThetaAlpha_plot + RV_ThetaBeta_plot) / 
  (RV_pTemp_plot + RV_pAlpha_plot + RV_pBeta_plot) + plot_annotation(title = "Ranavirus")
RV_plots

setwd(graphs)
ggsave("BD_plots.png",BD_plots)
ggsave("RV_plots.png",RV_plots)

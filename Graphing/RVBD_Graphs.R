# Graphing for models of BD and RV
# RCS
# 10/01/2025

#load library
library(ggplot2)
library(unmarked)
library(patchwork)
#setup working directories
repo <- 'C:/Users/rcscott/VTADS'
data <- paste(repo,"/Data", sep = "")
graphs <- paste(repo,"/Graphing", sep = "")
# Load necessary models
setwd(data)

BD2022_mods <- readRDS("BD2022_mods.RDS")
load("BD2022MarkData.RData")
BD2024_mods <- readRDS("BD2024_mods.RDS")

RV2022_mods <- readRDS("RV2022_mods.RDS")
load("RV2022MarkData.RData")
RV2024_mods <- readRDS("RV2024_mods.RDS")








############
#2022######
###########

#Create new data for prediction

alpha <- 1:7 #number of possible species
beta <- seq(from = 0, to = 0.5, by = 0.1)
Temp <- seq(from = -1.6, to = 1.4, by = 0.1)

temp_df <- data.frame(Temp = Temp)
alpha_df <- data.frame(alpha = rep(alpha, each = length(Temp)), Temp = rep(Temp, times = length(alpha)))
beta_df <- data.frame(beta = rep(beta, each = length(Temp)), Temp = rep(Temp, times = length(beta)))

### BD ###

### 2022 ###

View(BD2022_mods$model.table)
BD2022_mods[[25]]$results$beta
BD2022_mods[[27]]$results$beta

# Create param models
# For psi
TempPsi <- list(formula=~Temp + ComplexID)
AlphaPsi <- list(formula =~ alpha + Temp + ComplexID)
BetaPsi <- list (formula =~ beta + Temp + ComplexID)

# For Theta
TempTheta <-  list(formula=~Temp + ComplexID)
AlphaTempTheta <-  list(formula =~ alpha + Temp + ComplexID)
BetaTempTheta <- list (formula =~ beta + Temp + ComplexID)

# For p
TempP <-  list(formula=~Temp + pSurvey1_ + pSurvey2_ + pSurvey3_ + ComplexID)
AlphaTempP <-  list(formula =~ alpha + Temp + pSurvey1_ + pSurvey2_ + pSurvey3_ + ComplexID)
BetaTempP <- list (formula =~ beta + Temp + pSurvey1_ + pSurvey2_ + pSurvey3_ + ComplexID)

# Want to model Psi, theta and p ~ temp and p ~ alpha

# PSi ~ Temp
ComplexFix <- c("4","5","8","11","12")
BD2022.ddl.PsiTemp <- BD2022.ddl
BD2022.ddl.PsiTemp$Psi$fix <- NA
BD2022.ddl.PsiTemp$Psi$fix <- ifelse(BD2022.ddl.PsiTemp$Psi$ComplexID %in% ComplexFix,
                                     1, NA)


BD2022_TempPsi <- mark(data = BD2022.pr,
                            ddl = BD2022.ddl.PsiTemp,
                            model.parameters = list(Psi = TempPsi,
                                                    Theta = TempTheta,
                                                    p = TempP))
PsiTemp <- covariate.predictions(BD2022_TempPsi,
                                 data = temp_df,
                                 indices = 1)
PsiTemp_df <- PsiTemp$estimates

PsiTemp_plot <- ggplot(data = PsiTemp_df, mapping = aes(x = covdata, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Temp") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

PsiTemp_plot

# Theta ~ Temp
BD2022_mods[[27]]$results$beta
ComplexFix <- "6"
BD2022.ddl.ThetaTemp <- BD2022.ddl
BD2022.ddl.ThetaTemp$Theta$fix <- NA
BD2022.ddl.ThetaTemp$Theta$fix <- ifelse(BD2022.ddl.ThetaTemp$Theta$ComplexID %in% ComplexFix,
                                     1, NA)
BD2022_TempTheta <- mark(data = BD2022.pr,
                       ddl = BD2022.ddl.ThetaTemp,
                       model.parameters = list(Psi = TempPsi,
                                               Theta = TempTheta,
                                               p = TempP))
ThetaTemp <- covariate.predictions(BD2022_TempTheta,
                                 data = temp_df,
                                 indices = 13)
ThetaTemp_df <- ThetaTemp$estimates

ThetaTemp_plot <- ggplot(data = ThetaTemp_df, mapping = aes(x = covdata, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  xlab("Temp") + 
  ylab("Prevalence")+
  theme_classic()+
  ylim(c(0,1))

ThetaTemp_plot


# p ~ Temp
BD2022_mods[[27]]$results$beta
ComplexFix <- NA #all complexes seem reasonable
BD2022.ddl.pTemp <- BD2022.ddl

BD2022_TempP <- mark(data = BD2022.pr,
                         ddl = BD2022.ddl.pTemp,
                         model.parameters = list(Psi = TempPsi,
                                                 Theta = TempTheta,
                                                 p = TempP))
pTemp <- covariate.predictions(BD2022_TempP,
                                   data = temp_df,
                                   indices = 445,
                               drop = F)
pTemp_df <- pTemp$estimates

pTemp_plot <- ggplot(data = pTemp_df, mapping = aes(x = covdata, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  xlab("Temp") + 
  ylab("Prevalence")+
  theme_classic()+
  ylim(c(0,1))

pTemp_plot

# Put temp plots together

BD2022_TempPlots <- (PsiTemp_plot + ThetaTemp_plot)/pTemp_plot
BD2022_TempPlots


# p ~ alpha

pAlpha <- covariate.predictions(model = BD2022_mods[[25]],
                                data = alpha_df,
                                indices = 445)

pAlpha_df <- pAlpha$estimates

pAlpha_df <- pAlpha_df %>% group_by(alpha) %>% summarise(estimate = mean(estimate),
                                                         lcl = mean(lcl),
                                                         ucl = mean(ucl))
pAlpha_plot <- ggplot(dat = pAlpha_df, aes(x = alpha, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  xlab("Alpha diversity") + 
  ylab("Detection")+
  theme_classic()+
  ylim(c(0,1))
pAlpha_plot

# These are all the figures I can think of for 2022, now onto 2024

### 2024 ###
View(BD2024_mods$model.table)
# top model was the null model, so not really sure what there is to plot... lets look at RV

# Ranavirus

### 2022 ###
View(RV2022_mods$model.table)

# plot the following: psi ~ temp, theta ~ temp, p ~ alpha

# psi ~ temp

RV2022_mods[[25]]$results$beta
# Psi has an SE of 0 in Complex 8 so may want to fix that to 0.

ComplexFix <- "8"
RV2022.ddl.PsiTemp <- RV2022.ddl
RV2022.ddl.PsiTemp$Psi$fix <- NA
RV2022.ddl.PsiTemp$Psi$fix <- ifelse(RV2022.ddl.PsiTemp$Psi$ComplexID %in% ComplexFix,
                                     1, NA)
RV2022_TempPsi <- mark(data = RV2022.pr,
                         ddl = RV2022.ddl.PsiTemp,
                         model.parameters = list(Psi = TempPsi,
                                                 Theta = TempTheta,
                                                 p = AlphaTempP))


RV_PsiTemp <- covariate.predictions(RV2022_TempPsi,
                                    data = temp_df,
                                    indices = 1)

RV_PsiTemp_df <- RV_PsiTemp$estimates # goes from 0 - > like immediately... that's weird

RV_PsiTemp_plot <- ggplot(data = RV_PsiTemp_df, mapping = aes(x = covdata, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  xlab("Temperature") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))
  
RV_PsiTemp_plot

# theta ~ temp
RV2022_mods[[25]]$results$beta
# Complex 7 and 6 are problems for theta. Need to fix at 0 for 6 and 1 for 7

RV2022.ddl.ThetaTemp <- RV2022.ddl
RV2022.ddl.ThetaTemp$Theta$fix <- NA
for(i in 1:nrow(RV2022.ddl.ThetaTemp$Theta)){
  RV2022.ddl.ThetaTemp$Theta[i,9] <- if(RV2022.ddl.ThetaTemp$Theta[i,8] == "6"){
  1}else if(RV2022.ddl.ThetaTemp$Theta[i,8] == "7"){1} else{NA}
}
View(RV2022.ddl.ThetaTemp$Theta)

RV2022_TempTheta <- mark(data = RV2022.pr,
                       ddl = RV2022.ddl.PsiTemp,
                       model.parameters = list(Psi = TempPsi,
                                               Theta = TempTheta,
                                               p = AlphaTempP))

RV2022_TempTheta$results$beta
RV_ThetaTemp <- covariate.predictions(RV2022_TempTheta,
                                    data = temp_df,
                                    indices = 13)

RV_ThetaTemp_df <- RV_ThetaTemp$estimates

RV_ThetaTemp_plot <- ggplot(data = RV_ThetaTemp_df, mapping = aes(x = covdata, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  xlab("Temperature") + 
  ylab("Prevalence")+
  theme_classic()+
  ylim(c(0,1))

RV_ThetaTemp_plot


# p ~ alpha
# theta ~ temp
RV2022_mods[[25]]$results$beta

RV_pAlpha <- covariate.predictions(RV2022_mods[[25]],
                                      data = alpha_df,
                                      indices = 445)

RV_pAlpha_df <- RV_pAlpha$estimates
RV_pAlpha_df <- RV_pAlpha_df %>% group_by(alpha) %>% summarize(estimate = mean(estimate),
                                                               lcl = mean(lcl),
                                                               ucl = mean(ucl))
RV_ThetaTemp_plot <- ggplot(data = RV_pAlpha_df, mapping = aes(x = alpha, y = estimate))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
  xlab("Temperature") + 
  ylab("Prevalence")+
  theme_classic()+
  ylim(c(0,1))

RV_ThetaTemp_plot

# #I'm going to use model averaging for now but this may change depending on model results
# 
# # Graphing effects on psi
# #Alpha
# psi.alpha.df.BD.2022 <- predict(BD_mods, type = "state", newdata = alpha_df, appendData = T)
# psi.alpha.BD.2022.plot <- ggplot(data = psi.alpha.df.BD.2022, mapping = aes(x = Predicted, y = alpha))+
#   geom_smooth(linewidth = 1.5, colour = "black") +
#   xlab("Alpha diversity") + 
#   ylab("Occupancy")+
#   theme_classic()+
#   ylim(c(0,1))
# 
# #Beta
# psi.beta.df.BD.2022 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
# psi.beta.BD.2022.plot <- ggplot(data = psi.beta.df.BD.2022, mapping = aes(x = Predicted, y = beta))+
#   geom_smooth(linewidth = 1.5, colour = "black") +
#   xlab("Beta diversity") + 
#   ylab("Occupancy")+
#   theme_classic()+
#   ylim(c(0,1))
# 
# # Graphing effects on prevalence

# #Alpha
# phi.alpha.df.BD.2022 <- predict(BD_mods, type = "phi", newdata = alpha_df, appendData = T)
# phi.alpha.BD.2022.plot <- ggplot(data = phi.alpha.df.BD.2022, mapping = aes(x = Predicted, y = alpha))+
#   geom_smooth(linewidth = 1.5, colour = "black") +
#   xlab("Alpha diversity") + 
#   ylab("Occupancy")+
#   theme_classic()+
#   ylim(c(0,1))
# 
# #Beta
# phi.beta.df.BD.2022 <- predict(BD_mods, type = "phi", newdata = beta_df, appendData = T)
# phi.beta.BD.2022.plot <- ggplot(data = phi.beta.df.BD.2022, mapping = aes(x = Predicted, y = beta))+
#   geom_smooth(linewidth = 1.5, colour = "black") +
#   xlab("Beta diversity") + 
#   ylab("Occupancy")+
#   theme_classic()+
#   ylim(c(0,1))
# 
# # Graphing effects on detection
# 
# #Alpha
# p.alpha.df.BD.2022 <- predict(BD_mods, type = "state", newdata = alpha_df, appendData = T)
# p.alpha.BD.2022.plot <- ggplot(data = p.alpha.df.BD.2022, mapping = aes(x = Predicted, y = alpha))+
#   geom_smooth(linewidth = 1.5, colour = "black") +
#   xlab("Alpha diversity") + 
#   ylab("Occupancy")+
#   theme_classic()+
#   ylim(c(0,1))
# 
# #Beta
# p.beta.df.BD.2022 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
# p.beta.BD.2022.plot <- ggplot(data = p.beta.df.BD.2022, mapping = aes(x = Predicted, y = beta))+
#   geom_smooth(linewidth = 1.5, colour = "black") +
#   xlab("Beta diversity") + 
#   ylab("Occupancy")+
#   theme_classic()+
#   ylim(c(0,1))
# 
# # Put them all together in a figure
# BD_2022_figa <- psi.alpha.BD.2022.plot + phi.alpha.BD.2022.plot + p.alpha.BD.2022.plot /
#   psi.beta.BD.2022.plot + phi.beta.BD.2022.plot + p.beta.BD.2022.plot + plot_annotation(tag_levels = "A")
# ggsave(BD_2022_figs, filename = "BD_2022_figs.png")

### RV ##

# Graphing effects on psi
#Alpha
psi.alpha.df.RV.2022 <- predict(RV_mods, type = "state", newdata = alpha_df, appendData = T)
psi.alpha.RV.2022.plot <- ggplot(data = psi.alpha.df.RV.2022, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
psi.beta.df.RV.2022 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
psi.beta.RV.2022.plot <- ggplot(data = psi.beta.df.RV.2022, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on prevalence

#Alpha
phi.alpha.df.RV.2022 <- predict(RV_mods, type = "phi", newdata = alpha_df, appendData = T)
phi.alpha.RV.2022.plot <- ggplot(data = phi.alpha.df.RV.2022, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
phi.beta.df.RV.2022 <- predict(RV_mods, type = "phi", newdata = beta_df, appendData = T)
phi.beta.RV.2022.plot <- ggplot(data = phi.beta.df.RV.2022, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on detection

#Alpha
p.alpha.df.RV.2022 <- predict(RV_mods, type = "state", newdata = alpha_df, appendData = T)
p.alpha.RV.2022.plot <- ggplot(data = p.alpha.df.RV.2022, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
p.beta.df.RV.2022 <- predict(RV_mods, type = "state", newdata = beta_df, appendData = T)
p.beta.RV.2022.plot <- ggplot(data = p.beta.df.RV.2022, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Put them all together in a figure
RV_2022_figs <- psi.alpha.RV.2022.plot + phi.alpha.RV.2022.plot + p.alpha.RV.2022.plot /
  psi.beta.RV.2022.plot + phi.beta.RV.2022.plot + p.beta.RV.2022.plot + plot_annotation(tag_levels = "A")
ggsave(RV_2022_figs, filename = "RV_2022_figs.png")

############
#2024######
###########

#Create new data for prediction

alpha <- 2:6 #number of possible species
beta <- seq(from = 0, to = 0.7, by = 0.1)
temp <- seq(from = -1.8, to = 1.5, by = 0.1)

alpha_df <- data.frame(alpha = rep(alpha, each = length(temp)), temp = rep(temp, times = length(alpha)))
beta_df <- data.frame(beta = rep(beta, each = length(temp)), temp = rep(temp, times = length(beta)))

### BD ###

#I'm going to use model averaging for now but this may change depending on model results

# Graphing effects on psi
#Alpha
psi.alpha.df.BD.2024 <- predict(BD_mods, type = "state", newdata = alpha_df, appendData = T)
psi.alpha.BD.2024.plot <- ggplot(data = psi.alpha.df.BD.2024, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
psi.beta.df.BD.2024 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
psi.beta.BD.2024.plot <- ggplot(data = psi.beta.df.BD.2024, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on prevalence

#Alpha
phi.alpha.df.BD.2024 <- predict(BD_mods, type = "phi", newdata = alpha_df, appendData = T)
phi.alpha.BD.2024.plot <- ggplot(data = phi.alpha.df.BD.2024, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
phi.beta.df.BD.2024 <- predict(BD_mods, type = "phi", newdata = beta_df, appendData = T)
phi.beta.BD.2024.plot <- ggplot(data = phi.beta.df.BD.2024, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on detection

#Alpha
p.alpha.df.BD.2024 <- predict(BD_mods, type = "state", newdata = alpha_df, appendData = T)
p.alpha.BD.2024.plot <- ggplot(data = p.alpha.df.BD.2024, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
p.beta.df.BD.2024 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
p.beta.BD.2024.plot <- ggplot(data = p.beta.df.BD.2024, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Put them all together in a figure
BD_2024_figs <- psi.alpha.BD.2024.plot + phi.alpha.BD.2024.plot + p.alpha.BD.2024.plot /
  psi.beta.BD.2024.plot + phi.beta.BD.2024.plot + p.beta.BD.2024.plot + plot_annotation(tag_levels = "A")
ggsave(BD_2024_figs, filename = "BD_2024_figs.png")

### RV ##

# Graphing effects on psi
#Alpha
psi.alpha.df.RV.2024 <- predict(RV_mods, type = "state", newdata = alpha_df, appendData = T)
psi.alpha.RV.2024.plot <- ggplot(data = psi.alpha.df.RV.2024, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
psi.beta.df.RV.2024 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
psi.beta.RV.2024.plot <- ggplot(data = psi.beta.df.RV.2024, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on prevalence

#Alpha
phi.alpha.df.RV.2024 <- predict(RV_mods, type = "phi", newdata = alpha_df, appendData = T)
phi.alpha.RV.2024.plot <- ggplot(data = phi.alpha.df.RV.2024, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
phi.beta.df.RV.2024 <- predict(RV_mods, type = "phi", newdata = beta_df, appendData = T)
phi.beta.RV.2024.plot <- ggplot(data = phi.beta.df.RV.2024, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on detection

#Alpha
p.alpha.df.RV.2024 <- predict(RV_mods, type = "state", newdata = alpha_df, appendData = T)
p.alpha.RV.2024.plot <- ggplot(data = p.alpha.df.RV.2024, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
p.beta.df.RV.2024 <- predict(RV_mods, type = "state", newdata = beta_df, appendData = T)
p.beta.RV.2024.plot <- ggplot(data = p.beta.df.RV.2024, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Put them all together in a figure
RV_2024_figs <- psi.alpha.RV.2024.plot + phi.alpha.RV.2024.plot + p.alpha.RV.2024.plot /
  psi.beta.RV.2024.plot + phi.beta.RV.2024.plot + p.beta.RV.2024.plot + plot_annotation(tag_levels = "A")
ggsave(RV_2024_figs, filename = "RV_2024_figs.png")


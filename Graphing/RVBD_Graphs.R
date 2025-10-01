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

BD_mods <- readRDS("BD_mods.RDS")
RV_mods <- readRDS("RV_mods.RDS")







############
#2022######
###########

#Create new data for prediction

alpha <- 1:7 #number of possible species
beta <- seq(from = 0, to = 0.5, by = 0.1)
temp <- seq(from = -1.6, to = 1.4, by = 0.1)

alpha_df <- data.frame(alpha = rep(alpha, each = length(temp)), temp = rep(temp, times = length(alpha)))
beta_df <- data.frame(beta = rep(beta, each = length(temp)), temp = rep(temp, times = length(beta)))

### BD ###

#I'm going to use model averaging for now but this may change depending on model results

# Graphing effects on psi
#Alpha
psi.alpha.df.BD.2022 <- predict(BD_mods, type = "state", newdata = alpha_df, appendData = T)
psi.alpha.BD.2022.plot <- ggplot(data = psi.alpha.df.BD.2022, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
psi.beta.df.BD.2022 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
psi.beta.BD.2022.plot <- ggplot(data = psi.beta.df.BD.2022, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on prevalence

#Alpha
phi.alpha.df.BD.2022 <- predict(BD_mods, type = "phi", newdata = alpha_df, appendData = T)
phi.alpha.BD.2022.plot <- ggplot(data = phi.alpha.df.BD.2022, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
phi.beta.df.BD.2022 <- predict(BD_mods, type = "phi", newdata = beta_df, appendData = T)
phi.beta.BD.2022.plot <- ggplot(data = phi.beta.df.BD.2022, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Graphing effects on detection

#Alpha
p.alpha.df.BD.2022 <- predict(BD_mods, type = "state", newdata = alpha_df, appendData = T)
p.alpha.BD.2022.plot <- ggplot(data = p.alpha.df.BD.2022, mapping = aes(x = Predicted, y = alpha))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Alpha diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

#Beta
p.beta.df.BD.2022 <- predict(BD_mods, type = "state", newdata = beta_df, appendData = T)
p.beta.BD.2022.plot <- ggplot(data = p.beta.df.BD.2022, mapping = aes(x = Predicted, y = beta))+
  geom_smooth(linewidth = 1.5, colour = "black") +
  xlab("Beta diversity") + 
  ylab("Occupancy")+
  theme_classic()+
  ylim(c(0,1))

# Put them all together in a figure
BD_2022_figa <- psi.alpha.BD.2022.plot + phi.alpha.BD.2022.plot + p.alpha.BD.2022.plot /
  psi.beta.BD.2022.plot + phi.beta.BD.2022.plot + p.beta.BD.2022.plot + plot_annotation(tag_levels = "A")
ggsave(BD_2022_figs, filename = "BD_2022_figs.png")

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


# Graphing for models of BD and RV
# RCS
# 10/01/2025

#load library
library(ggplot2)
library(RMark)
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

load("TempDataOccMod.RData")
range(TemData2022_OccMod[,3:5],na.rm=T)
range(TemData2024_OccMod[,3:5],na.rm=T)

#Create new data for prediction

alpha <- 1:7 #number of possible species
beta <- seq(from = 0, to = 0.5, by = 0.1)
temp <- seq(from = 63, to = 80, by = 1)

temp_df <- data.frame(temp = temp)
alpha_df <- data.frame(alpha = rep(alpha, each = length(Temp)), Temp = rep(Temp, times = length(alpha)))
beta_df <- data.frame(beta = rep(beta, each = length(Temp)), Temp = rep(Temp, times = length(beta)))




############
#2022######
###########

# I'm actually not sure what to plot from 2022 since both RV + BD we're best explained
# by the null model

############
#2024######
###########

RV2024_mods$model.table

# I do want to plot RV. Specifically mod avg of Theta ~ Temp
# and p ~ beta + temp mod avg

# Theta ~ Temp
ThetaTemp <- covariate.predictions(model = RV2024_mods[[26]],
                                   data = temp_df,
                                   indices = 2,
                                   reals = T)
ThetaTemp_df <- ThetaTemp$estimates

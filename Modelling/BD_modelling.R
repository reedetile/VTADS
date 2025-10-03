#Description-----------------------------------------
#Modelling of disease dynamics
#  18 Apr 2025
#RCS

#Initialize -----------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)

# Load functions--------------------------------------


# Global Variables------------------------------------
repo <- 'C:/Users/rcscott/VTADS'
data <- paste(repo,"/Data", sep = "")

setwd(data)
BD_2022 <- readRDS ("BD2022_occMod.RDS")# read in disease data from 2022
BD_2024 <- readRDS("BD2024_occMod.RDS")# read in disease data from 2024
load("biodiversityData.RData") # read in biodiversity data
load("TempDataOccMod.RData") 
WaterTemp_2022_Site_lvl <- TemData2022_OccMod
WaterTemp_2024_Site_lvl <- TemData2024_OccMod
load("WaterData2022.RData")
WaterTemp_2022_Ind_lvl <- WaterTemp_2022_wider
WaterTemp_2022_Ind_lvl_dup <- WaterTemp_2022_duplicate
load("WaterData2024.RData")
WaterTemp_2024_Ind_lvl <- WaterTemp_2024_wider
WaterTemp_2024_Ind_lvl_dup <- WaterTemp_2024_duplicate

#Renamed df's for clarity. Now removing redundnat DF's
rm(list = c("TemData2022_OccMod",
            "TemData2024_OccMod",
            "WaterTemp_2022_wider",
            "WaterTemp_2022_duplicate",
            "WaterTemp_2024_wider",
            "WaterTemp_2024_duplicate"))
# Program Body------------------------------------------

# First lets finish cleaning up data. Need to change NaN to NA, and make sure columns are organized

TempData_Site_2022 <- WaterTemp_2022_Site_lvl 
TempData_Site_2022$mean_temp <- ifelse(is.nan(TempData_Site_2022$mean_temp), NA, TempData_Site_2022$mean_temp)
TempData_Site_2022$scaled_temp <- scale(TempData_Site_2022$mean_temp)
TempData_Site_2022 <- TempData_Site_2022 %>% arrange(SiteID)
TempData_Site_2022 <- TempData_Site_2022 %>% select(!mean_temp)

TempData_Site_2024 <- WaterTemp_2024_Site_lvl 
TempData_Site_2024$mean_temp <- ifelse(is.nan(TempData_Site_2024$mean_temp), NA, TempData_Site_2024$mean_temp)
TempData_Site_2024$scaled_temp <- scale(TempData_Site_2024$mean_temp)
TempData_Site_2024 <- TempData_Site_2024 %>% arrange(SiteID)
TempData_Site_2024 <- TempData_Site_2024 %>% select(!mean_temp)

# Individual + rep level temperatures will need to be normalized as well

# Ind level
Rep_lvl_list <- list(WaterTemp_2022_Ind_lvl_dup = WaterTemp_2022_Ind_lvl_dup,
                     WaterTemp_2024_Ind_lvl_dup = WaterTemp_2024_Ind_lvl_dup)
for (i in 1:length(Rep_lvl_list)) {
  df <- as.data.frame(Rep_lvl_list[[i]])
  for(j in 1:ncol(df)){
    df[,j] <- as.numeric(df[,j])
  }
  df[,3:ncol(df)] <- t(scale(t(df[,3:ncol(df)])))
  Rep_lvl_list[[i]] <- df
}

WaterTemp_2022_Ind_lvl_dup <- Rep_lvl_list[[1]]
WaterTemp_2024_Ind_lvl_dup <- Rep_lvl_list[[2]]

# Rep level
Ind_lvl_list <- list(WaterTemp_2022_Ind_lvl = WaterTemp_2022_Ind_lvl,
                     WaterTemp_2024_Ind_lvl = WaterTemp_2024_Ind_lvl)
for (i in 1:length(Ind_lvl_list)) {
  df <- as.data.frame(Ind_lvl_list[[i]])
  for(j in 1:ncol(df)){
    df[,j] <- as.numeric(df[,j])
  }
  df[,3:ncol(df)] <- t(scale(t(df[,3:ncol(df)])))
  Ind_lvl_list[[i]] <- df
}

WaterTemp_2022_Ind_lvl <- Ind_lvl_list[[1]]
WaterTemp_2024_Ind_lvl <- Ind_lvl_list[[2]]
#Now lets make the lists for site level covs

SiteCovs_2022 <- list(alphaData = Diversity_2022$alpha, betaData = Diversity_2022$beta, SiteTempData = TempData_Site_2022$scaled_temp)
SiteCovs_2024 <- list(alphaData = Diversity_2024$alpha, betaData = Diversity_2024$beta, SiteTempData = TempData_Site_2024$scaled_temp)
SiteCovs_2022 <- lapply(SiteCovs_2022, as.data.frame)
SiteCovs_2024 <- lapply(SiteCovs_2024, as.data.frame)

#Want to make a phi (aka theta) list as well
numFrogs <- 12*3
Sites <- 24
phi_list_2022 <- list(alphaData = NA, betaData = NA, IndTempData = WaterTemp_2022_Ind_lvl[,3:ncol(WaterTemp_2022_Ind_lvl)])

for(i in 1:length(phi_list_2022)){
  if(i == 3){
    next
  }
  data <- rep(unlist(SiteCovs_2022[[i]][[1]]), times = numFrogs)
  data <- data.frame(matrix(data = data, nrow = Sites, ncol = numFrogs))
  phi_list_2022[[i]] <- data
}

phi_list_2024 <- list(alphaData = NA, betaData = NA, IndTempData = WaterTemp_2024_Ind_lvl[,3:ncol(WaterTemp_2024_Ind_lvl)])

for(i in 1:length(phi_list_2024)){
  if(i == 3){
    next
  }
  data <- rep(unlist(SiteCovs_2024[[i]][[1]]), times = numFrogs)
  data <- data.frame(matrix(data = data, nrow = Sites, ncol = numFrogs))
  phi_list_2024[[i]] <- data
}

# Lastly need to make obsCovs list

dets <- numFrogs * 2

obsCovsList_2022 <- list(alphaData = NA, betaData = NA, RepTempData = WaterTemp_2022_Ind_lvl_dup[,3:ncol(WaterTemp_2022_Ind_lvl_dup)])

for(i in 1:length(obsCovsList_2022)){
  if(i == 3){
    next
  }
  data <- rep(unlist(SiteCovs_2022[[i]][[1]]), times = dets)
  data <- data.frame(matrix(data = data,nrow = Sites, ncol = dets))
  obsCovsList_2022[[i]] <- data
}

obsCovsList_2024 <- list(alphaData = NA, betaData = NA, RepTempData = WaterTemp_2024_Ind_lvl_dup[,3:ncol(WaterTemp_2024_Ind_lvl_dup)])

for(i in 1:length(obsCovsList_2024)){
  if(i == 3){
    next
  }
  data <- rep(unlist(SiteCovs_2024[[i]][[1]]), times = dets)
  data <- data.frame(matrix(data = data,nrow = Sites, ncol = dets))
  obsCovsList_2024[[i]] <- data
}

# Note on naming models = There are 3 parameters, psi, theta, and p
# Each parameter could be either constant, affected by alpha diversity, or effected by beta diversity
# 3 parameters * 3 possible covariates = 27 possible models
# Each model was named based on the parameter ~ covariate inputs.
# For example, the null model is named NullNullNull because all there parameters are constant. the 2nd model is named
# NullNullAlpha because it includes an effect of alpha on detection/intensity. And so on.

#creating UMFs

SiteCovs_2022_df <- do.call(cbind, SiteCovs_2022)
colnames(SiteCovs_2022_df) <- c("alpha", "beta","SiteTemp")
umf_2022 <- unmarkedMultFrame(y = BD_2022[,3:ncol(BD_2022)], 
                              siteCovs = SiteCovs_2022_df, 
                              obsCovs = obsCovsList_2022, 
                              yearlySiteCovs = phi_list_2022, 
                              numPrimary = numFrogs) 

SiteCovs_2024_df <- do.call(cbind, SiteCovs_2024)
colnames(SiteCovs_2024_df) <- c("alpha", "beta","SiteTemp")
umf_2024 <- unmarkedMultFrame(y = BD_2024[,3:ncol(BD_2024)], 
                              siteCovs = SiteCovs_2024_df, 
                              obsCovs = obsCovsList_2024, 
                              yearlySiteCovs = phi_list_2024, 
                              numPrimary = numFrogs)
##########
###2022###
##########

# If temp included
# Psi ~ Temp
NullNullNull <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = umf_2022 ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = umf_2022 )
NullNullBeta <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~beta + RepTemp,data = umf_2022 )
NullAlphaNull <- goccu(psiformula = ~SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~RepTemp,data = umf_2022 )
NullAlphaAlpha <- goccu(psiformula = ~SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~alpha + RepTemp,data = umf_2022 )
NullAlphaBeta <- goccu(psiformula = ~SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~beta + RepTemp,data = umf_2022 )
NullBetaNull <- goccu(psiformula = ~SiteTemp, phiformula = ~beta+Ind Temp, pformula = ~RepTemp,data = umf_2022 ) 
NullBetaAlpha <- goccu(psiformula = ~SiteTemp, phiformula = ~beta + IndTemp, pformula = ~alpha + RepTemp,data = umf_2022 )
NullBetaBeta <- goccu(psiformula = ~SiteTemp, phiformula = ~beta + IndTemp, pformula = ~beta + RepTemp,data = umf_2022 )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = umf_2022 )
AlphaNullAlpha <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = umf_2022 )
AlphaNullBeta <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~IndTemp, pformula = ~beta + RepTemp,data = umf_2022 )
AlphaAlphaNull <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~RepTemp,data = umf_2022 )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~alpha + RepTemp,data = umf_2022 )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~beta + RepTemp,data = umf_2022 )
AlphaBetaNull <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~RepTemp,data = umf_2022 )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~alpha + RepTemp,data = umf_2022 )
AlphaBetaBeta <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~beta + RepTemp,data = umf_2022 )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = umf_2022 )
BetaNullAlpha <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = umf_2022 )
BetaNullBeta <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~IndTemp, pformula = ~beta + RepTemp,data = umf_2022 )
BetaAlphaNull <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~RepTemp,data = umf_2022 )
BetaAlphaAlpha <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~alpha + RepTemp,data = umf_2022 )
BetaAlphaBeta <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~beta + RepTemp,data = umf_2022 )
BetaBetaNull <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~RepTemp,data = umf_2022 )
BetaBetaAlpha <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~alpha + RepTemp,data = umf_2022 )
BetaBetaBeta <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~beta + RepTemp,data = umf_2022 )

# Do model selection
fitlist.psi <- fitList(NullNullNull,
                       NullNullAlpha,
                       NullNullBeta,
                       NullAlphaNull,
                       NullAlphaAlpha,
                       NullAlphaBeta,
                       NullBetaNull,
                       NullBetaAlpha,
                       NullBetaBeta,
                       AlphaNullNull,
                       AlphaNullAlpha,
                       AlphaNullBeta,
                       AlphaAlphaNull,
                       AlphaAlphaAlpha,
                       AlphaAlphaBeta,
                       AlphaBetaNull,
                       AlphaBetaAlpha,
                       AlphaBetaBeta,
                       BetaNullNull,
                       BetaNullAlpha,
                       BetaNullBeta,
                       BetaAlphaNull,
                       BetaAlphaAlpha,
                       BetaAlphaBeta,
                       BetaBetaNull,
                       BetaBetaAlpha,
                       BetaBetaBeta)

modSel(fitlist.psi)


##########
###2024###
##########

# If temp included
# Psi ~ Temp
NullNullNull <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = umf_2024 ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
NullNullBeta <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
NullAlphaNull <- goccu(psiformula = ~SiteTemp, phiformula = ~alpha +IndTemp, pformula = ~RepTemp,data = umf_2024 )
NullAlphaAlpha <- goccu(psiformula = ~SiteTemp, phiformula = ~alpha +IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
NullAlphaBeta <- goccu(psiformula = ~SiteTemp, phiformula = ~alpha +IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
NullBetaNull <- goccu(psiformula = ~SiteTemp, phiformula = ~beta + IndTemp, pformula = ~RepTemp,data = umf_2024 ) 
NullBetaAlpha <- goccu(psiformula = ~SiteTemp, phiformula = ~beta + IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
NullBetaBeta <- goccu(psiformula = ~SiteTemp, phiformula = ~beta + IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = umf_2024 )
AlphaNullAlpha <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
AlphaNullBeta <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
AlphaAlphaNull <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~alpha +IndTemp, pformula = ~RepTemp,data = umf_2024 )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~alpha +IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~alpha +IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
AlphaBetaNull <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~RepTemp,data = umf_2024 )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
AlphaBetaBeta <- goccu(psiformula = ~alpha + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = umf_2024 )
BetaNullAlpha <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
BetaNullBeta <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )
BetaAlphaNull <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~RepTemp,data = umf_2024 )
BetaAlphaAlpha <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~alpha + RepTemp,data = umf_2024 )
BetaAlphaBeta <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~alpha +Ind Temp, pformula = ~beta + RepTemp,data = umf_2024 )
BetaBetaNull <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~RepTemp,data = umf_2024 )
BetaBetaAlpha <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~alpha + RepTemp,data = umf_2024 )
BetaBetaBeta <- goccu(psiformula = ~beta + SiteTemp, phiformula = ~beta + IndTemp, pformula = ~beta + RepTemp,data = umf_2024 )

# Do model selection
fitlist.BD <- fitList(NullNullNull,
                       NullNullAlpha,
                       NullNullBeta,
                       NullAlphaNull,
                       NullAlphaAlpha,
                       NullAlphaBeta,
                       NullBetaNull,
                       NullBetaAlpha,
                       NullBetaBeta,
                       AlphaNullNull,
                       AlphaNullAlpha,
                       AlphaNullBeta,
                       AlphaAlphaNull,
                       AlphaAlphaAlpha,
                       AlphaAlphaBeta,
                       AlphaBetaNull,
                       AlphaBetaAlpha,
                       AlphaBetaBeta,
                       BetaNullNull,
                       BetaNullAlpha,
                       BetaNullBeta,
                       BetaAlphaNull,
                       BetaAlphaAlpha,
                       BetaAlphaBeta,
                       BetaBetaNull,
                       BetaBetaAlpha,
                       BetaBetaBeta)

modSel(fitlist.BD)

setwd(data)
saveRDS(fitList.BD, file = "BD_mods")

# # If only want to explore Alpha*NumPonds, with temperature
# NullNullNull <- goccu(psiformula = ~SiteTemp, phiformula = ~IndTemp, pformula = ~RepTemp,data = ) # Really a temp model
# 
# # Effects of alpha + temp ONLY
# AlphaNullNull <- goccu(psiformula = ~ alpha + Temp, phiformula = ~IndTemp, pformula = ~RepTemp,data = )
# NullAlphaNull <- goccu(psiformula = ~ Temp, phiformula = ~alpha +Ind Temp, pformula = ~RepTemp,data = )
# NullNullAlpha <- goccu(psiformula = ~ Temp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = )
# AlphaAlphaNull <- goccu(psiformula = ~ alpha + Temp, phiformula = ~alpha +Ind Temp, pformula = ~RepTemp,data = )
# NullAlphaAlpha <- goccu(psiformula = ~ Temp, phiformula = ~alpha +Ind Temp, pformula = ~alpha + RepTemp,data = )
# AlphaNullAlpha <- goccu(psiformula = ~ alpha + Temp, phiformula = ~IndTemp, pformula = ~alpha + RepTemp,data = )
# AlphaAlphaAlpha <- goccu(psiformula = ~ alpha + Temp, phiformula = ~alpha +Ind Temp, pformula = ~alpha + RepTemp,data = )
# 
# # Effects of alpha + NumPonds + Temp
# AlphaPondsNullNull <- goccu(psiformula = ~ alpha + Ponds + Temp, phiformula = ~IndTemp, pformula = ~RepTemp,data = )
# 





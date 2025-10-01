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
RV <- # read in disease data
load("biodiversityData.RData") # read in biodiversity data
load("TempDataOccMod.RData") 

# Program Body------------------------------------------

# First lets finish cleaning up data. Need to change NaN to NA, and make sure columns are organized

TempData2022 <- TemData2022_OccMod 
TempData2022$mean_temp <- ifelse(is.nan(TempData2022$mean_temp), NA, TempData2022$mean_temp)
TempData2022$scaled_temp <- scale(TempData2022$mean_temp)
TempData2022 <- TempData2022 %>% arrange(SiteID)
TempData2022 <- TempData2022 %>% select(!mean_temp)

TempData2024 <- TemData2024_OccMod 
TempData2024$mean_temp <- ifelse(is.nan(TempData2024$mean_temp), NA, TempData2024$mean_temp)
TempData2024$scaled_temp <- scale(TempData2024$mean_temp)
TempData2024 <- TempData2024 %>% arrange(SiteID)
TempData2024 <- TempData2024 %>% select(!mean_temp)


#Now lets make the lists for site level covs

SiteCovs_2022 <- list(alphaData = Diversity_2022$alpha, betaData = Diversity_2022$beta, TempData = TempData2022$scaled_temp)
SiteCovs_2024 <- list(alphaData = Diversity_2024$alpha, betaData = Diversity_2024$beta, TempData = TempData2024$scaled_temp)
SiteCovs_2022 <- lapply(SiteCovs_2022, as.data.frame)
SiteCovs_2024 <- lapply(SiteCovs_2024, as.data.frame)

#Want to make a phi (aka theta) list as well
numFrogs <- 12*3
Sites <- 24
phi_list_2022 <- list(alphaData = NA, betaData = NA, TempData = NA)

for(i in 1:length(phi_list_2022)){
  data <- rep(unlist(SiteCovs_2022[[i]][[1]]), times = numFrogs)
  data <- data.frame(matrix(data = data, nrow = Sites, ncol = numFrogs))
  phi_list_2022[[i]] <- data
}

phi_list_2024 <- list(alphaData = NA, betaData = NA, TempData = NA)

for(i in 1:length(phi_list_2024)){
  data <- rep(unlist(SiteCovs_2024[[i]][[1]]), times = numFrogs)
  data <- data.frame(matrix(data = data, nrow = Sites, ncol = numFrogs))
  phi_list_2024[[i]] <- data
}

# Lastly need to make obsCovs list

dets <- numFrogs * 2

obsCovsList_2022 <- list(alphaData = NA, betaData = NA, TempData = NA)

for(i in 1:length(obsCovsList_2022)){
  data <- rep(unlist(SiteCovs_2022[[i]][[1]]), times = dets)
  data <- data.frame(matrix(data = data,nrow = Sites, ncol = dets))
  obsCovsList_2022[[i]] <- data
}

obsCovsList_2024 <- list(alphaData = NA, betaData = NA, TempData = NA)

for(i in 1:length(obsCovsList_2024)){
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
colnames(SiteCovs_2022_df) <- c("alpha", "beta","Temp")
umf_2022 <- unmarkedMultFrame(y = RV_2022, 
                              siteCovs = SiteCovs_2022_df, 
                              obsCovs = obsCovsList_2022, 
                              yearlySiteCovs = phi_list_2022, 
                              numPrimary = numFrogs) 

SiteCovs_2024_df <- do.call(cbind, SiteCovs_2024)
colnames(SiteCovs_2024_df) <- c("alpha", "beta","Temp")
umf_2024 <- unmarkedMultFrame(y = RV_2024, 
                              siteCovs = SiteCovs_2024_df, 
                              obsCovs = obsCovsList_2024, 
                              yearlySiteCovs = phi_list_2024, 
                              numPrimary = numFrogs)
##########
###2022###
##########

# If temp included
# Psi ~ Temp
NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = umf_2022 ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = umf_2022 )
NullNullBeta <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~beta+ Temp,data = umf_2022 )
NullAlphaNull <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = umf_2022 )
NullAlphaAlpha <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = umf_2022 )
NullAlphaBeta <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~beta+ Temp,data = umf_2022 )
NullBetaNull <- goccu(psiformula = ~Temp, phiformula = ~beta+ Temp, pformula = ~Temp,data = umf_2022 ) 
NullBetaAlpha <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = umf_2022 )
NullBetaBeta <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = umf_2022 )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = umf_2022 )
AlphaNullAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = umf_2022 )
AlphaNullBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = umf_2022 )
AlphaAlphaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = umf_2022 )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = umf_2022 )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = umf_2022 )
AlphaBetaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = umf_2022 )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = umf_2022 )
AlphaBetaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = umf_2022 )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~Temp,data = umf_2022 )
BetaNullAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = umf_2022 )
BetaNullBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = umf_2022 )
BetaAlphaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = umf_2022 )
BetaAlphaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = umf_2022 )
BetaAlphaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = umf_2022 )
BetaBetaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = umf_2022 )
BetaBetaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = umf_2022 )
BetaBetaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = umf_2022 )

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
NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = umf_2024 ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = umf_2024 )
NullNullBeta <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~beta+ Temp,data = umf_2024 )
NullAlphaNull <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = umf_2024 )
NullAlphaAlpha <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = umf_2024 )
NullAlphaBeta <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~beta+ Temp,data = umf_2024 )
NullBetaNull <- goccu(psiformula = ~Temp, phiformula = ~beta+ Temp, pformula = ~Temp,data = umf_2024 ) 
NullBetaAlpha <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = umf_2024 )
NullBetaBeta <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = umf_2024 )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = umf_2024 )
AlphaNullAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = umf_2024 )
AlphaNullBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = umf_2024 )
AlphaAlphaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = umf_2024 )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = umf_2024 )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = umf_2024 )
AlphaBetaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = umf_2024 )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = umf_2024 )
AlphaBetaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = umf_2024 )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~Temp,data = umf_2024 )
BetaNullAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = umf_2024 )
BetaNullBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = umf_2024 )
BetaAlphaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = umf_2024 )
BetaAlphaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = umf_2024 )
BetaAlphaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = umf_2024 )
BetaBetaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = umf_2024 )
BetaBetaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = umf_2024 )
BetaBetaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = umf_2024 )

# Do model selection
fitlist.RV <- fitList(NullNullNull,
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

modSel(fitlist.RV)

setwd(data)
saveRDS(fitList.RV, file = "RV_mods")

# # If only want to explore Alpha*NumPonds, with temperature
# NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = ) # Really a temp model
# 
# # Effects of alpha + temp ONLY
# AlphaNullNull <- goccu(psiformula = ~ alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
# NullAlphaNull <- goccu(psiformula = ~ Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
# NullNullAlpha <- goccu(psiformula = ~ Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
# AlphaAlphaNull <- goccu(psiformula = ~ alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
# NullAlphaAlpha <- goccu(psiformula = ~ Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
# AlphaNullAlpha <- goccu(psiformula = ~ alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
# AlphaAlphaAlpha <- goccu(psiformula = ~ alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
# 
# # Effects of alpha + NumPonds + Temp
# AlphaPondsNullNull <- goccu(psiformula = ~ alpha + Ponds + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
# 





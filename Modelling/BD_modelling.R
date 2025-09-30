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
BD <- # read in disease data
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
umf_2022 <- unmarkedMultFrame(y = BD_2022, 
                              siteCovs = SiteCovs_2022_df, 
                              obsCovs = obsCovsList_2022, 
                              yearlySiteCovs = phi_list_2022, 
                              numPrimary = numFrogs) 

SiteCovs_2024_df <- do.call(cbind, SiteCovs_2024)
colnames(SiteCovs_2024_df) <- c("alpha", "beta","Temp")
umf_2024 <- unmarkedMultFrame(y = BD_2024, 
                              siteCovs = SiteCovs_2024_df, 
                              obsCovs = obsCovsList_2024, 
                              yearlySiteCovs = phi_list_2024, 
                              numPrimary = numFrogs)
##########
###2022###
##########

# If temp included
# Psi ~ Temp
NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
NullNullBeta <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~beta+ Temp,data = )
NullAlphaNull <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
NullAlphaAlpha <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
NullAlphaBeta <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~beta+ Temp,data = )
NullBetaNull <- goccu(psiformula = ~Temp, phiformula = ~beta+ Temp, pformula = ~Temp,data = ) 
NullBetaAlpha <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
NullBetaBeta <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
AlphaNullAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
AlphaNullBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = )
AlphaAlphaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = )
AlphaBetaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
AlphaBetaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
BetaNullAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
BetaNullBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = )
BetaAlphaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
BetaAlphaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
BetaAlphaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = )
BetaBetaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = )
BetaBetaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
BetaBetaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )

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
NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
NullNullBeta <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~beta+ Temp,data = )
NullAlphaNull <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
NullAlphaAlpha <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
NullAlphaBeta <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~beta+ Temp,data = )
NullBetaNull <- goccu(psiformula = ~Temp, phiformula = ~beta+ Temp, pformula = ~Temp,data = ) 
NullBetaAlpha <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
NullBetaBeta <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
AlphaNullAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
AlphaNullBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = )
AlphaAlphaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = )
AlphaBetaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
AlphaBetaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
BetaNullAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
BetaNullBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = )
BetaAlphaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
BetaAlphaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
BetaAlphaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = )
BetaBetaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = )
BetaBetaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
BetaBetaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )

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





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

# Constants
Surveys <- 3
numFrogs <- 12*Surveys
dets <- numFrogs*2

# Global Variables------------------------------------
repo <- 'C:/Users/rcscott/VTADS'
data <- paste(repo,"/Data", sep = "")

setwd(data)
load("occModdata_2022.RData")# read in disease data from 2022
load("occModdata_2024.RData")# read in disease data from 2024
load("biodiversityData.RData") # read in biodiversity data
load("TempDataOccMod.RData") # Hobo logger data
WaterTemp_2022_Survey_lvl <- TemData2022_OccMod
WaterTemp_2024_Survey_lvl <- TemData2024_OccMod
load("WaterData2022.RData") # water temp nearest frog
WaterTemp_2022_Ind_lvl <- WaterTemp_2022_wider
WaterTemp_2022_Ind_lvl_dup <- WaterTemp_2022_duplicate
load("WaterData2024.RData") # water temp nearest frog
WaterTemp_2024_Ind_lvl <- WaterTemp_2024_wider
WaterTemp_2024_Ind_lvl_dup <- WaterTemp_2024_duplicate

#Renamed df's for clarity. Now removing redundant DF's, including RV df's
rm(list = c("TemData2022_OccMod",
            "TemData2024_OccMod",
            "WaterTemp_2022_wider",
            "WaterTemp_2022_duplicate",
            "WaterTemp_2024_wider",
            "WaterTemp_2024_duplicate",
            "BD_2022_wide",
            "BD_2024_wide"))
# Program Body------------------------------------------
#Now lets make the lists for site level covs

SiteCovs_2022 <- list(alphaData = Diversity_2022$alpha, betaData = Diversity_2022$beta)
SiteCovs_2024 <- list(alphaData = Diversity_2024$alpha, betaData = Diversity_2024$beta)
SiteCovs_2022 <- lapply(SiteCovs_2022, as.data.frame)
SiteCovs_2024 <- lapply(SiteCovs_2024, as.data.frame)

# Need to repeat survey temp for each frog in each survey

#2022
#impute data
WaterTemp_2022_Survey_lvl$temp1 <- rowMeans(WaterTemp_2022_Survey_lvl[,3:5],na.rm = T)
WaterTemp_2022_Survey_lvl[is.na(WaterTemp_2022_Survey_lvl$temp2),4] <- mean(WaterTemp_2022_Survey_lvl[,4],na.rm = T)
WaterTemp_2022_Survey_lvl[is.na(WaterTemp_2022_Survey_lvl$temp3),5] <- mean(WaterTemp_2022_Survey_lvl[,5],na.rm = T)

WaterTemp_2022_Survey_lvl[is.nan(WaterTemp_2022_Survey_lvl$temp1),3] <- mean(WaterTemp_2022_Survey_lvl[,3],na.rm = T)

# reshape to read into mark
temp1 <- matrix(data = rep(WaterTemp_2022_Survey_lvl$temp1, each = numFrogs/Surveys),
       nrow = nrow(WaterTemp_2022_Survey_lvl),ncol = numFrogs/Surveys,
       byrow = T)

temp2 <- matrix(data = rep(WaterTemp_2022_Survey_lvl$temp2, each = numFrogs/Surveys),
                nrow = nrow(WaterTemp_2022_Survey_lvl),ncol = numFrogs/Surveys,
                byrow = T)

temp3 <- matrix(data = rep(WaterTemp_2022_Survey_lvl$temp3, each = numFrogs/Surveys),
                nrow = nrow(WaterTemp_2022_Survey_lvl),ncol = numFrogs/Surveys,
                byrow = T)
colnames(temp1) <- 1:ncol(temp1)
colnames(temp2) <- 1:ncol(temp2)
colnames(temp3) <- 1:ncol(temp3)
for (i in 1:ncol(temp1)) {
  colnames(temp1)[[i]] <- paste("temp",i,sep = "")
  colnames(temp2)[[i]] <- paste("temp",12+i,sep = "")
  colnames(temp3)[[i]] <- paste("temp",24+i,sep = "")
}

# put all the temp data together
temp_data_2022 <- cbind(temp1,temp2,temp3) 


#2024
#impute data
WaterTemp_2024_Survey_lvl$temp1 <- rowMeans(WaterTemp_2024_Survey_lvl[,3:5],na.rm = T)
WaterTemp_2024_Survey_lvl[is.na(WaterTemp_2024_Survey_lvl$temp2),4] <- mean(WaterTemp_2024_Survey_lvl[,4],na.rm = T)
WaterTemp_2024_Survey_lvl[is.na(WaterTemp_2024_Survey_lvl$temp3),5] <- mean(WaterTemp_2024_Survey_lvl[,5],na.rm = T)

WaterTemp_2024_Survey_lvl[is.nan(WaterTemp_2024_Survey_lvl$temp1),3] <- mean(WaterTemp_2024_Survey_lvl[,3],na.rm = T)

# reshape to read into mark
temp1 <- matrix(data = rep(WaterTemp_2024_Survey_lvl$temp1, each = numFrogs/Surveys),
                nrow = nrow(WaterTemp_2024_Survey_lvl),ncol = numFrogs/Surveys,
                byrow = T)

temp2 <- matrix(data = rep(WaterTemp_2024_Survey_lvl$temp2, each = numFrogs/Surveys),
                nrow = nrow(WaterTemp_2024_Survey_lvl),ncol = numFrogs/Surveys,
                byrow = T)

temp3 <- matrix(data = rep(WaterTemp_2024_Survey_lvl$temp3, each = numFrogs/Surveys),
                nrow = nrow(WaterTemp_2024_Survey_lvl),ncol = numFrogs/Surveys,
                byrow = T)
colnames(temp1) <- 1:ncol(temp1)
colnames(temp2) <- 1:ncol(temp2)
colnames(temp3) <- 1:ncol(temp3)
for (i in 1:ncol(temp1)) {
  colnames(temp1)[[i]] <- paste("temp",i,sep = "")
  colnames(temp2)[[i]] <- paste("temp",i+12,sep = "")
  colnames(temp3)[[i]] <- paste("temp",i+24,sep = "")
}

# put all the temp data together
temp_data_2024 <- cbind(temp1,temp2,temp3) 
# # First lets finish cleaning up data. Need to change NaN to NA, and make sure columns are organized
# 
# TempData_Site_2022 <- WaterTemp_2022_Site_lvl 
# TempData_Site_2022$mean_temp <- ifelse(is.nan(TempData_Site_2022$mean_temp), NA, TempData_Site_2022$mean_temp)
# TempData_Site_2022$scaled_temp <- scale(TempData_Site_2022$mean_temp)
# TempData_Site_2022 <- TempData_Site_2022 %>% arrange(SiteID)
# TempData_Site_2022 <- TempData_Site_2022 %>% select(!mean_temp)
# 
# TempData_Site_2024 <- WaterTemp_2024_Site_lvl 
# TempData_Site_2024$mean_temp <- ifelse(is.nan(TempData_Site_2024$mean_temp), NA, TempData_Site_2024$mean_temp)
# TempData_Site_2024$scaled_temp <- scale(TempData_Site_2024$mean_temp)
# TempData_Site_2024 <- TempData_Site_2024 %>% arrange(SiteID)
# TempData_Site_2024 <- TempData_Site_2024 %>% select(!mean_temp)
# 
# # Individual + rep level temperatures will need to be normalized as well
# 
# # Rep level
# Rep_lvl_list <- list(WaterTemp_2022_Ind_lvl_dup = WaterTemp_2022_Ind_lvl_dup,
#                      WaterTemp_2024_Ind_lvl_dup = WaterTemp_2024_Ind_lvl_dup)
# for (i in 1:length(Rep_lvl_list)) {
#   df <- as.data.frame(Rep_lvl_list[[i]])
#   for(j in 1:ncol(df)){
#     df[,j] <- as.numeric(df[,j])
#   }
#   df[,4:ncol(df)] <- t(scale(t(df[,4:ncol(df)])))
#   Rep_lvl_list[[i]] <- df
# }
# 
# WaterTemp_2022_Ind_lvl_dup <- Rep_lvl_list[[1]]
# WaterTemp_2024_Ind_lvl_dup <- Rep_lvl_list[[2]]
# 
# # Ind level
# Ind_lvl_list <- list(WaterTemp_2022_Ind_lvl = WaterTemp_2022_Ind_lvl,
#                      WaterTemp_2024_Ind_lvl = WaterTemp_2024_Ind_lvl)
# for (i in 1:length(Ind_lvl_list)) {
#   df <- as.data.frame(Ind_lvl_list[[i]])
#   for(j in 1:ncol(df)){
#     df[,j] <- as.numeric(df[,j])
#   }
#   df[,4:ncol(df)] <- t(scale(t(df[,4:ncol(df)])))
#   Ind_lvl_list[[i]] <- df
# }
# 
# WaterTemp_2022_Ind_lvl <- Ind_lvl_list[[1]]
# WaterTemp_2024_Ind_lvl <- Ind_lvl_list[[2]]
# 
# 
# # #Want to make a phi (aka theta) list as well
# numFrogs <- 12*3
# # Sites <- 24
# # HoboTemp_phi <- data.frame(matrix(data = rep(SiteCovs_2022$SiteTempData$V1, 
# #                                              each = numFrogs),
# #                                   nrow = nrow(SiteCovs_2022$SiteTempData),
# #                                   ncol = ncol(WaterTemp_2022_Ind_lvl)-3,
# #                                   byrow = T))
# # phi_list_2022 <- list(alphaData = NA, 
# #                       betaData = NA, 
# #                       HoboTemp = HoboTemp_phi,
# #                       IndTempData = WaterTemp_2022_Ind_lvl[,4:ncol(WaterTemp_2022_Ind_lvl)])
# # 
# # for(i in 1:length(phi_list_2022)){
# #   if(i >= 3){
# #     next
# #   }
# #   data <- rep(unlist(SiteCovs_2022[[i]][[1]]), times = numFrogs)
# #   data <- data.frame(matrix(data = data, nrow = Sites, ncol = numFrogs))
# #   phi_list_2022[[i]] <- data
# # }
# # 
# # HoboTemp_phi <- data.frame(matrix(data = rep(SiteCovs_2024$SiteTempData$V1, 
# #                                              each = numFrogs),
# #                                   nrow = nrow(SiteCovs_2024$SiteTempData),
# #                                   ncol = ncol(WaterTemp_2024_Ind_lvl)-3,
# #                                   byrow = T))
# # phi_list_2024 <- list(alphaData = NA, 
# #                       betaData = NA, 
# #                       HoboTemp = HoboTemp_phi,
# #                       IndTempData = WaterTemp_2024_Ind_lvl[,4:ncol(WaterTemp_2024_Ind_lvl)])
# # 
# # for(i in 1:length(phi_list_2024)){
# #   if(i >= 3){
# #     next
# #   }
# #   data <- rep(unlist(SiteCovs_2024[[i]][[1]]), times = numFrogs)
# #   data <- data.frame(matrix(data = data, nrow = Sites, ncol = numFrogs))
# #   phi_list_2024[[i]] <- data
# # }
# # 
# # # Lastly need to make obsCovs list
# # 
# dets <- numFrogs * 2
# # HoboTemp_dets <- data.frame(matrix(data = rep(SiteCovs_2022$SiteTempData$V1, 
# #                                               each = dets),
# #                                    nrow = nrow(SiteCovs_2022$SiteTempData),
# #                                    ncol = ncol(WaterTemp_2022_Ind_lvl_dup)-3,
# #                                    byrow = T))
# # obsCovsList_2022 <- list(alphaData = NA, 
# #                          betaData = NA, 
# #                          RepTempData = WaterTemp_2022_Ind_lvl_dup[,4:ncol(WaterTemp_2022_Ind_lvl_dup)],
# #                          HoboTemp = HoboTemp_dets)
# # 
# # for(i in 1:length(obsCovsList_2022)){
# #   if(i >= 3){
# #     next
# #   }
# #   data <- rep(unlist(SiteCovs_2022[[i]][[1]]), times = dets)
# #   data <- data.frame(matrix(data = data,nrow = Sites, ncol = dets))
# #   obsCovsList_2022[[i]] <- data
# # }
# # 
# # 
# # HoboTemp_dets <- data.frame(matrix(data = rep(SiteCovs_2024$SiteTempData$V1, 
# #                                               each = dets),
# #                                    nrow = nrow(SiteCovs_2024$SiteTempData),
# #                                    ncol = ncol(WaterTemp_2024_Ind_lvl_dup)-3,
# #                                    byrow = T))
# # 
# # obsCovsList_2024 <- list(alphaData = NA, 
# #                          betaData = NA, 
# #                          RepTempData = WaterTemp_2024_Ind_lvl_dup[,4:ncol(WaterTemp_2024_Ind_lvl_dup)],
# #                          HoboTemp = HoboTemp_dets)
# # 
# # for(i in 1:length(obsCovsList_2024)){
# #   if(i >= 3){
# #     next
# #   }
# #   data <- rep(unlist(SiteCovs_2024[[i]][[1]]), times = dets)
# #   data <- data.frame(matrix(data = data,nrow = Sites, ncol = dets))
# #   obsCovsList_2024[[i]] <- data
# # }
# 
# # Note on naming models = There are 3 parameters, psi, theta, and p
# # Each parameter could be either constant, affected by alpha diversity, or effected by beta diversity
# # 3 parameters * 3 possible covariates = 27 possible models
# # Each model was named based on the parameter ~ covariate inputs.
# # For example, the null model is named NullNullNull because all there parameters are constant. the 2nd model is named
# # NullNullAlpha because it includes an effect of alpha on detection/intensity. And so on.
# 
# 
# 
# 
# 
# 

# I might have to use Mark.... 
# unmarked can't handle this many detections for goccu
library(RMark)

### 2022

# will need to make one dataframe.
SiteCovs_2022_df <- do.call(cbind, SiteCovs_2022)
colnames(SiteCovs_2022_df) <- c("alpha","beta")
SiteCovs_2024_df <- do.call(cbind, SiteCovs_2024)
colnames(SiteCovs_2024_df) <- c("alpha","beta")

RV_2022_wide[is.na(RV_2022_wide)] <- '.' # Change NA to .
ch_2022 <- RV_2022_wide %>% unite('ch', 3:ncol(RV_2022_wide),sep = "")

RV_2022_Mark <- cbind(SiteCovs_2022_df,ch_2022)

# Add survey data
nsurveys <- 3
Yes <- rep(1,dets*nrow(RV_2022_Mark)/nsurveys)
No <- rep(0,dets*nrow(RV_2022_Mark)/nsurveys)
pSurvey1 <- matrix(data = c(Yes,No,No), nrow = nrow(RV_2022_Mark), 
                  ncol =  dets)
colnames(Survey1) <- 1:dets
pSurvey2 <- matrix(data = c(No,Yes,No), nrow = nrow(RV_2022_Mark), 
                  ncol =  dets)
colnames(Survey2) <- 1:dets
Survey3 <- matrix(data = c(No,No,Yes), nrow = nrow(RV_2022_Mark), 
                  ncol =  dets)
colnames(Survey3) <- 1:dets

columns <- 1:ncol(Survey1)
for(i in 1:length(columns)){
  colnames(Survey1)[[i]] <- paste("pSurvey1",columns[[i]],sep="_")
  colnames(Survey2)[[i]] <- paste("pSurvey2",columns[[i]],sep="_")
  colnames(Survey3)[[i]] <- paste("pSurvey3",columns[[i]],sep="_")
}

RV_2022_Mark <- cbind(RV_2022_Mark,Survey1,Survey2,Survey3)
RV_2022_Mark$ComplexID <- as.factor(RV_2022_Mark$ComplexID)
#create processed data
RV2022.pr <- process.data(RV_2022_Mark,
                      model = 'MultScalOcc',
                      mixtures = 2,
                      groups = "ComplexID")

RV2022.ddl<- make.design.data(RV2022.pr)

# Add temp to ddl
tempTheta <- as.matrix(temp_data_2022)

temp1 <- tempTheta[,1:6]
temp2 <-  tempTheta[,13:18]
temp3 <- tempTheta[,25:30]
tempTheta <- cbind(temp1,temp2,temp3)
tempTheta <- c(t(tempTheta))
tempTheta
length(tempTheta)


tempP <- c(t(as.matrix(temp_data_2022)))
nrow(RV2022.ddl$Theta)
RV2022.ddl$Theta$tempTheta <- tempTheta
RV2022.ddl$p$tempP <- tempP

# add survey to ddl
Survey1 <- as.data.frame(Survey1)
Survey1Theta <- Survey1 %>% select(c(pSurvey1_1:pSurvey1_6,
                                     pSurvey1_25:pSurvey1_30,
                                     pSurvey1_61:pSurvey1_66))
Survey2 <- as.data.frame(Survey2)
Survey2Theta <- Survey2 %>% select(c(pSurvey2_1:pSurvey2_6,
                                     pSurvey2_25:pSurvey2_30,
                                     pSurvey2_61:pSurvey2_66))
Survey3 <- as.data.frame(Survey3)
Survey3Theta <- Survey3 %>% select(c(pSurvey3_1:pSurvey3_6,
                                     pSurvey3_25:pSurvey3_30,
                                     pSurvey3_61:pSurvey3_66))
Survey1Theta <- c(t(Survey1Theta))
Survey2Theta <- c(t(Survey2Theta))
Survey3Theta <- c(t(Survey3Theta))
RV2022.ddl$Theta$Survey1Theta <- Survey1Theta
RV2022.ddl$Theta$Survey2Theta <- Survey2Theta
RV2022.ddl$Theta$Survey3Theta <- Survey3Theta

pSurvey1 <- Survey1 %>% select(c(pSurvey1_1:pSurvey1_12,
                                 pSurvey1_25:pSurvey1_36,
                                 pSurvey1_61:pSurvey1_72))
pSurvey2 <- Survey2 %>% select(c(pSurvey2_1:pSurvey2_12,
                                 pSurvey2_25:pSurvey2_36,
                                 pSurvey2_61:pSurvey2_72))
pSurvey3 <- Survey3 %>% select(c(pSurvey3_1:pSurvey3_12,
                                 pSurvey3_25:pSurvey3_36,
                                 pSurvey3_61:pSurvey3_72))
pSurvey1 <- c(t(pSurvey1))
pSurvey2 <- c(t(pSurvey2))
pSurvey3 <- c(t(pSurvey3))
RV2022.ddl$p$pSurvey1 <- pSurvey1
RV2022.ddl$p$pSurvey2 <- pSurvey2
RV2022.ddl$p$pSurvey3 <- pSurvey3

# Create param models
# For psi

AlphaPsi <- list(formula =~ alpha)
BetaPsi <- list (formula =~ beta)

# For Theta
TempTheta <-  list(formula=~tempTheta + Survey1Theta + Survey2Theta + Survey3Theta)
AlphaTempTheta <-  list(formula =~ alpha + tempTheta + Survey1Theta + Survey2Theta + Survey3Theta)
BetaTempTheta <- list (formula =~ beta + tempTheta + Survey1Theta + Survey2Theta + Survey3Theta)

# For p
TempP <-  list(formula=~tempP + pSurvey1 + pSurvey2 + pSurvey3)
AlphaTempP <-  list(formula =~ alpha + tempP + pSurvey1 + pSurvey2 + pSurvey3)
BetaTempP <- list (formula =~ beta + tempP + pSurvey1 + pSurvey2 + pSurvey3)

# If temp included
# Psi ~ Temp
Null <- list(formula =~ 1)
TrueNull <- mark(data = RV2022.pr,
                 ddl = RV2022.ddl,
                 model.parameters = list(Psi = Null,
                                         Theta = Null,
                                         p = Null))
# Creating a function to make my life easier for creating models
RV_mark_func <- function(data = RV2022.pr, 
                         ddl = RV2022.ddl,
                         Psi = Null,
                         Theta = TempTheta,
                         p = TempP){
  mark(data = data, 
       ddl = ddl,
       model.parameters = list(Psi = Psi,
                               Theta = Theta,
                               p = p))
}


# Null model, but really a temp model
NullNullNull <- RV_mark_func()

### Psi is null
# alpha only on p
NullNullAlpha <- RV_mark_func(p = AlphaTempP)
# beta only on p
NullNullBeta <- RV_mark_func(p = BetaTempP)
# alpha only on theta
NullAlphaNull <- RV_mark_func(Theta = AlphaTempTheta)
# alpha on theta and p
NullAlphaAlpha <- RV_mark_func(Theta = AlphaTempTheta,
                               p = AlphaTempP)
# Alpha on theta, beta on p
NullAlphaBeta <- RV_mark_func(Theta = AlphaTempTheta,
                              p = BetaTempP)
# Beta on theta only
NullBetaNull <- RV_mark_func(Theta = BetaTempTheta)
# Beta on theta, alpha on p
NullBetaAlpha <- RV_mark_func(Theta = BetaTempTheta,
                              p = AlphaTempP)
# Beta on theta and p
NullBetaBeta <- RV_mark_func(Theta = BetaTempTheta,
                             p = BetaTempP)
### Psi ~ alpha
# Alpha on psi only
AlphaNullNull <- RV_mark_func(Psi = AlphaPsi)
# alpha on p
AlphaNullAlpha <- RV_mark_func(Psi = AlphaPsi,
                               p = AlphaTempP)
# beta on p
AlphaNullBeta <- RV_mark_func(Psi = AlphaPsi,
                              p = BetaTempP)
# Alpha on Theta
AlphaAlphaNull <- RV_mark_func(Psi = AlphaPsi,
                               Theta = AlphaTempTheta)
# Alpha on all 3
AlphaAlphaAlpha <- RV_mark_func(Psi = AlphaPsi,
                                Theta = AlphaTempTheta,
                                p = AlphaTempP)
# Alph on theta, beta on p
AlphaAlphaBeta <- RV_mark_func(Psi = AlphaPsi,
                               Theta = AlphaTempTheta,
                               p = BetaTempP)
# Beta on theta,
AlphaBetaNull <- RV_mark_func(Psi = AlphaPsi,
                              Theta = BetaTempTheta)
# beta on theta, alpha on p
AlphaBetaAlpha <- RV_mark_func(Psi = AlphaPsi,
                               Theta = BetaTempTheta,
                               p = AlphaTempP)
# Beta on theta and p
AlphaBetaBeta <- RV_mark_func(Psi = AlphaPsi,
                              Theta = BetaTempTheta,
                              p = BetaTempP)

### Psi ~ beta
# Beta only on Psi
BetaNullNull <- RV_mark_func(Psi = BetaPsi)
# Alpa on p
BetaNullAlpha <- RV_mark_func(Psi = BetaPsi,
                              p = AlphaTempP)
# Beta on psi an p
BetaNullBeta <- RV_mark_func(Psi = BetaPsi,
                             p = BetaTempP)
# Alpha on theta
BetaAlphaNull <- RV_mark_func(Psi = BetaPsi,
                              Theta = AlphaTempTheta)
# Alpha on Theta and p
BetaAlphaAlpha <- RV_mark_func(Psi = BetaPsi,
                               Theta = AlphaTempTheta,
                               p = AlphaTempP)
# Alpha on theta, beta on p
BetaAlphaBeta <- RV_mark_func(Psi = BetaPsi,
                              Theta = AlphaTempTheta,
                              p = BetaTempP)
# Beta on theta
BetaBetaNull <- RV_mark_func(Psi = BetaPsi,
                             Theta = BetaTempTheta)
# Beta on theta, alpha on p
BetaBetaAlpha <- RV_mark_func(Psi = BetaPsi,
                              Theta = BetaTempTheta,
                              p = AlphaTempP)
# Beta on all three
BetaBetaBeta <- RV_mark_func(Psi = BetaPsi,
                             Theta = BetaTempTheta,
                             p = BetaTempP)
RV2022_mods <-  collect.models()
View(RV2022_mods$model.table)

setwd(data)
saveRDS(RV2022_mods,file = "RV2022_mods.RDS")

### 2024

# remove 2022 models
rm(list = names(RV2022_mods))

# will need to make one dataframe.
SiteCovs_2024_df <- do.call(cbind, SiteCovs_2024)
colnames(SiteCovs_2024_df) <- c("alpha","beta")
RV_2024_wide <- RV_2024_wide %>% select(!c(Year,SiteID))
cols_to_convert <- colnames(RV_2024_wide)[3:ncol(RV_2024_wide)]
RV_2024_wide <- RV_2024_wide %>%
  mutate(across(all_of(cols_to_convert), as.character))
RV_2024_wide[is.na(RV_2024_wide)] <- '.' # Change NA to .
ch_2024 <- RV_2024_wide %>% unite('ch', 3:ncol(RV_2024_wide),sep = "")

RV_2024_Mark <- cbind(SiteCovs_2024_df,temp_data_2024,ch_2024)

### Add survey number
# Add survey data
nsurveys <- 3
Yes <- rep(1,numFrogs*nrow(RV_2024_Mark)/nsurveys)
No <- rep(0,numFrogs*nrow(RV_2024_Mark)/nsurveys)
Survey1 <- matrix(data = c(Yes,No,No), nrow = nrow(RV_2024_Mark), 
                  ncol =  dets)
colnames(Survey1) <- 1:dets
Survey2 <- matrix(data = c(No,Yes,No), nrow = nrow(RV_2024_Mark), 
                  ncol =  dets)
colnames(Survey2) <- 1:dets
Survey3 <- matrix(data = c(No,No,Yes), nrow = nrow(RV_2024_Mark), 
                  ncol =  dets)
colnames(Survey3) <- 1:dets

columns <- 1:ncol(Survey1)
for(i in 1:length(columns)){
  colnames(Survey1)[[i]] <- paste("pSurvey1",columns[[i]],sep="_")
  colnames(Survey2)[[i]] <- paste("pSurvey2",columns[[i]],sep="_")
  colnames(Survey3)[[i]] <- paste("pSurvey3",columns[[i]],sep="_")
}

# RV_2024_Mark <- cbind(RV_2024_Mark,Survey1,Survey2,Survey3)

#create processed data
RV2024.pr <- process.data(RV_2024_Mark,
                          model = 'MultScalOcc',
                          mixtures = 2,
                          groups = "ComplexID")



RV2024.ddl<- make.design.data(RV2024.pr)

# Add temp to ddl
tempTheta <- as.matrix(temp_data_2024)

temp1 <- tempTheta[,1:6]
temp2 <-  tempTheta[,13:18]
temp3 <- tempTheta[,25:30]
tempTheta <- cbind(temp1,temp2,temp3)
tempTheta <- c(t(tempTheta))
tempTheta
length(tempTheta)


tempP <- c(t(as.matrix(temp_data_2024)))
nrow(RV2024.ddl$Theta)
RV2024.ddl$Theta$tempTheta <- tempTheta
RV2024.ddl$p$tempP <- tempP

# add survey to ddl
Survey1 <- as.data.frame(Survey1)
Survey1Theta <- Survey1 %>% select(c(pSurvey1_1:pSurvey1_6,
                                     pSurvey1_25:pSurvey1_30,
                                     pSurvey1_61:pSurvey1_66))
Survey2 <- as.data.frame(Survey2)
Survey2Theta <- Survey2 %>% select(c(pSurvey2_1:pSurvey2_6,
                                     pSurvey2_25:pSurvey2_30,
                                     pSurvey2_61:pSurvey2_66))
Survey3 <- as.data.frame(Survey3)
Survey3Theta <- Survey3 %>% select(c(pSurvey3_1:pSurvey3_6,
                                     pSurvey3_25:pSurvey3_30,
                                     pSurvey3_61:pSurvey3_66))
Survey1Theta <- c(t(Survey1Theta))
Survey2Theta <- c(t(Survey2Theta))
Survey3Theta <- c(t(Survey3Theta))
RV2024.ddl$Theta$Survey1Theta <- Survey1Theta
RV2024.ddl$Theta$Survey2Theta <- Survey2Theta
RV2024.ddl$Theta$Survey3Theta <- Survey3Theta

pSurvey1 <- Survey1 %>% select(c(pSurvey1_1:pSurvey1_12,
                                 pSurvey1_25:pSurvey1_36,
                                 pSurvey1_61:pSurvey1_72))
pSurvey2 <- Survey2 %>% select(c(pSurvey2_1:pSurvey2_12,
                                 pSurvey2_25:pSurvey2_36,
                                 pSurvey2_61:pSurvey2_72))
pSurvey3 <- Survey3 %>% select(c(pSurvey3_1:pSurvey3_12,
                                 pSurvey3_25:pSurvey3_36,
                                 pSurvey3_61:pSurvey3_72))
pSurvey1 <- c(t(pSurvey1))
pSurvey2 <- c(t(pSurvey2))
pSurvey3 <- c(t(pSurvey3))
RV2024.ddl$p$pSurvey1 <- pSurvey1
RV2024.ddl$p$pSurvey2 <- pSurvey2
RV2024.ddl$p$pSurvey3 <- pSurvey3

# Create param models
# For psi

AlphaPsi <- list(formula =~ alpha)
BetaPsi <- list (formula =~ beta)

# For Theta
TempTheta <-  list(formula=~tempTheta + Survey1Theta + Survey2Theta + Survey3Theta)
AlphaTempTheta <-  list(formula =~ alpha + tempTheta + Survey1Theta + Survey2Theta + Survey3Theta)
BetaTempTheta <- list (formula =~ beta + tempTheta + Survey1Theta + Survey2Theta + Survey3Theta)

# For p
TempP <-  list(formula=~tempP + pSurvey1 + pSurvey2 + pSurvey3)
AlphaTempP <-  list(formula =~ alpha + tempP + pSurvey1 + pSurvey2 + pSurvey3)
BetaTempP <- list (formula =~ beta + tempP + pSurvey1 + pSurvey2 + pSurvey3)

# If temp included
# Psi ~ Temp

# Creating a function to make my life easier for creating models
Null <- list(formula =~ 1)
TrueNull <- mark(data = RV2024.pr,
                 ddl = RV2024.ddl,
                 model.parameters = list(Psi = Null,
                                         Theta = Null,
                                         p = Null))
RV_mark_func <- function(data = RV2024.pr, 
                         ddl = RV2024.ddl,
                         Psi = Null,
                         Theta = TempTheta,
                         p = TempP){
  mark(data = data, 
       ddl = ddl,
       model.parameters = list(Psi = Psi,
                               Theta = Theta,
                               p = p))
}


# Null model, but really a temp model
NullNullNull <- RV_mark_func()

### Psi is null
# alpha only on p
NullNullAlpha <- RV_mark_func(p = AlphaTempP)
# beta only on p
NullNullBeta <- RV_mark_func(p = BetaTempP)
# alpha only on theta
NullAlphaNull <- RV_mark_func(Theta = AlphaTempTheta)
# alpha on theta and p
NullAlphaAlpha <- RV_mark_func(Theta = AlphaTempTheta,
                               p = AlphaTempP)
# Alpha on theta, beta on p
NullAlphaBeta <- RV_mark_func(Theta = AlphaTempTheta,
                              p = BetaTempP)
# Beta on theta only
NullBetaNull <- RV_mark_func(Theta = BetaTempTheta)
# Beta on theta, alpha on p
NullBetaAlpha <- RV_mark_func(Theta = BetaTempTheta,
                              p = AlphaTempP)
# Beta on theta and p
NullBetaBeta <- RV_mark_func(Theta = BetaTempTheta,
                             p = BetaTempP)
### Psi ~ alpha
# Alpha on psi only
AlphaNullNull <- RV_mark_func(Psi = AlphaPsi)
# alpha on p
AlphaNullAlpha <- RV_mark_func(Psi = AlphaPsi,
                               p = AlphaTempP)
# beta on p
AlphaNullBeta <- RV_mark_func(Psi = AlphaPsi,
                              p = BetaTempP)
# Alpha on Theta
AlphaAlphaNull <- RV_mark_func(Psi = AlphaPsi,
                               Theta = AlphaTempTheta)
# Alpha on all 3
AlphaAlphaAlpha <- RV_mark_func(Psi = AlphaPsi,
                                Theta = AlphaTempTheta,
                                p = AlphaTempP)
# Alph on theta, beta on p
AlphaAlphaBeta <- RV_mark_func(Psi = AlphaPsi,
                               Theta = AlphaTempTheta,
                               p = BetaTempP)
# Beta on theta,
AlphaBetaNull <- RV_mark_func(Psi = AlphaPsi,
                              Theta = BetaTempTheta)
# beta on theta, alpha on p
AlphaBetaAlpha <- RV_mark_func(Psi = AlphaPsi,
                               Theta = BetaTempTheta,
                               p = AlphaTempP)
# Beta on theta and p
AlphaBetaBeta <- RV_mark_func(Psi = AlphaPsi,
                              Theta = BetaTempTheta,
                              p = BetaTempP)

### Psi ~ beta
# Beta only on Psi
BetaNullNull <- RV_mark_func(Psi = BetaPsi)
# Alpa on p
BetaNullAlpha <- RV_mark_func(Psi = BetaPsi,
                              p = AlphaTempP)
# Beta on psi an p
BetaNullBeta <- RV_mark_func(Psi = BetaPsi,
                             p = BetaTempP)
# Alpha on theta
BetaAlphaNull <- RV_mark_func(Psi = BetaPsi,
                              Theta = AlphaTempTheta)
# Alpha on Theta and p
BetaAlphaAlpha <- RV_mark_func(Psi = BetaPsi,
                               Theta = AlphaTempTheta,
                               p = AlphaTempP)
# Alpha on theta, beta on p
BetaAlphaBeta <- RV_mark_func(Psi = BetaPsi,
                              Theta = AlphaTempTheta,
                              p = BetaTempP)
# Beta on theta
BetaBetaNull <- RV_mark_func(Psi = BetaPsi,
                             Theta = BetaTempTheta)
# Beta on theta, alpha on p
BetaBetaAlpha <- RV_mark_func(Psi = BetaPsi,
                              Theta = BetaTempTheta,
                              p = AlphaTempP)
# Beta on all three
BetaBetaBeta <- RV_mark_func(Psi = BetaPsi,
                             Theta = BetaTempTheta,
                             p = BetaTempP)
RV2024_mods <-  collect.models()
View(RV2024_mods$model.table)

setwd(data)
saveRDS(RV2024_mods,file = "RV2024_mods.RDS")

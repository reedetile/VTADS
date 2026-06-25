#Description-----------------------------------------
#Modelling of disease dynamics (2nd draft)
# Wanted a second verson to clean up code
# Plus I need to change some analysis
#  12 January 2026
#RCS

#Initialize -----------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RMark)
# Load functions--------------------------------------

# Constants
Surveys <- 3
numFrogs <- 12*Surveys
dets <- numFrogs*2

# Global Variables------------------------------------
repo <- getwd()
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

# will need to make one dataframe.
SiteCovs_2022_df <- do.call(cbind, SiteCovs_2022)
colnames(SiteCovs_2022_df) <- c("alpha","beta")
SiteCovs_2024_df <- do.call(cbind, SiteCovs_2024)
colnames(SiteCovs_2024_df) <- c("alpha","beta")

# Stack Data
RV_2022_wide[is.na(RV_2022_wide)] <- '.' # Change NA to .
ch_2022 <- RV_2022_wide %>% unite('ch', 3:ncol(RV_2022_wide),sep = "")
RV_2022_Mark <- cbind(SiteCovs_2022_df,temp_data_2022,ch_2022)

RV_2024_wide <- RV_2024_wide %>% select(!c(Year,SiteID))
cols_to_convert <- colnames(RV_2024_wide)[3:ncol(RV_2024_wide)]
RV_2024_wide <- RV_2024_wide %>%
  mutate(across(all_of(cols_to_convert), as.character))
RV_2024_wide[is.na(RV_2024_wide)] <- '.' # Change NA to .
ch_2024 <- RV_2024_wide %>% unite('ch', 3:ncol(RV_2024_wide),sep = "")
RV_2024_Mark <- cbind(SiteCovs_2024_df,temp_data_2024,ch_2024)

RV_Mark <- rbind(RV_2022_Mark,RV_2024_Mark)

Surveys_mat <- matrix(data=NA, nrow = nrow(RV_Mark),ncol = numFrogs)
for (i in 1:nrow(Surveys_mat)) {
  Surveys_mat[i,] <- rep(1:3, each = numFrogs/Surveys)
}


Survey_num_df <- data.frame(Surveys_mat)
for(i in 1:ncol(Survey_num_df)){
  colnames(Survey_num_df)[[i]] <- paste("Survey_num",i,sep="")
}

Surveys_fac_mat <- matrix(data=NA, nrow = nrow(RV_Mark),ncol = numFrogs*Surveys)

survey_temp <- c(rep(1,12),rep(0,24),rep(0,12),rep(1,12),rep(0,12),rep(0,24),rep(1,12))
survey_temporary <- c(rep(1,12),
                      rep(0,24),
                      rep(0,12),
                      rep(1,12),
                      rep(0,12),
                      rep(0,24),
                      rep(1,12))

for (i in 1:nrow(Surveys_fac_mat)) {
  Surveys_fac_mat[i,] <- survey_temporary
}
Survey_fac_df <- data.frame(Surveys_fac_mat)

Survey_fac_df_names <- rep(NA,108)
for(i in 1:ncol(Survey_fac_df)){
  Survey_fac_df_names[[i]] <- if(i <= 12){
    paste("Survey1_fac_",i,sep="")
  } else
    if(i >= 13 & i <=24){
      paste("Survey2_fac_",i-12,sep="")
    } else
      if(i >= 25 & i <=36){
        paste("Survey3_fac_",i-24,sep="")
      } else
        if(i >= 37 & i <= 48){
          paste("Survey1_fac_",i-24,sep="")
        } else
          if(i >= 49 & i <= 60){
            paste("Survey2_fac_",i-36,sep="")
          } else
            if(i >= 61 & i <= 72){
              paste("Survey3_fac_",i-48,sep="")
            } else
              if(i >= 73 & i <= 84){
                paste("Survey1_fac_",i-48,sep="")
              } else
                if(i >= 85 & i <= 96){
                  paste("Survey2_fac_",i-60,sep="")
                } else
                  if(i >= 97 & i <= 108){
                    paste("Survey3_fac_",i-72,sep="")
                  }
}

colnames(Survey_fac_df) <- Survey_fac_df_names

RV_Mark <- cbind(RV_Mark, Survey_num_df,Survey_fac_df)

#create processed data
RV.pr <- process.data(RV_Mark,
                          model = 'MultScalOcc',
                          mixtures = 2)

RV.ddl<- make.design.data(RV.pr)




# # add survey to ddl
# RV.ddl$Theta$Survey <- rep(1:3, each = numFrogs/Surveys)
# RV.ddl$p$Survey <- rep(1:3, each = dets/Surveys)
save(RV.ddl, RV.pr, file = "RVMarkData.RData")


### Stepwise approach

## 06/18/2026: Updated approach: p first then theta

# p first. Null vs. temp/temp^2 vs survey(lin)/survey(fac)

temp <- list(formula=~temp)
tempsq <- list(formula =~ temp + I(temp^2))
Alpha <- list(formula=~alpha)
Null <- list(formula =~ 1)
SurveyThetaNum <- list(formula =~ Survey_num)
SurveyThetaFac <- list(formula =~ Survey1_fac_ + Survey2_fac_ + Survey3_fac_)
SurveyPNum <- list(formula =~ Survey_num)
SurveyPFac <- list(formula =~ Survey1_fac_ + Survey2_fac_ + Survey3_fac_)

TrueNull <- mark(data = RV.pr,
                 ddl = RV.ddl,
                 model.parameters = list(Psi = Null,
                                         Theta = Null,
                                         p = Null))

TempP <- mark(data = RV.pr,
              ddl = RV.ddl,
              model.parameters = list(Psi = Alpha,
                                      Theta = Alpha,
                                      p = temp))
TempSqP <- mark(data = RV.pr,
                ddl = RV.ddl,
                model.parameters = list(Psi = Alpha,
                                        Theta = Alpha,
                                        p = tempsq))

Survey_num <- mark(data = RV.pr,
                   ddl = RV.ddl,
                   model.parameters = list(Psi = Alpha,
                                           Theta = Alpha,
                                           p = SurveyPNum))

Survey_fac <- mark(data = RV.pr,
                   ddl = RV.ddl,
                   model.parameters = list(Psi = Alpha,
                                           Theta = Alpha,
                                           p = SurveyPFac))

RV_p_mods <- collect.models()
View(RV_p_mods$model.table) 
# at least initially it looks like I should carry over the numeric survey variable

## theta
rm(Survey_fac,
   Survey_num,
   TempP,
   TempSqP,
   TrueNull)

TrueNull <- mark(data = RV.pr,
                 ddl = RV.ddl,
                 model.parameters = list(Psi = Null,
                                         Theta = Null,
                                         p = Null),
                 model.name = "NullModel")

TempTheta <- mark(data = RV.pr,
              ddl = RV.ddl,
              model.parameters = list(Psi = Alpha,
                                      Theta = temp,
                                      p = SurveyPNum),
              model.name = "TempLinear")
TempSqP <- mark(data = RV.pr,
                ddl = RV.ddl,
                model.parameters = list(Psi = Alpha,
                                        Theta = tempsq,
                                        p = SurveyPNum),
                model.name = "TempSq")

Survey_num <- mark(data = RV.pr,
                   ddl = RV.ddl,
                   model.parameters = list(Psi = Alpha,
                                           Theta = SurveyThetaNum,
                                           p = SurveyPNum),
                   model.name = "SurveyNumeric")

Survey_fac <- mark(data = RV.pr,
                   ddl = RV.ddl,
                   model.parameters = list(Psi = Alpha,
                                           Theta = SurveyThetaFac,
                                           p = SurveyPNum),
                   model.name = "SurveyFactor")

RV_Theta_mods <- collect.models()
View(RV_Theta_mods$model.table) 

Survey_num$results$beta

rm(Survey_fac,
   Survey_num,
   TempP,
   TempSqP,
   TrueNull)







# Create param models

# Round 1: Survey is linear
# For psi

AlphaPsi <- list(formula =~ alpha)
BetaPsi <- list (formula =~ beta)

# For Theta
SurveyTheta <-  list(formula=~ Survey_num)
AlphaTheta <-  list(formula =~ alpha + Survey_num)
BetaTheta <- list (formula =~ beta + Survey_num)

# For p
SurveyP <-  list(formula=~ Survey_num)
AlphaP <-  list(formula =~ alpha + Survey_num)
BetaP <- list (formula =~ beta + Survey_num)
# If temp included
# Psi ~ Temp

# Creating a function to make my life easier for creating models
Null <- list(formula =~ 1)
TrueNull <- mark(data = RV.pr,
                 ddl = RV.ddl,
                 model.parameters = list(Psi = Null,
                                         Theta = Null,
                                         p = Null))
RV_mark_func <- function(data = RV.pr, 
                         ddl = RV.ddl,
                         Psi = Null,
                         Theta = SurveyTheta,
                         p = SurveyP){
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
NullNullAlpha <- RV_mark_func(p = AlphaP)
# beta only on p
NullNullBeta <- RV_mark_func(p = BetaP)
# alpha only on theta
NullAlphaNull <- RV_mark_func(Theta = AlphaTheta)
# alpha on theta and p
NullAlphaAlpha <- RV_mark_func(Theta = AlphaTheta,
                               p = AlphaP)
# Alpha on theta, beta on p
NullAlphaBeta <- RV_mark_func(Theta = AlphaTheta,
                              p = BetaP)
# Beta on theta only
NullBetaNull <- RV_mark_func(Theta = BetaTheta)
# Beta on theta, alpha on p
NullBetaAlpha <- RV_mark_func(Theta = BetaTheta,
                              p = AlphaP)
# Beta on theta and p
NullBetaBeta <- RV_mark_func(Theta = BetaTheta,
                             p = BetaP)
### Psi ~ alpha
# Alpha on psi only
AlphaNullNull <- RV_mark_func(Psi = AlphaPsi)
# alpha on p
AlphaNullAlpha <- RV_mark_func(Psi = AlphaPsi,
                               p = AlphaP)
# beta on p
AlphaNullBeta <- RV_mark_func(Psi = AlphaPsi,
                              p = BetaP)
# Alpha on Theta
AlphaAlphaNull <- RV_mark_func(Psi = AlphaPsi,
                               Theta = AlphaTheta)
# Alpha on all 3
AlphaAlphaAlpha <- RV_mark_func(Psi = AlphaPsi,
                                Theta = AlphaTheta,
                                p = AlphaP)
# Alph on theta, beta on p
AlphaAlphaBeta <- RV_mark_func(Psi = AlphaPsi,
                               Theta = AlphaTheta,
                               p = BetaP)
# Beta on theta,
AlphaBetaNull <- RV_mark_func(Psi = AlphaPsi,
                              Theta = BetaTheta)
# beta on theta, alpha on p
AlphaBetaAlpha <- RV_mark_func(Psi = AlphaPsi,
                               Theta = BetaTheta,
                               p = AlphaP)
# Beta on theta and p
AlphaBetaBeta <- RV_mark_func(Psi = AlphaPsi,
                              Theta = BetaTheta,
                              p = BetaP)

### Psi ~ beta
# Beta only on Psi
BetaNullNull <- RV_mark_func(Psi = BetaPsi)
# Alpa on p
BetaNullAlpha <- RV_mark_func(Psi = BetaPsi,
                              p = AlphaP)
# Beta on psi an p
BetaNullBeta <- RV_mark_func(Psi = BetaPsi,
                             p = BetaP)
# Alpha on theta
BetaAlphaNull <- RV_mark_func(Psi = BetaPsi,
                              Theta = AlphaTheta)
# Alpha on Theta and p
BetaAlphaAlpha <- RV_mark_func(Psi = BetaPsi,
                               Theta = AlphaTheta,
                               p = AlphaP)
# Alpha on theta, beta on p
BetaAlphaBeta <- RV_mark_func(Psi = BetaPsi,
                              Theta = AlphaTheta,
                              p = BetaP)
# Beta on theta
BetaBetaNull <- RV_mark_func(Psi = BetaPsi,
                             Theta = BetaTheta)
# Beta on theta, alpha on p
BetaBetaAlpha <- RV_mark_func(Psi = BetaPsi,
                              Theta = BetaTheta,
                              p = AlphaP)
# Beta on all three
BetaBetaBeta <- RV_mark_func(Psi = BetaPsi,
                             Theta = BetaTheta,
                             p = BetaP)

# # Round 2: Survey is quadratic
# # For psi
# 
# AlphaPsi <- list(formula =~ alpha)
# BetaPsi <- list (formula =~ beta)
# 
# # For Theta
# TempTheta <-  list(formula=~ temp + Survey + I(Survey^2))
# AlphaTheta <-  list(formula =~ alpha + temp + Survey + I(Survey^2))
# BetaTheta <- list (formula =~ beta + temp + Survey + I(Survey^2))
# 
# # For p
# TempP <-  list(formula=~ -1 + temp + Survey + I(Survey^2))
# AlphaP <-  list(formula =~ -1 + alpha + temp + Survey + I(Survey^2))
# BetaP <- list (formula =~ -1 +beta + temp + Survey + I(Survey^2))
# # If temp included
# # Psi ~ Temp
# 
# # Null model, but really a temp model
# NullNullNull_sq <- RV_mark_func()
# 
# ### Psi is null
# # alpha only on p
# NullNullAlpha_sq <- RV_mark_func(p = AlphaP)
# # beta only on p
# NullNullBeta_sq <- RV_mark_func(p = BetaP)
# # alpha only on theta
# NullAlphaNull_sq <- RV_mark_func(Theta = AlphaTheta)
# # alpha on theta and p
# NullAlphaAlpha_sq <- RV_mark_func(Theta = AlphaTheta,
#                                p = AlphaP)
# # Alpha on theta, beta on p
# NullAlphaBeta_sq <- RV_mark_func(Theta = AlphaTheta,
#                               p = BetaP)
# # Beta on theta only
# NullBetaNull_sq <- RV_mark_func(Theta = BetaTheta)
# # Beta on theta, alpha on p
# NullBetaAlpha_sq <- RV_mark_func(Theta = BetaTheta,
#                               p = AlphaP)
# # Beta on theta and p
# NullBetaBeta_sq <- RV_mark_func(Theta = BetaTheta,
#                              p = BetaP)
# ### Psi ~ alpha
# # Alpha on psi only
# AlphaNullNull_sq <- RV_mark_func(Psi = AlphaPsi)
# # alpha on p
# AlphaNullAlpha_sq <- RV_mark_func(Psi = AlphaPsi,
#                                p = AlphaP)
# # beta on p
# AlphaNullBeta_sq <- RV_mark_func(Psi = AlphaPsi,
#                               p = BetaP)
# # Alpha on Theta
# AlphaAlphaNull_sq <- RV_mark_func(Psi = AlphaPsi,
#                                Theta = AlphaTheta)
# # Alpha on all 3
# AlphaAlphaAlpha_sq <- RV_mark_func(Psi = AlphaPsi,
#                                 Theta = AlphaTheta,
#                                 p = AlphaP)
# # Alph on theta, beta on p
# AlphaAlphaBeta_sq <- RV_mark_func(Psi = AlphaPsi,
#                                Theta = AlphaTheta,
#                                p = BetaP)
# # Beta on theta,
# AlphaBetaNull_sq <- RV_mark_func(Psi = AlphaPsi,
#                               Theta = BetaTheta)
# # beta on theta, alpha on p
# AlphaBetaAlpha_sq <- RV_mark_func(Psi = AlphaPsi,
#                                Theta = BetaTheta,
#                                p = AlphaP)
# # Beta on theta and p
# AlphaBetaBeta_sq <- RV_mark_func(Psi = AlphaPsi,
#                               Theta = BetaTheta,
#                               p = BetaP)
# 
# ### Psi ~ beta
# # Beta only on Psi
# BetaNullNull_sq <- RV_mark_func(Psi = BetaPsi)
# # Alpa on p
# BetaNullAlpha_sq <- RV_mark_func(Psi = BetaPsi,
#                               p = AlphaP)
# # Beta on psi an p
# BetaNullBeta_sq <- RV_mark_func(Psi = BetaPsi,
#                              p = BetaP)
# # Alpha on theta
# BetaAlphaNull_sq <- RV_mark_func(Psi = BetaPsi,
#                               Theta = AlphaTheta)
# # Alpha on Theta and p
# BetaAlphaAlpha_sq <- RV_mark_func(Psi = BetaPsi,
#                                Theta = AlphaTheta,
#                                p = AlphaP)
# # Alpha on theta, beta on p
# BetaAlphaBeta_sq <- RV_mark_func(Psi = BetaPsi,
#                               Theta = AlphaTheta,
#                               p = BetaP)
# # Beta on theta
# BetaBetaNull_sq <- RV_mark_func(Psi = BetaPsi,
#                              Theta = BetaTheta)
# # Beta on theta, alpha on p
# BetaBetaAlpha_sq <- RV_mark_func(Psi = BetaPsi,
#                               Theta = BetaTheta,
#                               p = AlphaP)
# # Beta on all three
# BetaBetaBeta_sq <- RV_mark_func(Psi = BetaPsi,
#                              Theta = BetaTheta,
#                              p = BetaP)

RV_mods <-  collect.models()

RV_mods[[25]]$results$beta
View(RV_mods$model.table)

# model avg results for p ~ alpha
RV_pAlpha <- covariate.predictions(model = RV_mods,
                                   data = alpha_df,
                                   indices = 38)
mod.avg.p.lm <- lm(estimate ~ covdata, data = RV_pAlpha$estimates)
summary(mod.avg.p.lm)


setwd(data)
saveRDS(RV_mods,file = "RV_mods.RDS")

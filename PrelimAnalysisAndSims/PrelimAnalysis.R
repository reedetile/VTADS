#Description-----------------------------------------
#Preliminary analysis of VTADs data provided by Kerby
#  23 Oct 2024
#RCS

#Initialize -----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(unmarked)

# Global Variables-------------------------------------
repo <- 'C:/Users/rcscott/VTADS'
data <- paste(repo,"/Data", sep = "")
setwd(data)
Disease_Data <- read.csv('SummaryData1.csv')
Disease_Data <- Disease_Data[,1:3] #read in results from BD + RV qPCR

field_data <- read.csv('SwabDataMaster.csv')
# Program Body------------------------------------------
# Disease_Data$BD.Status[Disease_Data$BD.Status == 'Negative'] <- '0'
# Disease_Data$RV.Status[Disease_Data$RV.Status == 'Negative'] <- '0'
# 
# Disease_Data$BD.Status <- as.numeric(Disease_Data$BD.Status)
# Disease_Data$RV.Status <- as.numeric(Disease_Data$RV.Status)
# 
# Disease_Data$BD.PA <- ifelse(Disease_Data$BD.Status > 0, 1,0)
# Disease_Data$RV.PA <- ifelse(Disease_Data$RV.Status > 0, 1,0)
# sum(Disease_Data$BD.PA) #92 positive tests
# sum(Disease_Data$RV.PA) #103 positive tests
# 
# sum(Disease_Data$BD.PA)/nrow(Disease_Data) #naive prev. of 0.25
# sum(Disease_Data$RV.PA)/nrow(Disease_Data) #naive prev. of 0.28
# nrow(filter(Disease_Data, BD.PA == '1' & RV.PA == '1'))/nrow(Disease_Data) #naive cooccurrence = 0.11
# 
# # I want to match field data to disease data where possible#
# #To start, need to clean up disease_data
# 
# #remove spaces from disease_data
# Disease_Data$Sample.Name <- str_remove(Disease_Data$Sample.Name," ")
# field_data$SampleID <- str_remove_all(field_data$SampleID,"-")
# 
# 
# #Okay let's do an anti-join to see what is missing/probably mislabelled
# colnames(Disease_Data)[1] <- 'SampleID'
# missing_names <- anti_join(x = Disease_Data, y = field_data, by = 'SampleID')
# 
# #Okay there are 163 samples that don't match the field data
# #But, I don't have any controls in the field data so we can start by removing those from the missing data
# missing_names <- missing_names %>% filter(!grepl("C",SampleID))
# 
# #Okay, i want to check something else. Some of these columns I'm betting just are 
# #missing a 0 at the front
# missing_zeros <- missing_names %>% filter(nchar(SampleID) == 7 
#                                           & !substr(SampleID, 1, 1) == "0")
# #Okay, so there are 116 samples that I'm guessing are just missing a 0 at the front. GUESSING#
# missing_zeros$SampleID <- paste("0",missing_zeros$SampleID, sep = "")
# anti_join(x = missing_zeros, y = field_data, by = 'SampleID')
# write.csv(x = missing_names, file = "missnamed.csv")

### Okay, let's create a new dataframe of JUST the matching samples ###
Disease_Data$BD.Status[Disease_Data$BD.Status == 'Negative'] <- '0'
Disease_Data$RV.Status[Disease_Data$RV.Status == 'Negative'] <- '0'

Disease_Data$BD.Status <- as.numeric(Disease_Data$BD.Status)
Disease_Data$RV.Status <- as.numeric(Disease_Data$RV.Status)

colnames(Disease_Data)[1] <- 'SampleID'
field_data$SampleID <- str_remove_all(field_data$SampleID,"-")
prelim_data <- Disease_Data %>% 
  left_join(field_data, by = "SampleID")

prelim_data2 <- prelim_data %>% filter(!is.na(ComplexID)) #has 201 obs. 359-201 = 158. Which is the amount of samples
# with incorrect names!

### Limit number of columns ###
prelim_data_RV <- prelim_data2 %>% select(c(RV.Status, ComplexID, PondID, SiteID, Survey.Number, CatchOfDay, Species))
prelim_data_BD <- prelim_data2 %>% select(c(BD.Status, ComplexID, PondID, SiteID, Survey.Number, CatchOfDay, Species))

#Change BD/RV status to 0/1
prelim_data_RV$RV.Status <- ifelse(prelim_data_RV$RV.Status > 0, 1,0)
prelim_data_BD$BD.Status <- ifelse(prelim_data_BD$BD.Status > 0, 1,0)

# Let's create some dummy data
Num_visits <- max(prelim_data2$Survey.Number)
Num_frogs <- max(prelim_data2$CatchOfDay)
dummy_RV <- data.frame(RV.Status = rep(NA, time = Num_visits*Num_frogs),
                    ComplexID = rep("Dummy", time = Num_visits*Num_frogs),
                    PondID = rep("Dummy", time = Num_visits*Num_frogs),
                    SiteID = rep("Dummy", time = Num_visits*Num_frogs),
                    Survey.Number = rep(1:Num_visits, times = Num_frogs),
                    CatchOfDay = rep(1:Num_frogs, times = Num_visits),
                    Species = rep(NA, time = Num_visits*Num_frogs))
dummy_BD <- data.frame(BD.Status = rep(NA, time = Num_visits*Num_frogs),
                       ComplexID = rep("Dummy", time = Num_visits*Num_frogs),
                       PondID = rep("Dummy", time = Num_visits*Num_frogs),
                       SiteID = rep("Dummy", time = Num_visits*Num_frogs),
                       Survey.Number = rep(1:Num_visits, times = Num_frogs),
                       CatchOfDay = rep(1:Num_frogs, times = Num_visits),
                       Species = rep(NA, time = Num_visits*Num_frogs))


#then we can rbind to the rv and bd data
prelim_data_RV <- rbind(prelim_data_RV, dummy_RV)
prelim_data_BD <- rbind(prelim_data_BD, dummy_BD)

#Pivot wider so this can be read in to unmarked
RV_wider <- prelim_data_RV %>% pivot_wider(names_from = c("Survey.Number","CatchOfDay"), values_from = c(RV.Status, Species))
BD_wider <- prelim_data_BD %>% pivot_wider(names_from = c("Survey.Number","CatchOfDay"), values_from = c(BD.Status, Species))

#Need to get rid of dummy rows
RV_wider <- filter(.data = RV_wider, !SiteID %in% "Dummy")
BD_wider <- filter(.data = BD_wider, !SiteID %in% "Dummy")

#Okay now we need to order the columns
#separate in to different dataframes
RV_wider_sitecovs <- RV_wider[,1:3]
RV_wider_obsdet <- RV_wider[,4:ncol(RV_wider)]
RV_wider_obsdet <- RV_wider_obsdet[,sort(colnames(RV_wider_obsdet))]
RV_wider_dets <- RV_wider_obsdet[,1:42]
RV_wider_ObsCovs <- RV_wider_obsdet[,43:ncol(RV_wider_obsdet)]

BD_wider_sitecovs <- BD_wider[,1:3]
BD_wider_obsdet <- BD_wider[,4:ncol(BD_wider)] 
BD_wider_obsdet <- BD_wider_obsdet[,sort(colnames(BD_wider_obsdet))]
BD_wider_dets <- BD_wider_obsdet[,1:42]
BD_wider_ObsCovs <- BD_wider_obsdet[,43:ncol(BD_wider_obsdet)]

# #reordering
# RV_wider_dets <- RV_wider_dets[,sort(colnames(RV_wider_dets))]
# RV_wider_ObsCovs <- RV_wider_ObsCovs[,sort(colnames(RV_wider_ObsCovs))]
# 
# BD_wider_dets <- BD_wider_dets[,sort(colnames(BD_wider_dets))]
# BD_wider_ObsCovs <- BD_wider_ObsCovs[,sort(colnames(BD_wider_ObsCovs))]


#Need to code in variable for survey #
S <- 3
SurveyNum_RV <- data.frame(Survey = matrix(rep(1:S, times = nrow(RV_wider)),
                                           nrow = nrow(RV_wider), 
                                           ncol = S,
                                           byrow = TRUE))
SurveyNum_BD <- data.frame(Survey = matrix(rep(1:S, times = nrow(BD_wider)),
                                           nrow = nrow(BD_wider), 
                                           ncol = S,
                                           byrow = TRUE))


#okayyyy lets get read to read these in to unmarked!
RV_frame <- unmarkedMultFrame(y = RV_wider_dets, 
                              siteCovs = RV_wider_sitecovs, 
                              yearlySiteCovs = list(SurveyNum = SurveyNum_RV),
                              obsCovs = list(Species = RV_wider_ObsCovs),
                              numPrimary = 3)

BD_frame <- unmarkedMultFrame(y = BD_wider_dets, 
                              siteCovs = BD_wider_sitecovs, 
                              yearlySiteCovs = list(SurveyNum = SurveyNum_BD),
                              obsCovs = list(Species = BD_wider_ObsCovs),
                              numPrimary = 3)

#Null model
Null_RV <- goccu(psiformula = ~1, phiformula = ~1, pformula= ~1, RV_frame)
Null_BD <- goccu(psiformula = ~1, phiformula = ~1, pformula= ~1, BD_frame)

plogis(coef(Null_RV))
plogis(coef(Null_BD))


#"availability" by survey
surveyMod_RV <- goccu(psiformula = ~1, phiformula = ~SurveyNum, pformula= ~1, RV_frame)
surveyMod_BD <- goccu(psiformula = ~1, phiformula = ~SurveyNum, pformula= ~1, BD_frame)

plogis(coef(surveyMod_RV))
plogis(coef(surveyMod_BD))

#fitting models on survey
surveyfit_RV <- fitList('null' = Null_RV, "SurveyEffect" = surveyMod_RV)
modSel(surveyfit_RV)

surveyfit_BD <- fitList('null' = Null_BD, "SurveyEffect" = surveyMod_BD)
modSel(surveyfit_BD)

# Occupancy by pond
PondMod_RV <- goccu(psiformula = ~SiteID, phiformula = ~1, pformula = ~1, RV_frame)
PondMod_BD <- goccu(psiformula = ~SiteID, phiformula = ~1, pformula = ~1, BD_frame)

plogis(coef(PondMod_RV))
plogis(coef(PondMod_BD))

#fitting models on pond
occufit_RV <- fitList('null' = Null_RV, "PondEffect" = PondMod_RV)
modSel(occufit_RV)

occufit_BD <- fitList('null' = Null_BD, "PondEffect" = PondMod_BD)
modSel(occufit_BD)

#detection by species
SppMod_RV <- goccu(psiformula = ~ 1, phiformula = ~1, pformula = ~ Species, RV_frame)
SppMod_BD <- goccu(psiformula = ~ 1, phiformula = ~1, pformula = ~ Species, BD_frame)

#fitting models on species
det_fit_RV <- fitList('null' = Null_RV, "SpeciesEffect" = SppMod_RV)
modSel(det_fit_RV)

det_fit_BD <- fitList('null' = Null_BD, "SpeciesEffect" = SppMod_BD) 
modSel(det_fit_BD)


#Plotting!!!

#plotting effect of survey
#RV
survey_pred_RV <- predict(surveyMod_RV, type = "phi")
survey_pred_RV$Survey <- rep(1:3, times = nrow(survey_pred_RV)/S)
RV_Survey_plot <- ggplot(data = survey_pred_RV, aes(x = Survey, y = Predicted))
RV_Survey_plot+
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)+
  theme_classic()

#BD
survey_pred_BD <- predict(surveyMod_BD, type = "phi")
survey_pred_BD$Survey <- rep(1:3, times = nrow(survey_pred_BD)/S)
BD_Survey_plot <- ggplot(data = survey_pred_BD, aes(x = Survey, y = Predicted))
BD_Survey_plot+
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)+
  theme_classic()

#Plotting effect of species on bd
plotEffects(SppMod_BD, type = "det", covariate = "Species")

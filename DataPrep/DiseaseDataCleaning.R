# Description
# Script for cleaning + organizing disease data so it can be read into occ mod
# RCS
# 10/01/2025

# Load libraries
library(tidyr)
library(dplyr)
library(stringr)

# set working directories
repo <- "C:/Users/rcscott/VTADS"
data <- "C:/Users/rcscott/VTADS/Data"

# Read in data
setwd(data)
DiseaseData_2022 <- read.csv("qPCRresults_2022.csv")
DiseaseData_2024 <- read.csv("qPCRresults_2024.csv")
FieldData <- read.csv("SwabDataMaster.csv")
DiseaseData_2024$LabID <- str_remove(DiseaseData_2024$LabID, "'")
# DiseaseData$SampleID <- str_remove_all(DiseaseData$SampleID, "-")
# DiseaseData$SampleID <- trimws(DiseaseData$SampleID)
# FieldData$SampleID <- str_remove_all(FieldData$SampleID, "-")
# First need to join data

FieldData2 <- FieldData %>% select(c(ComplexID, PondID, SiteID, Survey.Number, CatchOfDay, SampleID,WaterTemp.Nearest,Species))
#Pare down field data to only what I need

#########
###2022##
#########
DiseaseData2_2022 <- left_join(DiseaseData_2022, FieldData2, by = "SampleID")
# Cool! I have all the data together.



DiseaseData2_2022 <- DiseaseData2_2022 %>% filter(Species == "GF")# Need to remove any samples that aren't GF's

# Remove negs and controls
DiseaseData2_2022 <- DiseaseData2_2022 %>% filter(!is.na(SiteID))
# DiseaseData2_2022$CatchOfDay <- sprintf("%02d",DiseaseData2_2022$CatchOfDay) #to make everything 2 digits.
# Don't need this for 2022 data
DiseaseData2_2022$LabNumber <- NA # A somewhat arbitrary number I'm using to order samples for pivoting

Complex <- 1:max(DiseaseData2_2022$ComplexID)
Pond <- 1:max(DiseaseData2_2022$PondID)
Visit <- 1:max(DiseaseData2_2022$Survey.Number)

for (i in 1:length(Complex)) {
  for (j in 1:length(Pond)) {
    for (k in 1:length(Visit)) {
      df <- DiseaseData2_2022 %>% filter(ComplexID == as.character(i) &
                                           PondID == as.character(j) &
                                           Survey.Number == as.character(k))
      if(nrow(df) == 0) next
      df <- df %>% arrange(CatchOfDay)
      df$LabNumber <- 1:nrow(df)
      DiseaseData2_2022$LabNumber[DiseaseData2_2022$ComplexID == as.character(i) &
                                    DiseaseData2_2022$PondID == as.character(j) &
                                    DiseaseData2_2022$Survey.Number == as.character(k)] <- df$LabNumber
      
    }
    
  }
  
}

max(DiseaseData2_2022$LabNumber)
DiseaseData2_2022$LabNumber <- as.numeric(DiseaseData2_2022$LabNumber)
DiseaseData2_2022 <- filter(DiseaseData2_2022, LabNumber < 13) #Just gonna remove the 2 cases where we caught a 13th gf

#Now I need to pivot it in a way that it can be read by an occ mod in unmarked
#BD
BD_2022 <- DiseaseData2_2022 %>% select(!c(RV_Rep1:RV_Rep2,SiteID,WaterTemp.Nearest, CatchOfDay,Species))
BD_2022_longer <- BD_2022 %>% pivot_longer(cols = BD_Rep1:BD_Rep2,names_to = "Rep", values_to = "PA")
BD_2022_longer$Rep <- str_remove(BD_2022_longer$Rep, "BD_")
BD_2022_wide <- BD_2022_longer %>% 
  select(!SampleID) %>%
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)
BD_2022_wide <- arrange(BD_2022_wide,as.numeric(ComplexID),as.numeric(PondID))
#RV
RV_2022 <- DiseaseData2_2022 %>% select(!c(BD_Rep1:BD_Rep2,SiteID,WaterTemp.Nearest, CatchOfDay,Species))
RV_2022_longer <- RV_2022 %>% pivot_longer(cols = RV_Rep1:RV_Rep2,names_to = "Rep", values_to = "PA")
RV_2022_longer$Rep <- str_remove(RV_2022_longer$Rep, "RV_")
RV_2022_wide <- RV_2022_longer %>% 
  select(!SampleID) %>%
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)



##########
###2024###
##########

FieldData2 <- FieldData2 %>% select(!Species)


DiseaseData2_2024 <- left_join(DiseaseData_2024, FieldData2, by = "SampleID")
# Cool! I have all the data together.

# Remove negs and controls
DiseaseData2_2024 <- DiseaseData2_2024 %>% filter(!is.na(SiteID))
DiseaseData2_2024$CatchOfDay <- sprintf("%02d",DiseaseData2_2024$CatchOfDay) #to make everything 2 digits
DiseaseData2_2024$LabNumber <- NA # A somewhat arbitrary number I'm using to order samples for pivoting

Complex <- 1:max(DiseaseData2_2024$ComplexID)
Pond <- 1:max(DiseaseData2_2024$PondID)
Visit <- 1:max(DiseaseData2_2024$Survey.Number)

for (i in 1:length(Complex)) {
  for (j in 1:length(Pond)) {
    for (k in 1:length(Visit)) {
      df <- DiseaseData2_2024 %>% filter(ComplexID == as.character(i) &
                                      PondID == as.character(j) &
                                      Survey.Number == as.character(k))
      if(nrow(df) == 0) next
      df <- df %>% arrange(CatchOfDay)
      df$LabNumber <- 1:nrow(df)
      DiseaseData2_2024$LabNumber[DiseaseData2_2024$ComplexID == as.character(i) &
                     DiseaseData2_2024$PondID == as.character(j) &
                     DiseaseData2_2024$Survey.Number == as.character(k)] <- df$LabNumber
      
    }
    
  }
  
}

# Okay so actual max number of 13. That could screw things up a little
# Stopped here for today (10/01/2025). Need to next pivot wider based on Visit number + LabNumber. Should actually be pretty easy from here... I think

# DiseaseData2_2024 %>% filter(ComplexID == as.character(1) &
#                           PondID == as.character(1) &
#                           Survey.Number == as.character(1))

# DiseaseData2_2024$LabNumber[DiseaseData2_2024$ComplexID == as.character(1) &
#                          DiseaseData2_2024$PondID == as.character(2) &
#                          DiseaseData2_2024$Survey.Number == as.character(1)]
max(DiseaseData2_2024$LabNumber)
DiseaseData2_2024 <- filter(DiseaseData2_2024, LabNumber <= 12) #Just gonna remove the 2 cases where we caught a 13th gf

#Now I need to pivot it in a way that it can be read by an occ mod in unmarked
#BD
BD_2024 <- DiseaseData2_2024 %>% select(!c(LabID,RV_Rep1:RV_Rep2,SiteID,WaterTemp.Nearest, CatchOfDay))
BD_2024_longer <- BD_2024 %>% pivot_longer(cols = BD_Rep1:BD_Rep2,names_to = "Rep", values_to = "PA")
BD_2024_longer$Rep <- str_remove(BD_2024_longer$Rep, "BD_")
BD_2024_wide <- BD_2024_longer %>% 
  select(!SampleID) %>%
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


DummyData_0801 <- c("8","1",rep(NA, times = ncol(BD_2024_wide)-2))
BD_2024_wide <- rbind(BD_2024_wide, DummyData_0801)
BD_2024_wide <- arrange(BD_2024_wide,as.numeric(ComplexID),as.numeric(PondID))
#RV
RV_2024 <- DiseaseData2_2024 %>% select(!c(LabID,BD_Rep1:BD_Rep2,SiteID,WaterTemp.Nearest, CatchOfDay))
RV_2024_longer <- RV_2024 %>% pivot_longer(cols = RV_Rep1:RV_Rep2,names_to = "Rep", values_to = "PA")
RV_2024_longer$Rep <- str_remove(RV_2024_longer$Rep, "RV_")
RV_2024_wide <- RV_2024_longer %>% 
  select(!SampleID) %>%
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


DummyData_0801 <- c("8","1",rep(NA, times = ncol(RV_2024_wide)-2))
RV_2024_wide <- rbind(RV_2024_wide, DummyData_0801)
RV_2024_wide <- arrange(RV_2024_wide,as.numeric(ComplexID),as.numeric(PondID))

### Okay last thing, need to setup water temp data to be read into occmods ###
# 2022
WaterTemp_2022 <- DiseaseData2_2022 %>% select(!c(SampleID,BD_Rep1:RV_Rep2,SiteID,CatchOfDay,Species))
WaterTemp_2022$ComplexID <- sprintf("%02d",WaterTemp_2022$ComplexID)
WaterTemp_2022 <- as.data.frame(WaterTemp_2022)
WaterTemp_2022$WaterTemp.Nearest[WaterTemp_2022$WaterTemp.Nearest == ""] <- NA 
WaterTemp_2022_wider <- WaterTemp_2022 %>% pivot_wider(names_from = c(Survey.Number, LabNumber),
                                                   values_from = WaterTemp.Nearest,
                                                   names_sort = T)

WaterTemp_2022_duplicate <- merge(WaterTemp_2022_wider,WaterTemp_2022_wider, by = c("ComplexID","PondID"))
col.names <- colnames(WaterTemp_2022_duplicate)
col.names <- col.names %>% str_replace("(\\d).x","\\1_1") %>% str_replace(".y","_2")
col.names
colnames(WaterTemp_2022_duplicate) <- col.names

WaterTemp_2022_duplicate <- WaterTemp_2022_duplicate[,order(names(WaterTemp_2022_duplicate))] #now data is organized

# 2024
WaterTemp_2024 <- DiseaseData2_2024 %>% select(!c(LabID,SampleID,BD_Rep1:RV_Rep2,SiteID,CatchOfDay))
WaterTemp_2024$ComplexID <- sprintf("%02d",WaterTemp_2024$ComplexID)
WaterTemp_2024 <- as.data.frame(WaterTemp_2024)
WaterTemp_2024$WaterTemp.Nearest[WaterTemp_2024$WaterTemp.Nearest == ""] <- NA 
WaterTemp_2024_wider <- WaterTemp_2024 %>% pivot_wider(names_from = c(Survey.Number, LabNumber),
                                                       values_from = WaterTemp.Nearest,
                                                       names_sort = T)

dummy_water_data  <-  c("08","1",rep(NA, times = ncol(WaterTemp_2024_wider)-2))
WaterTemp_2024_wider <- rbind(WaterTemp_2024_wider,dummy_water_data)
WaterTemp_2024_wider <- arrange(WaterTemp_2024_wider, ComplexID, PondID)
WaterTemp_2024_duplicate <- merge(WaterTemp_2024_wider,WaterTemp_2024_wider, by = c("ComplexID","PondID"))
col.names <- colnames(WaterTemp_2024_duplicate)
col.names <- col.names %>% str_replace("(\\d).x","\\1_1") %>% str_replace(".y","_2")
col.names
colnames(WaterTemp_2024_duplicate) <- col.names

WaterTemp_2024_duplicate <- WaterTemp_2024_duplicate[,order(names(WaterTemp_2024_duplicate))] #now data is organized



setwd(data)
saveRDS(BD_2022_wide,file = "BD2022_occMod.RDS")
saveRDS(RV_2022_wide, file = "RV2022_occMod.RDS")
saveRDS(BD_2024_wide,file = "BD2024_occMod.RDS")
saveRDS(RV_2022_wide, file = "RV2024_occMod.RDS")
save(WaterTemp_2022_wider, WaterTemp_2022_duplicate, file = "WaterData2022.RData")
save(WaterTemp_2024_wider, WaterTemp_2024_duplicate, file = "WaterData2024.RData")

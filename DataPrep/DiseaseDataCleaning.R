# Description
# Script for cleaning + organizing disease data so it can be read into occ mod
# RCS
# 10/01/2025

# Load libraries
library(dplyr)
library(stringr)

# set working directories
repo <- "C:/Users/rcscott/VTADS"
data <- "C:/Users/rcscott/VTADS/Data"

# Read in data
setwd(data)
DiseaseData <- read.csv("qPCRresults.csv")
FieldData <- read.csv("SwabDataMaster.csv")
DiseaseData$LabID <- str_remove(DiseaseData$LabID, "'")
# DiseaseData$SampleID <- str_remove_all(DiseaseData$SampleID, "-")
# DiseaseData$SampleID <- trimws(DiseaseData$SampleID)
# FieldData$SampleID <- str_remove_all(FieldData$SampleID, "-")
# First need to join data

# let's do a practice by pairing down the field data
FieldData2 <- FieldData %>% select(c(ComplexID, PondID, SiteID, Survey.Number, CatchOfDay, SampleID,WaterTemp.Nearest))


DiseaseData2 <- left_join(DiseaseData, FieldData2, by = "SampleID")
# Cool! I have all the data together.

# Remove negs and controls
DiseaseData2 <- DiseaseData2 %>% filter(!is.na(SiteID))
DiseaseData2$CatchOfDay <- sprintf("%02d",DiseaseData2$CatchOfDay) #to make everything 2 digits
DiseaseData2$LabNumber <- NA # A somewhat arbitrary number I'm using to order samples for pivoting

Complex <- 1:max(DiseaseData2$ComplexID)
Pond <- 1:max(DiseaseData2$PondID)
Visit <- 1:max(DiseaseData2$Survey.Number)

for (i in 1:length(Complex)) {
  for (j in 1:length(Pond)) {
    for (k in 1:length(Visit)) {
      df <- DiseaseData2 %>% filter(ComplexID == as.character(i) &
                                      PondID == as.character(j) &
                                      Survey.Number == as.character(k))
      if(nrow(df) == 0) next
      df <- df %>% arrange(CatchOfDay)
      df$LabNumber <- 1:nrow(df)
      DiseaseData2$LabNumber[DiseaseData2$ComplexID == as.character(i) &
                     DiseaseData2$PondID == as.character(j) &
                     DiseaseData2$Survey.Number == as.character(k)] <- df$LabNumber
      
    }
    
  }
  
}

# Okay so actual max number of 13. That could screw things up a little
# Stopped here for today (10/01/2025). Need to next pivot wider based on Visit number + LabNumber. Should actually be pretty easy from here... I think

DiseaseData2 %>% filter(ComplexID == as.character(1) &
                          PondID == as.character(1) &
                          Survey.Number == as.character(1))

DiseaseData2$LabNumber[DiseaseData2$ComplexID == as.character(1) &
                         DiseaseData2$PondID == as.character(2) &
                         DiseaseData2$Survey.Number == as.character(1)]

#Now I need to pivot it in a way that it can be read by an occ mod in unmarked
BD <- DiseaseData2 %>% select(!c(LabID,RV_Rep1:RV_Rep2,SiteID,WaterTemp.Nearest))


BD_wider <- 



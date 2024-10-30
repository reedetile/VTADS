#Description-----------------------------------------
#Preliminary analysis of VTADs data provided by Kerby
#  23 Oct 2024
#RCS

#Initialize -----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Global Variables-------------------------------------
VTADS <- getwd()

Disease_Data <- read.csv('SummaryData1.csv')
Disease_Data <- Disease_Data[,1:3] #read in results from BD + RV qPCR

field_data <- read.csv('SwabDataMaster.csv')
# Program Body------------------------------------------
Disease_Data$BD.Status[Disease_Data$BD.Status == 'Negative'] <- '0'
Disease_Data$RV.Status[Disease_Data$RV.Status == 'Negative'] <- '0'

Disease_Data$BD.Status <- as.numeric(Disease_Data$BD.Status)
Disease_Data$RV.Status <- as.numeric(Disease_Data$RV.Status)

Disease_Data$BD.PA <- ifelse(Disease_Data$BD.Status > 0, 1,0)
Disease_Data$RV.PA <- ifelse(Disease_Data$RV.Status > 0, 1,0)
sum(Disease_Data$BD.PA) #92 positive tests
sum(Disease_Data$RV.PA) #103 positive tests

sum(Disease_Data$BD.PA)/nrow(Disease_Data) #naive prev. of 0.25
sum(Disease_Data$RV.PA)/nrow(Disease_Data) #naive prev. of 0.28
nrow(filter(Disease_Data, BD.PA == '1' & RV.PA == '1'))/nrow(Disease_Data) #naive cooccurrence = 0.11

# I want to match field data to disease data where possible#
#To start, need to clean up disease_data

#remove spaces from disease_data
Disease_Data$Sample.Name <- str_remove(Disease_Data$Sample.Name," ")
field_data$SampleID <- str_remove_all(field_data$SampleID,"-")


#Okay let's do an anti-join to see what is missing/probably mislabelled
colnames(Disease_Data)[1] <- 'SampleID'
missing_names <- anti_join(x = Disease_Data, y = field_data, by = 'SampleID')

#Okay there are 163 samples that don't match the field data
#But, I don't have any controls in the field data so we can start by removing those from the missing data
missing_names <- missing_names %>% filter(!grepl("C",SampleID))

#Okay, i want to check something else. Some of these columns I'm betting just are 
#missing a 0 at the front
missing_zeros <- missing_names %>% filter(nchar(SampleID) == 7 
                                          & !substr(SampleID, 1, 1) == "0")
#Okay, so there are 116 samples that I'm guessing are just missing a 0 at the front. GUESSING#
missing_zeros$SampleID <- paste("0",missing_zeros$SampleID, sep = "")
anti_join(x = missing_zeros, y = field_data, by = 'SampleID')
write.csv(x = missing_names, file = "missnamed.csv")

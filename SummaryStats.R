# Summary Info
# 01/14/2026
# RCS

# library
library(tidyverse)
library(RMark)
# working directories
data <- "C:/Users/rcscott/VTADS/Data"
setwd(data)
# load data
load("occModdata_2022.RData")
load("occModdata_2024.RData")
results_2022 <- read.csv("qPCRresults_2022.csv")
results_2024 <- read.csv("qPCRresults_2024.csv")
load("biodiversityData.RData")
load("PresenceAbsenceData.RData")
BD_mods <- readRDS("BD_mods.RDS")
RV_mods <- readRDS("RV_mods.RDS")


### Summary of biodiversity
# what was the range of alpha
range(Diversity_2022$alpha)
range(Diversity_2024$alpha)
#2-7 species

# What was the range of beta?
range(Diversity_2022$beta)
range(Diversity_2024$beta)
# 0-0.71

colSums(PA_2022[,5:15])
colSums(PA_2024[,5:15])
# How many total samples each year?
Controls <- "Bd\\+FV3|NTC|Blank|Empty"
results_2022 <- results_2022 %>% filter(!str_detect(Sample, Controls)) %>%
  filter(!str_detect(Sample,"c|C"))

LabIDKey <- read.csv("LabIDKey.csv")
LabIDKey <- LabIDKey %>% select(!BD_Rep1)
colnames(LabIDKey)[[1]] <- "Sample"
results_2024 <- left_join(results_2024,LabIDKey, by = "Sample")
results_2024 <- results_2024 %>% filter(!str_detect(Sample, Controls)) %>%
  filter(!str_detect(SampleID,"c|C|NEG"))

nrow(results_2022)/4 # 597
nrow(results_2024)/4 # 624
597+624
# BD
BD_2022_wide$Year <- 2022
BD_2022_wide <- relocate(BD_2022_wide,Year,.before = ComplexID)
BD_2024_wide <- BD_2024_wide %>% select(!SiteID)
# naive occupancy
# 2022
cols.num <- 4:ncol(BD_2022_wide)
BD_2022_wide[cols.num] <- sapply(BD_2022_wide[cols.num],as.numeric)
sapply(BD_2022_wide, class)
BD_2022_wide$Occ <- ifelse(rowSums(BD_2022_wide[,4:ncol(BD_2022_wide)],na.rm = T) > 0, 
                           1,0)
sum(rowSums(BD_2022_wide[,4:ncol(BD_2022_wide)], na.rm = T))/2
sum(BD_2022_wide$Occ)/nrow(BD_2022_wide) 

# Naive occ in 2022 was 0.875

cols.num <- 4:ncol(BD_2024_wide)
BD_2024_wide[cols.num] <- sapply(BD_2024_wide[cols.num],as.numeric)
sapply(BD_2024_wide, class)
BD_2024_wide$Occ <- ifelse(rowSums(BD_2024_wide[,4:ncol(BD_2024_wide)],na.rm = T) > 0, 
                           1,0)
sum(rowSums(BD_2024_wide[,4:ncol(BD_2024_wide)], na.rm = T))/2
sum(BD_2024_wide$Occ)/nrow(BD_2024_wide) 
# Naive prev in 2024 was also 0.875

BD_data <- rbind(BD_2022_wide,BD_2024_wide)

# Naive prevalence
BDOcc <- BD_data %>% 
  pivot_longer(cols = `1_1_1`:`3_12_2`,
               names_to = c("Visit","Frog","Rep"),
               names_pattern = "(\\d+)_(\\d+)_(\\d+)",
               values_to = "PA") %>%
  pivot_wider(names_from = Rep,
              names_prefix = "Rep_",
              values_from =  PA)%>%
  mutate(BD_occ = case_when(Rep_1 == 1 | Rep_2 == 1 ~ 1,
                             Rep_2 == 0 & Rep_2 == 0 ~ 0,
                             is.na(Rep_1) & is.na(Rep_2) ~ NA)) %>%
  select(!c(Rep_1:Rep_2))
sum(BDOcc$BD_occ, na.rm = T)
# 196 individuals tested positive for Bd  

BDPrev <- BDOcc %>%  
  group_by(Year,ComplexID, PondID,Visit) %>%
  dplyr::summarise(BD_prev = sum(BD_occ,na.rm = T)/n())
range(BDPrev$BD_prev) # prevalence ranged between 0-0.58

# RV
RV_2022_wide$Year <- 2022
RV_2022_wide <- relocate(RV_2022_wide,Year,.before = ComplexID)
RV_2024_wide <- RV_2024_wide %>% select(!SiteID)
# naive occupancy
# 2022
cols.num <- 4:ncol(RV_2022_wide)
RV_2022_wide[cols.num] <- sapply(RV_2022_wide[cols.num],as.numeric)
sapply(RV_2022_wide, class)
RV_2022_wide$Occ <- ifelse(rowSums(RV_2022_wide[,4:ncol(RV_2022_wide)],na.rm = T) > 0, 
                           1,0)

sum(rowSums(RV_2022_wide[,4:ncol(RV_2022_wide)], na.rm = T))/2

sum(RV_2022_wide$Occ)/nrow(RV_2022_wide) 
# Naive occ in 2022 was 0.5

cols.num <- 4:ncol(RV_2024_wide)
RV_2024_wide[cols.num] <- sapply(RV_2024_wide[cols.num],as.numeric)
sapply(RV_2024_wide, class)
RV_2024_wide$Occ <- ifelse(rowSums(RV_2024_wide[,4:ncol(RV_2024_wide)],na.rm = T) > 0, 
                           1,0)

sum(rowSums(RV_2024_wide[,4:ncol(RV_2024_wide)], na.rm = T))/2

sum(RV_2024_wide$Occ)/nrow(RV_2024_wide) 
# Naive occ in 2024 was 0.33

colnames(RV_2024_wide) <- str_remove(colnames(RV_2024_wide),"FV3_PA")
RV_data <- rbind(RV_2022_wide,RV_2024_wide)

# Naive prevalence
RVFrogs <- RV_data %>% 
  pivot_longer(cols = `1_1_1`:`3_12_2`,
               names_to = c("Visit","Frog","Rep"),
               names_pattern = "(\\d+)_(\\d+)_(\\d+)",
               values_to = "PA") %>%
  pivot_wider(names_from = Rep,
              names_prefix = "Rep_",
              values_from =  PA)%>%
  mutate(RV_occ = case_when(Rep_1 == 1 | Rep_2 == 1 ~ 1,
                            Rep_2 == 0 & Rep_2 == 0 ~ 0,
                            is.na(Rep_1) & is.na(Rep_2) ~ NA)) %>%
  select(!c(Rep_1:Rep_2))
sum(RVFrogs$RV_occ,na.rm = T)
# 46 individuals tested positive for RV

RVPrev <- RVFrogs %>%
  group_by(Year,ComplexID, PondID,Visit) %>%
  dplyr::summarise(RV_prev = sum(RV_occ,na.rm = T)/n())
range(RVPrev$RV_prev) # prevalence ranged between 0-0.58 (same as BD)


# estimated psi for each pathogen
model.average(BD_mods, parameter = "Psi")
# estimated psi = 0.96 but they had a bunch of models get dropped?
model.average(RV_mods, parameter = "Psi")
# model average = 0.58
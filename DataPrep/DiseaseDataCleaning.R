# Description
# Script for cleaning + organizing disease data so it can be read into occ mod
# RCS
# 10/01/2025

# Load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# set working directories
repo <- "C:/Users/rcscott/VTADS"
data <- "C:/Users/rcscott/VTADS/Data"

# Read in data
setwd(data)
DiseaseData_2022 <- read.csv("qPCRresults_2022.csv")
DiseaseData_2024 <- read.csv("qPCRresults_2024.csv")

# Notes on 2024 data
# There were a few controls that tested positive. I checked curves on each to determine whether they actually amplified
# 04-55.
# 07-37 Doesn't look like it amplified. RFU was upward adjusted on 12/10/2025
# 07-08 Doesn't look like it amplified. RFU was upward adjusted on 12/10/2025
# 02-61 Definitely amplified. This was a field control for 09-02, visit 1
# 03-49 Doesn't look like it amplified. May just need to adjust RFU
# 03-63 Doesn't look like it amplified. May just need to adjust RFU
# 07-07 Doesn't look like it amplified. RFU was upward adjusted on 12/10/2025
# 08-05. Def amplified. This was a field control for 02-01, visit 3
# 10-53. Definitely amplified. This was a field control for 12-02, visit 3
FieldData <- read.csv("SwabDataMaster.csv")
colnames(DiseaseData_2024)[[4]] <- "LabID"
DiseaseData_2024$LabID <- str_remove_all(DiseaseData_2024$LabID, "'")
# DiseaseData$SampleID <- str_remove_all(DiseaseData$SampleID, "-")
# DiseaseData$SampleID <- trimws(DiseaseData$SampleID)
# FieldData$SampleID <- str_remove_all(FieldData$SampleID, "-")
# First need to join data

FieldData2 <- FieldData %>% select(c(ComplexID, 
                                     PondID, 
                                     SiteID, 
                                     Survey.Number, 
                                     CatchOfDay, 
                                     SampleID,
                                     WaterTemp.Nearest,
                                     Species))
FieldData2$SiteID <- str_remove_all(FieldData2$SiteID,"'")
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
LabIDKey <- read.csv("LabIDKey.csv")
LabIDKey$LabID <- str_remove(LabIDKey$LabID,"'")
LabIDKey <- LabIDKey %>% select(!BD_Rep1)
anti_join(LabIDKey, DiseaseData_2024, by = "LabID")
FieldData2 <- FieldData2 %>% select(!Species)

DiseaseData_2024$Target <- ifelse(DiseaseData_2024$Fluor == "FAM", "Bd","FV3")
DiseaseData_2024$Rep <- droplevels(as.factor(DiseaseData_2024$Rep))
levels(DiseaseData_2024$Rep)



Controls <- "Bd\\+FV3|NTC|Blank|Empty"

DiseaseData_2024_wide <- DiseaseData_2024 %>% select(!c(Fluor,Well)) %>%
  filter(!str_detect(LabID,Controls))%>%
  pivot_wider(names_from = c(Target,Rep), values_from = Cq)
# May want to left join before I change to P/A
DiseaseData_2024_wide <- DiseaseData_2024_wide %>%
  left_join(LabIDKey, by = "LabID") %>%
  left_join(FieldData2, by = "SampleID")

# Establishing P/A(detection/nondetection)

# 12/10/2025. Need to continue working on this

cq_threshold <- c(39,37,37,40)
# 39 = Threshold for pond 09-02, visit 1
# 37 = Threshold for pond 02-01 visit 3 and 12-02 visit 3
# 40 = standard threshold for all other ponds


# First making PA for Gilbrook02, visit 1 
Gilbrook02_01 <- DiseaseData_2024_wide %>% filter(SiteID == "09-02" & 
                                                    Survey.Number == "1")

Gilbrook02_01 <- Gilbrook02_01 %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[1]] & Bd_1 > 3 ~ 1,
                                                             Bd_1 >= cq_threshold[[1]] & Bd_1 < 45 ~ NA,
                                                             Bd_1 == 45 ~ 0),
                                          BD_PA2 = case_when(Bd_2 < cq_threshold[[1]] & Bd_2 > 3 ~ 1,
                                                             Bd_2 >= cq_threshold[[1]] & Bd_2 < 45 ~ NA,
                                                             Bd_2 == 45 ~ 0),
                                          FV3_PA1 = case_when(FV3_1 < cq_threshold[[1]] & FV3_1 > 3 ~ 1,
                                                           FV3_1 >= cq_threshold[[1]] & FV3_1 < 45 ~ NA,
                                                           FV3_1 == 45 ~ 0),
                                          FV3_PA2 = case_when(FV3_2 < cq_threshold[[1]] & FV3_2 > 3 ~ 1,
                                                              FV3_2 >= cq_threshold[[1]] & FV3_2 < 45 ~ NA,
                                                              FV3_2 == 45 ~ 0))
# Next making PA for Audobon visit 3
Audobon3 <- DiseaseData_2024_wide %>% filter(SiteID == "02-01" &
                                               Survey.Number == "3")
Audobon3 <- Audobon3 %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[2]] & Bd_1 > 3 ~ 1,
                                                             Bd_1 >= cq_threshold[[2]] & Bd_1 < 45 ~ NA,
                                                             Bd_1 == 45 ~ 0),
                                          BD_PA2 = case_when(Bd_2 < cq_threshold[[2]] & Bd_2 > 3 ~ 1,
                                                             Bd_2 >= cq_threshold[[2]] & Bd_2 < 45 ~ NA,
                                                             Bd_2 == 45 ~ 0),
                                          FV3_PA1 = case_when(FV3_1 < cq_threshold[[2]] & FV3_1 > 3 ~ 1,
                                                              FV3_1 >= cq_threshold[[2]] & FV3_1 < 45 ~ NA,
                                                              FV3_1 == 45 ~ 0),
                                          FV3_PA2 = case_when(FV3_2 < cq_threshold[[2]] & FV3_2 > 3 ~ 1,
                                                              FV3_2 >= cq_threshold[[2]] & FV3_2 < 45 ~ NA,
                                                              FV3_2 == 45 ~ 0))

# Makeing PA for IndianBrook 02 visit 3
IndianBrook3 <- DiseaseData_2024_wide %>% filter(SiteID == "12-02" &
                                                   Survey.Number == "3")
IndianBrook3 <- IndianBrook3 %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[3]] & Bd_1 > 3 ~ 1,
                                                   Bd_1 >= cq_threshold[[3]] & Bd_1 < 45 ~ NA,
                                                   Bd_1 == 45 ~ 0),
                                BD_PA2 = case_when(Bd_2 < cq_threshold[[3]] & Bd_2 > 3 ~ 1,
                                                   Bd_2 >= cq_threshold[[3]] & Bd_2 < 45 ~ NA,
                                                   Bd_2 == 45 ~ 0),
                                FV3_PA1 = case_when(FV3_1 < cq_threshold[[3]] & FV3_1 > 3 ~ 1,
                                                    FV3_1 >= cq_threshold[[3]] & FV3_1 < 45 ~ NA,
                                                    FV3_1 == 45 ~ 0),
                                FV3_PA2 = case_when(FV3_2 < cq_threshold[[3]] & FV3_2 > 3 ~ 1,
                                                    FV3_2 >= cq_threshold[[3]] & FV3_2 < 45 ~ NA,
                                                    FV3_2 == 45 ~ 0))

# Now need to make a filter for all other samples
GoodSamples <- DiseaseData_2024_wide %>% 
  anti_join(Gilbrook02_01, by = "LabID") %>%
  anti_join(Audobon3, by = "LabID") %>%
  anti_join(IndianBrook3, by = "LabID")
FieldControls <- GoodSamples %>% filter(str_detect(SampleID,"C|NEG"))
GoodSamples <- GoodSamples %>% filter(!str_detect(SampleID,"C|NEG"))
GoodSamples <- GoodSamples %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[4]] & Bd_1 > 3 ~ 1,
                                                         Bd_1 >= cq_threshold[[4]] & Bd_1 < 45 ~ NA,
                                                         Bd_1 == 45 ~ 0),
                                      BD_PA2 = case_when(Bd_2 < cq_threshold[[4]] & Bd_2 > 3 ~ 1,
                                                         Bd_2 >= cq_threshold[[4]] & Bd_2 < 45 ~ NA,
                                                         Bd_2 == 45 ~ 0),
                                      FV3_PA1 = case_when(FV3_1 < cq_threshold[[4]] & FV3_1 > 3 ~ 1,
                                                          FV3_1 >= cq_threshold[[4]] & FV3_1 < 45 ~ NA,
                                                          FV3_1 == 45 ~ 0),
                                      FV3_PA2 = case_when(FV3_2 < cq_threshold[[4]] & FV3_2 > 3 ~ 1,
                                                          FV3_2 >= cq_threshold[[4]] & FV3_2 < 45 ~ NA,
                                                          FV3_2 == 45 ~ 0))

DiseaseData2_2024 <- rbind(GoodSamples,
                           Audobon3,
                           Gilbrook02_01,
                           IndianBrook3)
nrow(DiseaseData2_2024) + nrow(FieldControls)

# #naive prevalence
BdPrev <- DiseaseData2_2024 %>%
  mutate(Bd_occ = case_when(BD_PA1 == 1 | BD_PA2 == 1 ~ 1,
                            BD_PA2 == 0 & BD_PA2 == 0 ~ 0,
                            is.na(BD_PA2) & is.na(BD_PA2) ~ NA)) %>%
  select(!c(LabID:SampleID,BD_PA1:FV3_PA2)) %>%
  group_by(SiteID,Survey.Number) %>%
  dplyr::summarise(BD_prev = sum(Bd_occ,na.rm = T)/n(),water.temp = mean(as.numeric(WaterTemp.Nearest),
                                                                         na.rm = T))
range(BdPrev$BD_prev) # ranges 0-0.5
ggplot(BdPrev, aes(x = Survey.Number, y = BD_prev, colour = SiteID))+
  geom_point()+
  geom_line()
ggplot(data = BdPrev, aes(x = water.temp, y = BD_prev))+
  geom_point()

# Cool! I have all the data together.

# #Need to do a quick check of all the controls to make sure they're negative
# Negs2024 <- DiseaseData2_2024 %>% filter(SampleID == "NEG")
# rowSums(Negs2024[,2:5]) 
# # Looks like there are 3 cases where the field control may have tested positive
# Controls2024 <- DiseaseData2_2024 %>% filter(str_detect(SampleID,"C"))
# rowSums(Controls2024[,2:5]) 
# # Looks like there are 6 cases where the field control may have tested positive
# 
# #Samples to check curves on
# # 04-55.
# # 07-37 Doesn't look like it amplified. May just need to adjust RFU
# # 07-08 Doesn't look like it amplified. May just need to adjust RFU
# # 02-61 Definitely amplified
# # 03-49 Doesn't look like it amplified. May just need to adjust RFU
# # 03-63 Doesn't look like it amplified. May just need to adjust RFU
# # 07-07 Doesn't look like it amplified. May just need to adjust RFU
# # 08-05. Def amplified
# # 10-53. Definitely amplified

#Not all amphibibians were tested, so need to setup a lab numbering system
Complex <- 1:max(DiseaseData2_2024$ComplexID)
Pond <- 1:max(DiseaseData2_2024$PondID)
Visit <- 1:max(DiseaseData2_2024$Survey.Number)
DiseaseData2_2024$LabNumber <- NA
for (i in 1:length(Complex)) {
  for (j in 1:length(Pond)) {
    for (k in 1:length(Visit)) {
      df <- DiseaseData2_2024 %>% filter(ComplexID == as.character(i) &
                                      PondID == as.character(j) &
                                      Survey.Number == as.character(k))
      if(nrow(df) == 0) next
      df <- df %>% arrange(CatchOfDay)
      df$LabNumber <- nrow(df):1
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
BD_2024 <- DiseaseData2_2024 %>% select(!c(LabID:SampleID,WaterTemp.Nearest, CatchOfDay, FV3_PA1:FV3_PA2))
BD_2024_longer <- BD_2024 %>% pivot_longer(cols = BD_PA1:BD_PA2,names_to = "Rep", values_to = "PA")
BD_2024_longer$Rep <- str_remove(BD_2024_longer$Rep, "BD_PA")
BD_2024_wide <- BD_2024_longer %>% 
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


# DummyData_0801 <- c("8","1",rep(NA, times = ncol(BD_2024_wide)-2))
# BD_2024_wide <- rbind(BD_2024_wide, DummyData_0801)
BD_2024_wide <- arrange(BD_2024_wide,as.numeric(ComplexID),as.numeric(PondID))

### Getting naive occupancy of Bd
# BD_2024_wide$Occ <- ifelse(rowSums(BD_2024_wide[,4:ncol(BD_2024_wide)],na.rm = T) > 0, 
#                            1,0)
# sum(BD_2024_wide$Occ)/nrow(BD_2024_wide) # naive occupancy of 0.875


#RV
RV_2024 <- DiseaseData2_2024 %>% select(!c(LabID:SampleID,WaterTemp.Nearest, CatchOfDay, BD_PA1:BD_PA2))
RV_2024_longer <- RV_2024 %>% pivot_longer(cols = FV3_PA1:FV3_PA2,names_to = "Rep", values_to = "PA")
RV_2024_longer$Rep <- str_remove(RV_2024_longer$Rep, "FV3_PA")
RV_2024_wide <- RV_2024_longer %>% 
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


# DummyData_0801 <- c("8","1",rep(NA, times = ncol(RV_2024_wide)-2))
# RV_2024_wide <- rbind(RV_2024_wide, DummyData_0801)
# RV_2024_wide <- arrange(RV_2024_wide,as.numeric(ComplexID),as.numeric(PondID))

### Getting naive occupancy of FV3
RV_2024_wide$Occ <- ifelse(rowSums(RV_2024_wide[,4:ncol(RV_2024_wide)],na.rm = T) > 0, 
                            1,0)
sum(RV_2024_wide$Occ)/nrow(RV_2024_wide) # naive occupancy of 0.333

#naive prevalence
RVPrev <- DiseaseData2_2024 %>% 
  mutate(FV3_occ = case_when(FV3_PA1 == 1 | FV3_PA2 == 1 ~ 1,
                            FV3_PA2 == 0 & FV3_PA2 == 0 ~ 0,
                            is.na(FV3_PA2) & is.na(FV3_PA2) ~ NA)) %>%
  select(!c(LabID:SampleID,FV3_PA1:FV3_PA2)) %>% 
  group_by(SiteID, Survey.Number) %>%
  dplyr::summarise(FV3_prev = sum(FV3_occ,na.rm = T)/n())
range(RVPrev$FV3_prev)
ggplot(RVPrev, aes(x = Survey.Number, y = FV3_prev, colour = SiteID))+
  geom_point()+
  geom_line()+
  theme_classic()



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

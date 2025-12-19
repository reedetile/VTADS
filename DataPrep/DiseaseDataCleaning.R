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
# 02-61 Definitely amplified. This was a field control for 09-"02", visit 1
# 03-49 Doesn't look like it amplified. May just need to adjust RFU
# 03-63 Doesn't look like it amplified. May just need to adjust RFU
# 07-07 Doesn't look like it amplified. RFU was upward adjusted on 12/10/2025
# 08-05. Def amplified. This was a field control for 02-"01", visit 3
# 10-53. Definitely amplified. This was a field control for 12-"02", visit 3
FieldData <- read.csv("SwabDataMaster.csv")
colnames(DiseaseData_2024)[[4]] <- "LabID"
DiseaseData_2024$LabID <- str_remove_all(DiseaseData_2024$LabID, "'")
DiseaseData_2022$Sample <- str_remove_all(DiseaseData_2022$Sample, " ")
# DiseaseData$SampleID <- str_remove_all(DiseaseData$SampleID, "-")
# DiseaseData$SampleID <- trimws(DiseaseData$SampleID)
# FieldData$SampleID <- str_remove_all(FieldData$SampleID, "-")
# First need to join data

FieldData2 <- FieldData %>% select(c(Year,
                                     ComplexID, 
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
FieldData_2022 <- FieldData2 %>% filter(Year == "2022")
FieldData_2022$SampleID <- str_remove_all(FieldData_2022$SampleID,pattern = "-")

#Need to pivot wider the disease data so I can know how may samples I actually have
Controls <- "Bd\\+FV3|NTC|Blank|Empty"
DiseaseData_2022$Target <- ifelse(DiseaseData_2022$Fluor == "FAM","Bd","FV3")
DiseaseData_2022$Sample <- str_replace_all(DiseaseData_2022$Sample, pattern = "c",replacement = "C")


Duplicates_2022 <- DiseaseData_2022 |>
  dplyr::summarise(n = dplyr::n(), .by = c(Sample, Target, Rep)) |>
  dplyr::filter(n > 1L) 

Dups <- unique(Duplicates_2022$Sample)
DiseaseData_2022_noDups <- DiseaseData_2022 %>%
  filter(!(Sample %in% Dups))
  # filter(!(Sample %in% Dups & str_detect(Source.Name,"20251204-3|20251208-1|20251209-1|20251209-2|20251211-1"))) %>%
  # filter(!(Sample == "12030102" & (Well == "E3" | Well == "E4"))) %>%
  # filter(!(Sample == "08010301" & (Well == "E3" | Well == "E4"))) %>%
  # filter(!str_detect(Sample,"'|Bd\\+FV3|NTC|Blank|Empty| "))

Dups_1212_2 <- DiseaseData_2022 %>%
  filter(Sample == "08010301" & str_detect(Well, "E"))

ReRuns_1212_1 <- DiseaseData_2022 %>%
  filter(Sample %in% Dups & str_detect(Source.Name,"20251212-1"))%>%
  filter(!(Sample == "12030102" & str_detect(Well, "E")))

ReRuns_1210_1 <- DiseaseData_2022 %>%
  filter(Sample %in% Dups & 
           Source.Name == "20251210-1.csv" &
           !(Well == "D09" | Well == "D10"))

ReRuns_1209_2 <- DiseaseData_2022%>%
  filter(Sample %in% Dups & !(Sample %in% unique(ReRuns_1210_1$Sample))&
           str_detect(Source.Name,"20251209-2"))

ReRuns_1208_2 <- DiseaseData_2022 %>%
  filter(Sample %in% Dups & !(Sample %in% unique(ReRuns_1210_1$Sample)) &
           str_detect(Source.Name,"20251208-2"))

ReRuns_1208_1 <- DiseaseData_2022 %>%
  filter(Sample %in% Dups & !(Sample %in% unique(ReRuns_1210_1$Sample)) &
           str_detect(Source.Name, "20251208-1"))




DiseaseData_2022_AllSamples <- rbind(DiseaseData_2022_noDups,
                                     Dups_1212_2,
                                     ReRuns_1212_1,
                                     ReRuns_1210_1,
                                     ReRuns_1209_2,
                                     ReRuns_1208_2,
                                     ReRuns_1208_1)

DiseaseData_2022_wider <- DiseaseData_2022_AllSamples %>% 
  select(!c(Source.Name,Fluor,Well)) %>% 
  filter(!(str_detect(Sample,"'|Bd\\+FV3|NTC|Blank|Empty") | Sample == "")) %>%
  #filter(!(Sample %in% Dups)) %>%
  pivot_wider(names_from = c(Target,Rep), values_from = Cq)

# I have 557 rows, but should have "612", with missing two boxes. What's wrong?
SamplesList <- c("03010209","03010202","03010205","03010208","03010204","030102010","03010212","03010211","03010207","03010201","03010206","03010203",
                 "0301028","09020207","09020208","09010204","09020215","09020209","09010201","09010203","09020205","09020210","09020214","0902012",
                 "0902013","09020211","09020206","09020202","09030206","09030201","09030210","0903020","09030207","09030202","09030212","09030213",
                 "09030211","09030209","09030215","09030216","09030214","09030204","09030205","09030203","09030208","09030102","09030103","09030105",
                 "09030106","09030104","090301C","09030101","09020102","20102010/30102030/20102060","09020104","09020105","09020101","09020112","09020110","09020107",
                 "09020103","09020108","09020111","09020106","090201C","10030106","10030111","10030109","10030104","10030103","10030108","10030110",
                 "01030107","10030101","10030112","10030105","100301C","10030102","10010110","10010112","10010109","10010102","10010105","10010108",
                 "10010101","10010106","10010107","1001010C","10010104","10010111","10010103","10020108","10020110","10020107","10020109","100201C",
                 "10020111","10020105","10020106","10020101","10020112","10020103","10020102","10020104","08020102","08020106","08020111","08020104",
                 "08020108","080201C","0802010","08020101","08020112","08020107","08020103","08020105","08020109","08020107","080101C","08010104",
                 "08010106","08010103","08010102","08010101","08010105","05010302","05020305","05020307","05020302","05020308","05020301","08020310",
                 "05020311","050103C","05010301","05020303","05020306","050203C","05020304","05020309","07010111","07010101","07010105","07010102",
                 "070101C","07010108","07010113","07010104","07010109","07010103","0701012","07010110","07010107","07010106","07020110","07020108",
                 "07020111","07020102","070201C","07020104","07020109","07020112","07020103","07020106","07020105","07020101","120203C","10020311",
                 "07020107","10020306","10020309","10020305","10020302","10020310","10020304","10020303","10020308","10020301","10020312","10020307",
                 "10010302","10010305","10010310","10010301","10010308","10010303","10010304","10010312","10010311","10013C","10010309","10010306",
                 "10010307","10030301","10030307","10030303","10030311","10030312","10030304","10030302","10030308","10030C","10030309","10030306",
                 "1003010","10030305","09030306","09030313","09030305","09030302","09030304","09030309","09030315","09030310","09030301","09030311",
                 "09030307","090308","090303C","09030303","09030312","09030314","09010302","09010311","09010303","09010301","09010306","09010304",
                 "09010308","09010312","09010307","09010305","09010309","09010310","090103C","09020309","09020308","090203C","09020311","09020306",
                 "09020304","09020305","09020303","09020310","09020312","09020302","09020301","11010306","11010301","11010302","11010307","11010311",
                 "11010305","11010303","11010304","11010312","110103C","11010310","11010309","11010308","11090312","11040306","11040307","11040305",
                 "110403C","11040304","11040303","11040308","11040309","11040301","11040310","11040311","11040302","11020303","11020302","11020307",
                 "11020312","11020310","11020305","11020301","11020304","11020308","11020311","11020309","11020306","110203C","06010310","06010309",
                 "09020307","06010312","06010305","06010307","06010311","06010306","06010303","06010304","06010301","06010302","060103C","06010308",
                 "06020301","06020310","06020302","06020304","06020312","06020307","06020309","06020306","06020305","060203C","06020303","06020311",
                 "06020308","02010313","02010304","02010301","02010307","02010305","02010306","02010309","02010312","02010302","020103C","02010303",
                 "02010310","02010311","02010304","02010308","04010307","04010301","04010306","04010312","04010308","040103C","04010310","04010304",
                 "04010305","04010309","04010311","04010302","04010303","03010304","03010308","03010310","03010307","03010312","03010301","03010311",
                 "03010309","03010305","03010306","03010302","03010303","030103C","12020301","12020302","12020309","12020307","12020311","12020305",
                 "120203C","12020308","12020312","12020310","12020304","12020306","12030302","12030304","12030313","12030307","12030305","12030311",
                 "12030306","12030312","12030301","120303C","12030309","12030303","12030308","12030310","12010301","12010302","12010309","12010311",
                 "07020310","07020302","07020309","07020307","07020305","07020301","07020303","07020312","070203C","12020303","07020311","07020306",
                 "12010305","07020308","07020304","120103C","12010307","12010312","12010303","12010304","12010306","12030108","12010310","07010308",
                 "070103C","07010310","07010302","07010304","07010312","07010301","07010311","07010307","07010305","07010303","07010309","07010306",
                 "01010307","01010302","01010306","01010309","01010312","01010304","010103C","01010305","01010303","01010308","01010301","01010311",
                 "12010206","12010209","12010204","01010310","12010211","120102C","12010203","12010207","12010202","12010205","12010212","12010201",
                 "12010210","120202C","12020201","12020202","12020210","12020208","12020206","12020204","12020205","12020207","12020203","12020211",
                 "12020212","12030211","12030202","12030201","12030209","120302C","12030203","12030205","12030204","12030210","12030206","12030212",
                 "12030208","12030207","080102C","08010202","08010203","08010208","08010204","08010206","08010207","08010205","08010201","080202C",
                 "06010105","06010106","06020105","10020205","10020206","100302C","10010203","10010202","12010104","1203010","12030106",
                 "06010112","06010101","06020103","10020201","10030206","10030209","10010212","10010211","12010103","12030102","12030109",
                 "060101C","06020107","06020102","10020210","10030202","10030211","10010204","100102C","12010105","12030105","12030111",
                 "06010108","06020109","06020106","10020204","10030212","10030204","10010201","12010111","120101C","12030108","1203011C",
                 "06010107","06020108","10020203","10020212","10030210","10030208","10010208","12010110","12010112","12030102","12030112",
                 "06010110","06010103","06020110","10020209","100202C","10030207","10030201","10010206","12010102","12010101","12030103","12030101",
                 "06010109","06010102","06020101","100202011","10020202","10030203","10010205","10010209","12010106","12010108","12030113","12020109",
                 "06010104","06010111","06020104","10020208","10020207","10030205","10010210","10010207","12010109","12010107","12030104","12020103",
                 "08020310","08020307","08010302","08010309", "08020303","08020311","080306","08010308", "11040202","08020308","08010301","08010311",
                 "11040211","08020302","08010312","08010307","11040212","11040204","08020309","08010305","08010301","11040213","11040210","08020301",
                 "08020304","0801030C","110402C","11040207","08020306","080203C","08010303","11040203","11040209","08020305","08020312","08010304")

length(unique(SamplesList)) #There should be 600 unique samples
length(unique(SamplesList)) - nrow(DiseaseData_2022_wider) #want this to be 0
MissingSamps <- setdiff(SamplesList,DiseaseData_2022_wider$Sample)
MissingSamps
# 10010109 was not run
# 10020110 was not run
# Otherwise looks all good

Bd_1_df <- DiseaseData_2022_wider %>%
  filter(Bd_1 < 45 & Bd_1 >= 39) %>%
  select(!FV3_1:FV3_2)
# Checked the curves for any samples where 39 <= Rep 1 <45 and rep 2 was 40-45. All looked good


Bd_2_df <- DiseaseData_2022_wider %>%
  filter(Bd_2 < 45 & Bd_2 >= 39 & !(Sample %in% Bd_1_df$Sample)) %>%
  select(!FV3_1:FV3_2)
# Checked the curves for any samples where 39 <= Rep 2 <45 and rep 1 was 40-45. All looked good

FV3_1_df <- DiseaseData_2022_wider %>%
  filter(FV3_1 < 45 & FV3_1 >= 39) %>%
  select(!Bd_1:Bd_2)
# Checked the curves for any samples where 39 <= Rep 1 <45 and rep 2 was 40-45. All looked good


FV3_2_df <- DiseaseData_2022_wider %>%
  filter(FV3_2 < 45 & FV3_2 >= 39 & !(Sample %in% FV3_1_df$Sample)) %>%
  select(!Bd_1:Bd_2)
# Checked the curves for any samples where 39 <= Rep 2 <45 and rep 1 was 40-45. All looked good

#Combine Field data to disease results
names(DiseaseData_2022_wider)[[1]] <- "SampleID"
DiseaseData_2022_Full <- DiseaseData_2022_wider %>% left_join(FieldData_2022, by = "SampleID")
#Stopped here 12/18/2025. Need to explore what samples are missing field data, which may inform which samples are mislabelled.

Mislabelled_lab_2022 <- DiseaseData_2022_Full %>% filter(!str_detect(SampleID, "c|C")) %>% anti_join(FieldData_2022, by = "SampleID")
# samples that were mislabelled in the lab processes
Lost <- c("07020207","07020209","04010209","04010213","08020210","05020203","11010208","11010207","110202C",	"11020204","06010207","06020206",
          "07020210","07020204","04010201","04010210","08020203","050202C","11010205","11010204","11020205","06010204","06010208","06020209",
          "07020202","07020203","04010206","04010202","08020206","05020202","11010210","11010202","11020203","06010210","06010206","06020203",
          "07020211","07020206","04010208","08020204","08020207","05020201","11010206","11010209","11020202","06010205","06010209","06020212",
          "07020201","07020205","04010207","08020208","08020209","050102C","11010201","11020212","11020208","06010213","060102C","06020211",
          "07020208","04010204","04010212","08020212","08020201","05010202","110102C","11020210","11020207","06010211","06010202","06020202",
          "07020212","04010211","04010203","08020202","12020209","05010201","11010211","11020206","11020209","06010201","06010212","06020205",
          "070202C","040102C","04020105","08020211","12010208","11010203","11010212","11020211","11020201","06010203","06020210","06020208",
          "03010108","030101C","050201C","05010102","02010106","04010110","04010109","02010213","02010214","12020106","01010208","11010105",
          "03010111","03010105","05020105","02010112","02010111","04010104","040101C","02010208","02010211","12020107","01010203","11010101",
          "03010109","03010106","05010102","02010105","02010104","04010102","04010108","02010207","120201C","12020104","010102C","11010102",
          "03010107","03010110","08010104","02010101","02010102","04010106","02010204","02010209","12020108","01010209","01010212","11010107",
          "03010102","06020207","05010103","02010110","02010109","04010103","02010203","020102C","12020105","01010205","01010210","11010108",
          "03010101","06020204","05020101","02010108","020101C","04010107","02010210","02010201","12020102","01010202","01010201","110101C",
          "03010103","060202C","05010101","02010107","04010101","04010112","02010202","02010205","12020110","01010206","01010207","1101010C",
          "03010104","06020201","050101C","02010103","04010111","04010105","02010212","02010206","12020101","01010211","01010204","11010103"
          
)


NoControls_2022 <- DiseaseData_2022_Full %>% filter(!str_detect(SampleID, "c|C")) #just making it so I can search 2022 w/o controls
Missing_Field_2022 <- FieldData_2022 %>% filter(!(SampleID %in% Lost)) %>% anti_join(NoControls_2022, by = "SampleID")
# Data from field that are missing corresponding disease data

Missing_Field_2022 <- Missing_Field_2022 %>% arrange(SiteID,Survey.Number,CatchOfDay)


#SampleID 0301028 is a mislabel
Missing_Field_2022 %>% filter(str_detect(SampleID,"030102"))
DiseaseData_2022_Full %>% filter(str_detect(SampleID,"030102")) %>% arrange(CatchOfDay)
# SampleID 0301028 could be 03010210 as it is missing

# SampleID 01030107 is a mislabel?
Missing_Field_2022 %>% filter(str_detect(SampleID,"0301"))
# I'm pretty confident this is 10030107

# SampleID 09010203 is a mislabel
Missing_Field_2022 %>% filter(str_detect(SampleID, "0902"))
# I'm fairly confident this is 09020203

# SampleID 0903020 is a mislabel
Missing_Field_2022 %>% filter(str_detect(SampleID,"0903"))
# This could be 09030308 (only one missing from 09-03)
######################################################


# Need to check field and extraction controls
FieldControls2022 <- filter(DiseaseData_2022_wider, str_detect(Sample, "c|C") &
                              (Bd_1 < 45 | Bd_2 < 45 | FV3_1 < 45 | FV3_2 < 45))
# 050103C def amplified. Will need to adjust cq threshold for that visit, probably to 36
# 10013C Doesn't look like it actually amplified and can probs just be coded as 0
# 120303C def amplified. Will need to adjust cq threshold for that visit, probably to 38
# 070203C def amplified. Will need to adjust cq threshold for that visit, probably to 35
# 010103C Doesn't look like it actually amplified and can probs just be coded as 0
# 120302C Doesn't look like it actually amplified and can probs just be coded as 0




# Change cQ to PA
cq_threshold_2022 <- c(36,38,35)




# # Remove negs and controls
# DiseaseData2_2022 <- DiseaseData2_2022 %>% filter(!is.na(SiteID))
# # DiseaseData2_2022$CatchOfDay <- sprintf("%02d",DiseaseData2_2022$CatchOfDay) #to make everything 2 digits.
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
DiseaseData2_2022 <- filter(DiseaseData2_"2022", LabNumber < 13) #Just gonna remove the 2 cases where we caught a 13th gf

#Now I need to pivot it in a way that it can be read by an occ mod in unmarked
#BD
BD_2022 <- DiseaseData2_2022 %>% select(!c(RV_Rep1:RV_Rep"2",SiteID,WaterTemp.Nearest, CatchOfDay,Species))
BD_2022_longer <- BD_2022 %>% pivot_longer(cols = BD_Rep1:BD_Rep"2",names_to = "Rep", values_to = "PA")
BD_2022_longer$Rep <- str_remove(BD_2022_longer$Rep, "BD_")
BD_2022_wide <- BD_2022_longer %>% 
  select(!SampleID) %>%
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)
BD_2022_wide <- arrange(BD_2022_wide,as.numeric(ComplexID),as.numeric(PondID))
#RV
RV_2022 <- DiseaseData2_2022 %>% select(!c(BD_Rep1:BD_Rep"2",SiteID,WaterTemp.Nearest, CatchOfDay,Species))
RV_2022_longer <- RV_2022 %>% pivot_longer(cols = RV_Rep1:RV_Rep"2",names_to = "Rep", values_to = "PA")
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
anti_join(LabIDKey, DiseaseData_"2024", by = "LabID")
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
  left_join(FieldData"2", by = "SampleID")

# Establishing P/A(detection/nondetection)

# 12/10/2025. Need to continue working on this

cq_threshold <- c(39,37,37,40)
# 39 = Threshold for pond 09-"02", visit 1
# 37 = Threshold for pond 02-01 visit 3 and 12-02 visit 3
# 40 = standard threshold for all other ponds


# First making PA for Gilbrook"02", visit 1 
Gilbrook02_01 <- DiseaseData_2024_wide %>% filter(SiteID == "09-02" & 
                                                    Survey.Number == "1")

Gilbrook02_01 <- Gilbrook02_01 %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[1]] & Bd_1 > 3 ~ "1",
                                                             Bd_1 >= cq_threshold[[1]] & Bd_1 < 45 ~ NA,
                                                             Bd_1 == 45 ~ 0),
                                          BD_PA2 = case_when(Bd_2 < cq_threshold[[1]] & Bd_2 > 3 ~ "1",
                                                             Bd_2 >= cq_threshold[[1]] & Bd_2 < 45 ~ NA,
                                                             Bd_2 == 45 ~ 0),
                                          FV3_PA1 = case_when(FV3_1 < cq_threshold[[1]] & FV3_1 > 3 ~ "1",
                                                           FV3_1 >= cq_threshold[[1]] & FV3_1 < 45 ~ NA,
                                                           FV3_1 == 45 ~ 0),
                                          FV3_PA2 = case_when(FV3_2 < cq_threshold[[1]] & FV3_2 > 3 ~ "1",
                                                              FV3_2 >= cq_threshold[[1]] & FV3_2 < 45 ~ NA,
                                                              FV3_2 == 45 ~ 0))
# Next making PA for Audobon visit 3
Audobon3 <- DiseaseData_2024_wide %>% filter(SiteID == "02-01" &
                                               Survey.Number == "3")
Audobon3 <- Audobon3 %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[2]] & Bd_1 > 3 ~ "1",
                                                             Bd_1 >= cq_threshold[[2]] & Bd_1 < 45 ~ NA,
                                                             Bd_1 == 45 ~ 0),
                                          BD_PA2 = case_when(Bd_2 < cq_threshold[[2]] & Bd_2 > 3 ~ "1",
                                                             Bd_2 >= cq_threshold[[2]] & Bd_2 < 45 ~ NA,
                                                             Bd_2 == 45 ~ 0),
                                          FV3_PA1 = case_when(FV3_1 < cq_threshold[[2]] & FV3_1 > 3 ~ "1",
                                                              FV3_1 >= cq_threshold[[2]] & FV3_1 < 45 ~ NA,
                                                              FV3_1 == 45 ~ 0),
                                          FV3_PA2 = case_when(FV3_2 < cq_threshold[[2]] & FV3_2 > 3 ~ "1",
                                                              FV3_2 >= cq_threshold[[2]] & FV3_2 < 45 ~ NA,
                                                              FV3_2 == 45 ~ 0))

# Makeing PA for IndianBrook 02 visit 3
IndianBrook3 <- DiseaseData_2024_wide %>% filter(SiteID == "12-02" &
                                                   Survey.Number == "3")
IndianBrook3 <- IndianBrook3 %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[3]] & Bd_1 > 3 ~ "1",
                                                   Bd_1 >= cq_threshold[[3]] & Bd_1 < 45 ~ NA,
                                                   Bd_1 == 45 ~ 0),
                                BD_PA2 = case_when(Bd_2 < cq_threshold[[3]] & Bd_2 > 3 ~ "1",
                                                   Bd_2 >= cq_threshold[[3]] & Bd_2 < 45 ~ NA,
                                                   Bd_2 == 45 ~ 0),
                                FV3_PA1 = case_when(FV3_1 < cq_threshold[[3]] & FV3_1 > 3 ~ "1",
                                                    FV3_1 >= cq_threshold[[3]] & FV3_1 < 45 ~ NA,
                                                    FV3_1 == 45 ~ 0),
                                FV3_PA2 = case_when(FV3_2 < cq_threshold[[3]] & FV3_2 > 3 ~ "1",
                                                    FV3_2 >= cq_threshold[[3]] & FV3_2 < 45 ~ NA,
                                                    FV3_2 == 45 ~ 0))

# Now need to make a filter for all other samples
GoodSamples <- DiseaseData_2024_wide %>% 
  anti_join(Gilbrook02_"01", by = "LabID") %>%
  anti_join(Audobon"3", by = "LabID") %>%
  anti_join(IndianBrook"3", by = "LabID")
FieldControls <- GoodSamples %>% filter(str_detect(SampleID,"C|NEG"))
GoodSamples <- GoodSamples %>% filter(!str_detect(SampleID,"C|NEG"))
GoodSamples <- GoodSamples %>% mutate(BD_PA1 = case_when(Bd_1 < cq_threshold[[4]] & Bd_1 > 3 ~ "1",
                                                         Bd_1 >= cq_threshold[[4]] & Bd_1 < 45 ~ NA,
                                                         Bd_1 == 45 ~ 0),
                                      BD_PA2 = case_when(Bd_2 < cq_threshold[[4]] & Bd_2 > 3 ~ "1",
                                                         Bd_2 >= cq_threshold[[4]] & Bd_2 < 45 ~ NA,
                                                         Bd_2 == 45 ~ 0),
                                      FV3_PA1 = case_when(FV3_1 < cq_threshold[[4]] & FV3_1 > 3 ~ "1",
                                                          FV3_1 >= cq_threshold[[4]] & FV3_1 < 45 ~ NA,
                                                          FV3_1 == 45 ~ 0),
                                      FV3_PA2 = case_when(FV3_2 < cq_threshold[[4]] & FV3_2 > 3 ~ "1",
                                                          FV3_2 >= cq_threshold[[4]] & FV3_2 < 45 ~ NA,
                                                          FV3_2 == 45 ~ 0))

DiseaseData2_2024 <- rbind(GoodSamples,
                           Audobon"3",
                           Gilbrook02_"01",
                           IndianBrook3)
nrow(DiseaseData2_2024) + nrow(FieldControls)

# #naive prevalence
BdPrev <- DiseaseData2_2024 %>%
  mutate(Bd_occ = case_when(BD_PA1 == 1 | BD_PA2 == 1 ~ "1",
                            BD_PA2 == 0 & BD_PA2 == 0 ~ "0",
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
DiseaseData2_2024 <- filter(DiseaseData2_"2024", LabNumber <= 12) #Just gonna remove the 2 cases where we caught a 13th gf

#Now I need to pivot it in a way that it can be read by an occ mod in unmarked
#BD
BD_2024 <- DiseaseData2_2024 %>% select(!c(LabID:SampleID,WaterTemp.Nearest, CatchOfDay, FV3_PA1:FV3_PA2))
BD_2024_longer <- BD_2024 %>% pivot_longer(cols = BD_PA1:BD_PA"2",names_to = "Rep", values_to = "PA")
BD_2024_longer$Rep <- str_remove(BD_2024_longer$Rep, "BD_PA")
BD_2024_wide <- BD_2024_longer %>% 
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


# DummyData_0801 <- c("8","1",rep(NA, times = ncol(BD_2024_wide)-2))
# BD_2024_wide <- rbind(BD_2024_wide, DummyData_0801)
BD_2024_wide <- arrange(BD_2024_wide,as.numeric(ComplexID),as.numeric(PondID))

### Getting naive occupancy of Bd
# BD_2024_wide$Occ <- ifelse(rowSums(BD_2024_wide[,4:ncol(BD_2024_wide)],na.rm = T) > "0", 
#                            "1",0)
# sum(BD_2024_wide$Occ)/nrow(BD_2024_wide) # naive occupancy of 0.875


#RV
RV_2024 <- DiseaseData2_2024 %>% select(!c(LabID:SampleID,WaterTemp.Nearest, CatchOfDay, BD_PA1:BD_PA2))
RV_2024_longer <- RV_2024 %>% pivot_longer(cols = FV3_PA1:FV3_PA"2",names_to = "Rep", values_to = "PA")
RV_2024_longer$Rep <- str_remove(RV_2024_longer$Rep, "FV3_PA")
RV_2024_wide <- RV_2024_longer %>% 
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


# DummyData_0801 <- c("8","1",rep(NA, times = ncol(RV_2024_wide)-2))
# RV_2024_wide <- rbind(RV_2024_wide, DummyData_0801)
# RV_2024_wide <- arrange(RV_2024_wide,as.numeric(ComplexID),as.numeric(PondID))

### Getting naive occupancy of FV3
RV_2024_wide$Occ <- ifelse(rowSums(RV_2024_wide[,4:ncol(RV_2024_wide)],na.rm = T) > "0", 
                            "1",0)
sum(RV_2024_wide$Occ)/nrow(RV_2024_wide) # naive occupancy of 0.333

#naive prevalence
RVPrev <- DiseaseData2_2024 %>% 
  mutate(FV3_occ = case_when(FV3_PA1 == 1 | FV3_PA2 == 1 ~ "1",
                            FV3_PA2 == 0 & FV3_PA2 == 0 ~ "0",
                            is.na(FV3_PA2) & is.na(FV3_PA2) ~ NA)) %>%
  select(!c(LabID:SampleID,FV3_PA1:FV3_PA2)) %>% 
  group_by(SiteID, Survey.Number) %>%
  dplyr::summarise(FV3_prev = sum(FV3_occ,na.rm = T)/n())
range(RVPrev$FV3_prev)
ggplot(RVPrev, aes(x = Survey.Number, y = FV3_prev, colour = SiteID))+
  geom_point()+
  geom_line()+
  theme_classic()


#Now I need to pivot it in a way that it can be read by an occ mod in unmarked
#BD
RV_2024 <- DiseaseData2_2024 %>% select(!c(LabID:SampleID,WaterTemp.Nearest, CatchOfDay, BD_PA1:BD_PA2))
RV_2024_longer <- RV_2024 %>% pivot_longer(cols = FV3_PA1:FV3_PA"2",names_to = "Rep", values_to = "PA")
RV_2024_longer$Rep <- str_remove(RV_2024_longer$Rep, "BD_PA")
RV_2024_wide <- RV_2024_longer %>% 
  pivot_wider(names_from = c(Survey.Number, LabNumber, Rep),
              values_from = PA,
              names_sort = T)


RV_2024_wide <- arrange(RV_2024_wide,as.numeric(ComplexID),as.numeric(PondID))

setwd(data)
save(BD_2022_wide,RV_2022_wide, file = "occModdata_2022.RData")
save(BD_2024_wide,RV_2024_wide, file = "occModdata_2024.RData")
### Okay last thing, need to setup water temp data to be read into occmods ###
# 2022
WaterTemp_2022 <- DiseaseData2_2022 %>% select(!c(SampleID,BD_Rep1:RV_Rep"2",SiteID,CatchOfDay,Species))
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
WaterTemp_2024 <- DiseaseData2_2024 %>% select(!c(LabID,SampleID,BD_Rep1:RV_Rep"2",SiteID,CatchOfDay))
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



save(WaterTemp_2022_wider, WaterTemp_2022_duplicate, file = "WaterData2022.RData")
save(WaterTemp_2024_wider, WaterTemp_2024_duplicate, file = "WaterData2024.RData")

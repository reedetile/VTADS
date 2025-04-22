#Description-----------------------------------------
#Data cleaning and set up for biodiversity data
#  18 Apr 2025
#RCS

#Initialize -----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
# Load functions--------------------------------------


# Global Variables-------------------------------------
VES <- read.csv("vesDataMaster.csv")
dipnet <- read.csv("DipnetDataMaster.csv")

# Program Body------------------------------------------

### Dipnet Data ####
# lets start by checking to make sure every input for eacch variable makes sense
unique(dipnet$ComplexID) # looks good
unique(dipnet$PondID) # looks good
unique(dipnet$SiteID) # looks good
unique(dipnet$SiteName) # This... may need to be cleaned up
unique(dipnet$Date) #looks good

# Okay so the goal is to summarize which species were detected across all three surveys at each pond
#lets start by limiting to only the variables we care about
dipnet$Sweep <- as.numeric(dipnet$Sweep)
dipnet2 <- dipnet %>% select(c(ComplexID:SiteID,Date,Sweep,AB:RSN))
dipnet2 <- dipnet2 %>% filter(Sweep < 19) #taking only first 18 sweep to keep sites equal

#Since I am running single season mult scale occ mods I'll need to seperate the years
dipnet2$Date <- mdy(dipnet2$Date)
unique(dipnet2$Date)
dipnet2$Year <- year(dipnet2$Date)
dipnet_2022 <- filter(dipnet2, Year == "2022")
dipnet_2024 <- filter(dipnet2, Year == "2024")

# Let's clean 2022 dipnet data first
dipnet_2022_sum <- dipnet_2022 %>% group_by(ComplexID, PondID, SiteID, Date) %>% dplyr::summarize(AB = sum(AB),
                                                                      AT = sum(AT),
                                                                      FT = sum(FT),
                                                                      GT = sum(GT),
                                                                      GF = sum(GF),
                                                                      MF = sum(MF),
                                                                      LF = sum(LF),
                                                                      PF = sum(PF),
                                                                      SP = sum(SP),
                                                                      WF = sum(WF),
                                                                      RSN = sum(RSN))
# Need to add visit info
dipnet_2022_sum <- arrange(dipnet_2022_sum, SiteID, Date)
dipnet_2022_sum$Visit <- rep(1:3,times = length(unique(dipnet_2022_sum$SiteID)))
dipnet_2022_sum <- dipnet_2022_sum %>% relocate(Visit, .after = Date)

# this is just to help visualize the dipnet data
dipnet_longer <- dipnet_2022_sum %>% pivot_longer(cols = AB:RSN, names_to = "Species")                                                                
ggplot(dipnet_longer, mapping = aes(x = Species, y = value))+
  geom_bar(stat = "summary")+
  theme_classic()

# Convert sum to P/A
dipnet_2022_PA <- dipnet_2022_sum
dipnet_2022_PA[,-c(1:5)] <- (dipnet_2022_PA[,-c(1:5)] != 0)*1

#Make all NAs 0
dipnet_2022_PA[is.na(dipnet_2022_PA)] <- 0


# Let's clean 2024 dipnet data next
dipnet_2024_sum <- dipnet_2024 %>% group_by(ComplexID, PondID, SiteID, Date) %>% dplyr::summarize(AB = sum(AB),
                                                                                                  AT = sum(AT),
                                                                                                  FT = sum(FT),
                                                                                                  GT = sum(GT),
                                                                                                  GF = sum(GF),
                                                                                                  MF = sum(MF),
                                                                                                  LF = sum(LF),
                                                                                                  PF = sum(PF),
                                                                                                  SP = sum(SP),
                                                                                                  WF = sum(WF),
                                                                                                  RSN = sum(RSN))
# Need to add visit info
dipnet_2024_sum <- arrange(dipnet_2024_sum, SiteID, Date)
dipnet_2024_sum$Visit <- rep(1:3,times = length(unique(dipnet_2024_sum$SiteID)))
dipnet_2024_sum <- dipnet_2024_sum %>% relocate(Visit, .after = Date)

# this is just to help visualize the dipnet data
dipnet_longer <- dipnet_2024_sum %>% pivot_longer(cols = AB:RSN, names_to = "Species")                                                                
ggplot(dipnet_longer, mapping = aes(x = Species, y = value))+
  geom_bar(stat = "summary")+
  theme_classic()

# Convert sum to P/A
dipnet_2024_PA <- dipnet_2024_sum
dipnet_2024_PA[,-c(1:5)] <- (dipnet_2024_PA[,-c(1:5)] != 0)*1

#Make all NAs 0
dipnet_2024_PA[is.na(dipnet_2024_PA)] <- 0

#okay dipnet data is workable, let's look at VES data

#VES
VES <- VES %>% select(c(ComplexID:SiteID, Date, SpeciesID, Count))
unique(VES$SpeciesID)


VES_clean <- VES %>% filter(SpeciesID != "" &
                              SpeciesID != "Tadpole" &
                              SpeciesID != "TADS" &
                              SpeciesID != "TAD(GF)" &
                              SpeciesID != "TAD" &
                              SpeciesID != "Tads" &
                              SpeciesID != "GF TAD" &
                              SpeciesID != "TADPOLE" &
                              SpeciesID != "Tadpoles" &
                              SpeciesID != "Painted Turtle" &
                              SpeciesID != "PAINTEDTURTLE" &
                              SpeciesID != "PAINTED TURTLE" &
                              SpeciesID != "PAINTED T" &
                              SpeciesID != "GARTER SNAKE" &
                              SpeciesID != "UNKNOWN" &
                              SpeciesID != "GF/AB")

VES_clean$SpeciesID <- str_trim(VES_clean$SpeciesID)
VES_clean$SpeciesID <- as.factor(VES_clean$SpeciesID)
VES_clean$SpeciesID <- droplevels(VES_clean$SpeciesID)
levels(VES_clean$SpeciesID)

# Remove NA for SiteID
VES_clean$SiteID <- as.factor(VES_clean$SiteID)
VES_clean$SiteID <- droplevels(VES_clean$SiteID)
levels(VES_clean$SiteID)

VES_clean$Count <- str_remove_all(VES_clean$Count, "[+~]")
unique(VES_clean$Count)
VES_clean$Count <- as.numeric(VES_clean$Count)

#Again need to separate in to 2022 and 2024
VES_clean$Date <- mdy(VES_clean$Date)
VES_clean$Year <- year(VES_clean$Date)

VES_2022 <- filter(VES_clean, Year == "2022")
VES_2024 <- filter(VES_clean, Year == "2024")

# VES 2022
# lets summarize
VES_2022_sum <- VES_2022 %>% group_by(ComplexID, PondID, SiteID, SpeciesID, Date) %>% summarize(count = sum(Count))

# lets plot it just to see how it looks
ggplot(data = VES_2022_sum, mapping = aes(x = SpeciesID, y = count))+
  geom_bar(stat = "summary")+
  theme_classic()
#pivot_wider
VES_2022_wider <- VES_2022_sum %>% pivot_wider(names_from = SpeciesID, values_from = count)

#Need to add in Visit number
StAlbansMissing <- data.frame(ComplexID = 5, 
                               PondID = 1, 
                               SiteID = "05-01",
                               Date = mdy("08/05/2022"), 
                               AB = NA, 
                               GF = NA, 
                               PF = NA, 
                               RSN = NA, 
                               AT = NA, 
                               WF = NA, 
                               SP = NA)

VES_2022_wider <- rbind(VES_2022_wider, StAlbansMissing)


VES_2022_wider <- arrange(VES_2022_wider, SiteID, Date)
VES_2022_wider$Visit <- rep(1:3,times = length(unique(VES_2022_wider$SiteID)))
VES_2022_wider <- VES_2022_wider %>% relocate(Visit, .after = Date)

#convert to P/A
VES_2022_PA <- VES_2022_wider
VES_2022_PA[,-c(1:5)] <- (VES_2022_PA[,-c(1:5)] != 0)*1

# compare VES and dipnet. What spp are surveyed in dipnet but not VES
dipnet_2022_spp <- names(dipnet_2022_PA)[6:length(names(dipnet_2022_PA))]
VES_2022_spp <- names(VES_2022_PA)[6:length(names(VES_2022_PA))]
dipnet_2022_spp[which(!(dipnet_2022_spp %in% VES_2022_spp))] # FT, GT, MF, and LF are not in VES data, which checks out

#Need to add in FT, MF, and LF to VES
VES_2022_PA$FT <- 0
VES_2022_PA$MF <- 0
VES_2022_PA$LF <- 0
VES_2022_PA$GT <- 0

# Need to change all NAs in VES_PA to 0
VES_2022_PA[is.na(VES_2022_PA)] <- 0

# VES 2024
# lets summarize
VES_2024_sum <- VES_2024 %>% group_by(ComplexID, PondID, SiteID, SpeciesID, Date) %>% summarize(count = sum(Count))

# lets plot it just to see how it looks
ggplot(data = VES_2024_sum, mapping = aes(x = SpeciesID, y = count))+
  geom_bar(stat = "summary")+
  theme_classic()
#pivot_wider
VES_2024_wider <- VES_2024_sum %>% pivot_wider(names_from = SpeciesID, values_from = count)
View(arrange(VES_2024_wider, SiteID, Date))
#Need to add 1st visit for 05-01
StAlbansMissing <- data.frame(ComplexID = 5, 
                               PondID = 1, 
                               SiteID = "05-01",
                               Date = mdy("06/06/2024"), 
                               AB = NA, 
                               GF = NA, 
                               PF = NA, 
                               RSN = NA, 
                               AT = NA, 
                               WF = NA, 
                               SP = NA)
#Need to add 2nd or 3rd visit for 09-01
GilbrookMissing <- data.frame(ComplexID = 9, 
                              PondID = 1, 
                              SiteID = "09-01",
                              Date = mdy("07/01/2024"), 
                              AB = NA, 
                              GF = NA, 
                              PF = NA, 
                              RSN = NA, 
                              AT = NA, 
                              WF = NA, 
                              SP = NA)
#Need to add 3rd visit for 11-01
StMikesMissing <- data.frame(ComplexID = 11, 
                              PondID = 1, 
                              SiteID = "09-01",
                              Date = mdy("07/19/2024"), 
                              AB = NA, 
                              GF = NA, 
                              PF = NA, 
                              RSN = NA, 
                              AT = NA, 
                              WF = NA, 
                              SP = NA)

#Need to add in Visit number

VES_2024_wider <- rbind(VES_2024_wider, StAlbansMissing)
VES_2024_wider <- rbind(VES_2024_wider, GilbrookMissing)
VES_2024_wider <- rbind(VES_2024_wider, StMikesMissing)





VES_2024_wider <- arrange(VES_2024_wider, SiteID, Date)
VES_2024_wider$Visit <- rep(1:3,times = length(unique(VES_2024_wider$SiteID)))
VES_2024_wider <- VES_2024_wider %>% relocate(Visit, .after = Date)

#convert to P/A
VES_2024_PA <- VES_2024_wider
VES_2024_PA[,-c(1:5)] <- (VES_2024_PA[,-c(1:5)] != 0)*1

# compare VES and dipnet. What spp are surveyed in dipnet but not VES
dipnet_2024_spp <- names(dipnet_2024_PA)[6:length(names(dipnet_2024_PA))]
VES_2024_spp <- names(VES_2024_PA)[6:length(names(VES_2024_PA))]
dipnet_2024_spp[which(!(dipnet_2024_spp %in% VES_2024_spp))] # FT, GT, MF, and LF are not in VES data, which checks out

#Need to add in FT, MF, and LF to VES
VES_2024_PA$FT <- 0
VES_2024_PA$MF <- 0
VES_2024_PA$LF <- 0

# Need to change all NAs in VES_PA to 0
VES_2024_PA[is.na(VES_2024_PA)] <- 0



# OKAY. We now have both VES and dipnet data in the same format
dim(dipnet_2022_PA)
dim(VES_2022_PA)

dim(dipnet_2024_PA)
dim(VES_2024_PA)
# All are 72*16, which is good!


#lets check the names though
names(dipnet_2022_PA)
names(VES_2022_PA)
names(dipnet_2024_PA)
names(VES_2024_PA)
# The first 5 columns are fine but the SPP columns are out of order. We need them to be in the same order so we can combine them.
# so let's put them alphabetically
dipnet_2022_PAONLY <- dipnet_2022_PA[,-(1:5)]
dipnet_2022_SITEINFO <- dipnet_2022_PA[, 1:5]
dipnet_2022_PAONLY
dipnet_2022_PAONLY <- dipnet_2022_PAONLY[,order(colnames(dipnet_2022_PAONLY))]
dipnet_2022_PA <- cbind(dipnet_2022_SITEINFO, dipnet_2022_PAONLY)


# Repeat for 2024
dipnet_2024_PAONLY <- dipnet_2024_PA[,-(1:5)]
dipnet_2024_SITEINFO <- dipnet_2024_PA[, 1:5]
dipnet_2024_PAONLY
dipnet_2024_PAONLY <- dipnet_2024_PAONLY[,order(colnames(dipnet_2024_PAONLY))]
dipnet_2024_PA <- cbind(dipnet_2024_SITEINFO, dipnet_2024_PAONLY)


# VES
#2022
VES_2022_PAONLY <- VES_2022_PA[,-(1:5)]
VES_2022_SITEINFO <- VES_2022_PA[, 1:5]
VES_2022_PAONLY
VES_2022_PAONLY <- VES_2022_PAONLY[,order(colnames(VES_2022_PAONLY))]
VES_2022_PA <- cbind(VES_2022_SITEINFO, VES_2022_PAONLY)

# 2024
VES_2024_PAONLY <- VES_2024_PA[,-(1:5)]
VES_2024_SITEINFO <- VES_2024_PA[, 1:5]
VES_2024_PAONLY
VES_2024_PAONLY <- VES_2024_PAONLY[,order(colnames(VES_2024_PAONLY))]
VES_2024_PA <- cbind(VES_2024_SITEINFO, VES_2024_PAONLY)

# lets check the names again
names(dipnet_2022_PA)
names(VES_2022_PA)
names(dipnet_2024_PA)
names(VES_2024_PA)

# they're in the column order. Next need to make sure rows match. For this it might make the most sense to use arrange
dipnet_2022_PA <- dipnet_2022_PA %>% arrange(ComplexID, PondID, Visit)
VES_2022_PA <- VES_2022_PA %>% arrange(ComplexID, PondID, Visit)

dipnet_2024_PA <- dipnet_2024_PA %>% arrange(ComplexID, PondID, Visit)
VES_2024_PA <- VES_2024_PA %>% arrange(ComplexID, PondID, Visit)

  
PA_2022 <- bind_rows(dipnet_2022_PA,VES_2022_PA) %>%
  select(!(Date)) %>%
  group_by(ComplexID, PondID, SiteID, Visit) %>%
  summarize(across(everything(), sum))

PA_2022[,-c(1:4)] <- (PA_2022[,-c(1:4)] != 0)*1

PA_2024 <- bind_rows(dipnet_2024_PA,VES_2024_PA) %>%
  select(!(Date)) %>%
  group_by(ComplexID, PondID, SiteID, Visit) %>%
  summarize(across(everything(), sum))

PA_2024[,-c(1:4)] <- (PA_2024[,-c(1:4)] != 0)*1
# Yay now I have 6 DF's that can be used to assess biodiversity
dipnet_2022_PA #Dipnet data for 2022
dipnet_2024_PA #dipnet data for 2024
VES_2022_PA # VES data for 2022
VES_2024_PA # VES data for 2024
PA_2022 # Overall data for 2022
PA_2024 # Overall data for 2024

save(dipnet_2022_PA, #Dipnet data for 2022
     dipnet_2024_PA, #dipnet data for 2024
     VES_2022_PA, # VES data for 2022
     VES_2024_PA, # VES data for 2024
     PA_2022, # Overall data for 2022
     PA_2024, # Overall data for 2024
     file = "biodiversityData.RData")

# End script     
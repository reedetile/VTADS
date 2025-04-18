#Description-----------------------------------------
#Data cleaning and set up for biodiversity data
#  18 Apr 2025
#RCS

#Initialize -----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

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

dipnet_sum <- dipnet2 %>% group_by(ComplexID, PondID, SiteID) %>% dplyr::summarize(AB = sum(AB),
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
dipnet_longer <- dipnet_sum %>% pivot_longer(cols = AB:RSN, names_to = "Species")                                                                
ggplot(dipnet_longer, mapping = aes(x = Species, y = value))+
  geom_bar(stat = "summary")+
  theme_classic()

#Description-----------------------------------------
#Setup of biodiversity data
#  22 Apr 2025
#RCS

#Initialize -----------------------------------------
library(ggplot2)
library(vegan)
library(dplyr)

# Global Variables-------------------------------------
repo <- getwd()
graphs <- paste(repo,'/Graphs', sep ="")
load("biodiversityData.RData")

# Program Body------------------------------------------

# Let's start with the easy thing: measuring alpha diversity at each survey
# Alpha diversity
#dipnet
dipnet2022_alpha <- rowSums(dipnet_2022_PA[6:ncol(dipnet_2022_PA)])
dipnet2024_alpha <- rowSums(dipnet_2024_PA[6:ncol(dipnet_2024_PA)])

# VES
VES2022_alpha <-  rowSums(VES_2022_PA[6:ncol(dipnet_2022_PA)])
VES2024_alpha <- rowSums(VES_2024_PA[6:ncol(dipnet_2024_PA)])

# Overall
overall2022_alpha <-  rowSums(PA_2022[6:ncol(PA_2022)])
overall2024_alpha <- rowSums(PA_2024[6:ncol(PA_2024)])

# Now to measure beta, which I think will be a little harder
visits <- 3 # just establishing the number of visits I did to each pond
dipnet_beta_list <- vector("list", length = nrow(dipnet_2022_PA))

BoVT <- dipnet_2022_PA %>% filter(ComplexID == "1" & Visit == "1")
test <- vegdist(BoVT[,5:16], binary = T)
Rutland <- dipnet_2022_PA %>% filter(ComplexID == "7" & Visit == "1")
test <- vegdist(Rutland[,5:16], binary = T)
CampJ <- dipnet_2022_PA %>% filter(ComplexID == "10" & Visit == "1")
test <- mean(vegdist(CampJ[,5:16], binary = T))




for (i in 1:max(dipnet_2022_PA$ComplexID)) {
  for (j in 1:visits) {
    
  }
  
}
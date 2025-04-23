#Description-----------------------------------------
#Setup of biodiversity data
#  22 Apr 2025
#RCS

#Initialize -----------------------------------------
library(ggplot2)
library(vegan)
library(dplyr)
library(tidyr)

# Global Variables-------------------------------------
repo <- getwd()
graphs <- paste(repo,'/Graphs', sep ="")
load("biodiversityData.RData")

# Program Body------------------------------------------

# Let's start with the easy thing: measuring alpha diversity at each survey
# Alpha diversity
#dipnet
dipnet_2022_PA$alpha <- rowSums(dipnet_2022_PA[6:ncol(dipnet_2022_PA)])
dipnet_2024_PA$alpha <- rowSums(dipnet_2024_PA[6:ncol(dipnet_2024_PA)])

# VES
VES_2022_PA$alpha <-  rowSums(VES_2022_PA[6:ncol(VES_2022_PA)])
VES_2024_PA$alpha <- rowSums(VES_2024_PA[6:ncol(VES_2024_PA)])

# Overall
PA_2022$alpha<-  rowSums(PA_2022[6:ncol(PA_2022)])
PA_2024$alpha <- rowSums(PA_2024[6:ncol(PA_2024)])

# reorder columns real quick

dipnet_2022_PA <- dipnet_2022_PA %>% relocate(alpha, .after = Visit)
dipnet_2024_PA <- dipnet_2024_PA %>% relocate(alpha, .after = Visit)
VES_2022_PA <- VES_2022_PA %>% relocate(alpha, .after = Visit)
VES_2024_PA <- VES_2024_PA %>% relocate(alpha, .after = Visit)
PA_2022 <- PA_2022 %>% relocate(alpha, .after = Visit)
PA_2024 <- PA_2024 %>% relocate(alpha, .after = Visit)

# Now to measure beta, which I think will be a little harder


visits <- 3 # just establishing the number of visits I did to each pond

# Dipnet
# 2022
dipnet_2022_PA$beta <- NA
dipnet_2022_PA <-  relocate(dipnet_2022_PA, beta, .after = alpha)
for (i in 1:max(dipnet_2022_PA$ComplexID)) {
  for (j in 1:visits) {
    Complex <- as.character(i)
    visit <- as.character(j)
    metacomm <- dipnet_2022_PA %>% filter(ComplexID == Complex & Visit == visit)
    betadiv <- mean(vegdist(metacomm[,8:ncol(metacomm)], binary =T))
    detadiv <- rep(betadiv, nrow(metacomm))
    dipnet_2022_PA[dipnet_2022_PA$ComplexID == Complex & dipnet_2022_PA$Visit == visit,7] <- betadiv
  }
}

# Dipnet 2024
dipnet_2024_PA$beta <- NA
dipnet_2024_PA <-  relocate(dipnet_2024_PA, beta, .after = alpha)
for (i in 1:max(dipnet_2024_PA$ComplexID)) {
  for (j in 1:visits) {
    Complex <- as.character(i)
    visit <- as.character(j)
    metacomm <- dipnet_2024_PA %>% filter(ComplexID == Complex & Visit == visit)
    betadiv <- mean(vegdist(metacomm[,8:ncol(metacomm)], binary =T))
    detadiv <- rep(betadiv, nrow(metacomm))
    dipnet_2024_PA[dipnet_2024_PA$ComplexID == Complex & dipnet_2024_PA$Visit == visit,7] <- betadiv
  }
}


# VES
# 2022
VES_2022_PA$beta <- NA
VES_2022_PA <-  relocate(VES_2022_PA, beta, .after = alpha)
for (i in 1:max(VES_2022_PA$ComplexID)) {
  for (j in 1:visits) {
    Complex <- as.character(i)
    visit <- as.character(j)
    metacomm <- VES_2022_PA %>% filter(ComplexID == Complex & Visit == visit)
    betadiv <- mean(vegdist(metacomm[,8:ncol(metacomm)], binary =T))
    detadiv <- rep(betadiv, nrow(metacomm))
    VES_2022_PA[VES_2022_PA$ComplexID == Complex & VES_2022_PA$Visit == visit,7] <- betadiv
  }
}

#2024
VES_2024_PA$beta <- NA
VES_2024_PA <-  relocate(VES_2024_PA, beta, .after = alpha)
for (i in 1:max(VES_2024_PA$ComplexID)) {
  for (j in 1:visits) {
    Complex <- as.character(i)
    visit <- as.character(j)
    metacomm <- VES_2024_PA %>% filter(ComplexID == Complex & Visit == visit)
    betadiv <- mean(vegdist(metacomm[,8:ncol(metacomm)], binary =T))
    detadiv <- rep(betadiv, nrow(metacomm))
    VES_2024_PA[VES_2024_PA$ComplexID == Complex & VES_2024_PA$Visit == visit,7] <- betadiv
  }
}

# Overall
# 2022
PA_2022$beta <- NA
PA_2022 <-  relocate(PA_2022, beta, .after = alpha)
for (i in 1:max(PA_2022$ComplexID)) {
  for (j in 1:visits) {
    Complex <- as.character(i)
    visit <- as.character(j)
    metacomm <- PA_2022 %>% filter(ComplexID == Complex & Visit == visit)
    betadiv <- mean(vegdist(metacomm[,7:ncol(metacomm)], binary =T))
    detadiv <- rep(betadiv, nrow(metacomm))
    PA_2022[PA_2022$ComplexID == Complex & PA_2022$Visit == visit,6] <- betadiv
  }
}

# 2024
PA_2024$beta <- NA
PA_2024 <-  relocate(PA_2024, beta, .after = alpha)
for (i in 1:max(PA_2024$ComplexID)) {
  for (j in 1:visits) {
    Complex <- as.character(i)
    visit <- as.character(j)
    metacomm <- PA_2024 %>% filter(ComplexID == Complex & Visit == visit)
    betadiv <- mean(vegdist(metacomm[,7:ncol(metacomm)], binary =T))
    detadiv <- rep(betadiv, nrow(metacomm))
    PA_2024[PA_2024$ComplexID == Complex & PA_2024$Visit == visit,6] <- betadiv
  }
}

# My opinion based on the 6 DF's.  I get a lot of warnings for the VES + dipnet DF's b/c there are a lot of cases where nothing was detected
# I think that only useful info is the overall P/A data (PA_2022 and PA_2024). So until I here otherwise I'm only moving forward with that

Diversity_2022 <- PA_2022 %>% 
  select(ComplexID:beta) %>% 
  pivot_wider(names_from = Visit, values_from = c(beta, alpha)) 
Diversity_2024 <- PA_2024 %>%
  select(ComplexID:beta) %>%
  pivot_wider(names_from = Visit, values_from = c(beta, alpha)) 

visit1 <- PA_2022 %>% filter(Visit == "1")
vegdist(visit1[, 7:17])


# What if instead of measuring beta diversity within meta comms we just compared beta of all ponds in a given survey period
PA_2022$beta <- NA
for (i in 1:visits) {
  visit <- as.character(j)
  visit_data <- PA_2022 %>% filter(Visit == visit) #this subsets to just a particular visit / sampling period
  betadiv <- mean(betadiver(visit_data[,7:ncol(metacomm)], binary =T))
  
  
}
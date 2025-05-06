#Description-----------------------------------------
# Script to organize and prepare temperature data to be read in to disease models
#  06 May 2025
#RCS

#Initialize -----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)
# Load functions--------------------------------------


# Global Variables-------------------------------------
repo <- "D:/gitrepos/VTADS"
TempData <- paste(repo,"/TemperatureData", sep = "")

# Program Body------------------------------------------
setwd(TempData)
Sites <- list.files()[1:23]
Temperature_list <- vector("list", length = length(Sites))
names(Temperature_list) <- Sites
names(Temperature_list)
SiteID <- c("02-01",
            "01-01",
            "10-01",
            "10-02",
            "10-03",
            "04-01",
            "09-01",
            "09-02",
            "09-03",
            "03-01",
            "08-01",
            "08-02",
            "06-01",
            "07-01",
            "12-01",
            "12-02",
            "12-03",
            "07-02",
            "05-01",
            "05-02",
            "11-01",
            "11-02",
            "11-04")


for(i in 1:length(Sites)){
  setwd(paste(TempData, Sites[[i]], sep = "/"))
  temp = list.files(pattern="\\.csv$")
  temp_list <- vector("list", length = length(temp))
  for (j in 1:length(temp)) {
    df <- fread(temp[[j]])
    df <- df[,2:3]
    df <- as.data.frame(df)
    df$Date <- str_extract(df[,1], "\\d+/\\d+/\\d+")
    df <- df[,-1]
    df$Date <- mdy(df$Date)
    names(df) <- c("Temp","Date")
    temp_list[[j]] <- df 
  }
  Temp_df <- do.call(rbind, temp_list)
  Temp_df <- Temp_df[complete.cases(Temp_df),]
  Temperature_list[[i]] <-  list(Temp_df, SiteID[[i]])
}
  
setwd(TempData)
save(Temperature_list, file = "Temperature_list.RData")

# Okay so now we have all the data in a list.
# We need to go through site by site and establish mean temperature for 2 weeks prior to survey
# Then should probably combine them all in 1 df
for(i in 1:length(Temperature_list)){
  object <- names(Temperature_list)[[i]]
  assign(object, Temperature_list[[i]])
}
  
setwd(paste(repo, "/Data", sep = ""))
load("biodiversityData.RData")
SwabData <- read.csv("SwabDataMaster.csv")
SwabData <- filter(SwabData, SiteID != "")
unique(SwabData$SiteID)



Survey <- c("1","2","3")
Year <- c("2022","2024")
num_visits <- length(Survey)*length(Year)
AvgTempsList <- vector("list", length = length(Temperature_list))
for(i in 1:length(Temperature_list)){
  Site_list <- vector("list", length = length(Year))
  Site_df <- Temperature_list[[i]][[1]]
  Site_df$Temp <- as.numeric(Site_df$Temp)
  name <- names(Temperature_list)[i]
  print(paste("i = ", i))
  for (j in 1:length(Year)) {
    print(paste("j = ", j))
    surv_list <- vector("list", length = length(Survey))
    for (k in 1:length(Survey)) {
      print(paste("k = ", k))
      surv_subset <- SwabData %>% filter(SiteID ==  Temperature_list[[i]][[2]] & 
                                           Survey.Number == Survey[[j]] & 
                                           Year == Year[[j]])
      survey.date <- mdy(unique(surv_subset$Date))
      TwoWeeks <- survey.date - days(14)
      time_interval <- interval(TwoWeeks, survey.date)
      data <- Site_df %>% filter(Date %within% time_interval)
      Site <- name
      Year_visit <- Year[[j]]
      survey_visit <- Survey[[k]]
      mean_temp <- mean(data$Temp)
      surv_list[[j]] <- list(Year_visit, survey_visit, mean_temp)
    }
    Site_Year <- paste(name, Year[[j]], sep = "")
    Site_list[[j]] <- assign(Site_Year, surv_list)
  }
  AvgTempsList[[i]] <- assign(name, Site_list)
}


surv_subset <- SwabData %>% filter(SiteID ==  Temperature_list[[7]][[2]] & 
                                     Survey.Number == Survey[[1]] & 
                                     Year == Year[[1]])
survey.date <- mdy(unique(surv_subset$Date))

# Audobon
Audobon_list <- vector("list", length = length(Year))
for(i in 1:length(Year)){
  surv_list <- vector("list", length = length(Survey))
  for(j in 1:length(Survey)){
    surv_subset <- SwabData %>% filter(SiteID == '02-01' & Survey.Number == Survey[[j]] & Year == Year[[i]])
    survey.date <- mdy(unique(surv_subset$Date))
    TwoWeeks <- survey.date - days(14)
    time_interval <- interval(TwoWeeks, survey.date)
    data <- Audobon[[1]] %>% filter(Date %within% time_interval)
    Site <- "Audobon"
    Year_visit <- Year[[i]]
    survey_visit <- Survey[[j]]
    mean_temp <- mean(data$Temp)
    surv_list[[j]] <- c(Year_visit, survey_visit, mean_temp)
  }
  Audobon_Year <- paste("Audobon", Year[[i]])
  Audobon_list[[i]] <- assign(Audobon_Year, surv_list)
}
    
    
# Birds of Vermont
BirdsOfVT$Temp <- as.numeric(BirdsOfVT$Temp)
BirdsOfVT_list <- vector("list", length = length(Year))
for(i in 1:length(Year)){
  surv_list <- vector("list", length = length(Survey))
  for(j in 1:length(Survey)){
    surv_subset <- SwabData %>% filter(SiteID == '01-01' & Survey.Number == Survey[[j]] & Year == Year[[i]])
    survey.date <- mdy(unique(surv_subset$Date))
    TwoWeeks <- survey.date - days(14)
    time_interval <- interval(TwoWeeks, survey.date)
    data <- BirdsOfVT %>% filter(Date %within% time_interval)
    Site <- "BirdsOfVT"
    Year_visit <- Year[[i]]
    survey_visit <- Survey[[j]]
    mean_temp <- mean(data$Temp)
    surv_list[[j]] <- c(Year_visit, survey_visit, mean_temp)
  }
  BirdsOfVT_Year <- paste("BirdsOfVT", Year[[i]])
  BirdsOfVT_list[[i]] <- assign(BirdsOfVT_Year, surv_list)
}


# Camp Johson-02
CampJ02_list <- vector("list", length = length(Year))
for(i in 1:length(Year)){
  surv_list <- vector("list", length = length(Survey))
  for(j in 1:length(Survey)){
    surv_subset <- SwabData %>% filter(SiteID == '10-02' & Survey.Number == Survey[[j]] & Year == Year[[i]])
    survey.date <- mdy(unique(surv_subset$Date))
    TwoWeeks <- survey.date - days(14)
    time_interval <- interval(TwoWeeks, survey.date)
    data <- CampJohnson-2 %>% filter(Date %within% time_interval)
    Site <- "CampJ02"
    Year_visit <- Year[[i]]
    survey_visit <- Survey[[j]]
    mean_temp <- mean(data$Temp)
    surv_list[[j]] <- c(Year_visit, survey_visit, mean_temp)
  }
  CampJ02_Year <- paste("CampJ02", Year[[i]])
  CampJ02_list[[i]] <- assign(CampJ02_Year, surv_list)
}


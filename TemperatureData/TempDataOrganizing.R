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
repo <- 'C:/Users/rcscott/VTADS'
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

setwd(paste(TempData, Sites[[7]], sep = "/"))
temp = list.files(pattern="\\.csv$")
temp_list <- vector("list", length = length(temp))
df <- fread(temp[[1]])
df <- df[,2:3]
df <- as.data.frame(df)
df$Date <- str_extract(df[,1], "\\d+/\\d+/\\d+")
df <- df[,-1]
df$Date <- mdy(df$Date)
df <- df[!(df$Temp == "" | is.na(df$Temp)), ]
names(df) <- c("Temp","Date")

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
dipnet <- read.csv("DipnetDataMaster.csv")
dipnet <- filter(dipnet, SiteID != "")
unique(dipnet$SiteID)

#lets start by limiting to only the variables we care about
dipnet$Sweep <- as.numeric(dipnet$Sweep)
dipnet2 <- dipnet %>% select(c(ComplexID:SiteID,Date,Sweep,AB:RSN))
dipnet2 <- dipnet2 %>% filter(Sweep < 19) #taking only first 18 sweep to keep sites equal

dipnet2$Date <- mdy(dipnet2$Date)
unique(dipnet2$Date)

dipnet_sum <- dipnet2 %>% group_by(ComplexID, PondID, SiteID, Date) %>% 
  dplyr::summarize(AB = sum(AB),
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
dipnet_sum <- arrange(dipnet_sum, SiteID, Date)
dipnet_sum$Visit <- rep(1:3,times = length(unique(dipnet_sum$SiteID))*2)
SurveyDates <- dipnet_sum %>% select(c(SiteID, Date, Visit))
SurveyDates$Year <- year(SurveyDates$Date)



Survey <- c("1","2","3")
Year_array <- c("2022","2024")
num_visits <- length(Survey)*length(Year_array)
AvgTempsList <- vector("list", length = length(Temperature_list))

for(i in 1:length(Temperature_list)){
  Site_list <- vector("list", length = length(Year_array))
  Site_df <- Temperature_list[[i]][[1]]
  Site_df$Temp <- as.numeric(Site_df$Temp)
  Site_df$Year <- year(Site_df$Date)
  site_name <- SiteID[[i]]
  site_ls <- vector("list", length = length(Year_array))
  df <- data.frame(matrix(nrow = 1, ncol = 3))
  colnames(df) <- c("SiteID", "Year","mean_temp")
    for (j in 1:length(Year_array)) {
      data <- Site_df %>% filter(Year == Year_array[[j]])
      AvgTemp <- mean(data$Temp, na.rm = T)
      df[j,1] <- site_name
      df[j,2] <- Year_array[[j]]
      df[j,3] <- AvgTemp
    }
  AvgTempsList[[i]] <- df
}

AvgTemps_df <- do.call(rbind, AvgTempsList)

Lastrows <- data.frame(SiteID = c("06-02","06-02"), Year = c("2022","2024"), mean_temp = rep(NA,2))

AvgTemps_df <- rbind(AvgTemps_df,Lastrows)

TemData2022_OccMod <- filter(AvgTemps_df, Year == "2022")
TemData2024_OccMod <- filter(AvgTemps_df, Year == "2024")



setwd(paste(repo, "/Data", sep = ""))
save(TemData2022_OccMod, TemData2024_OccMod, file = "TempDataOccMod.RData")  



# for(i in 1:length(Temperature_list)){
#   Site_list <- vector("list", length = length(Year_array))
#   Site_df <- Temperature_list[[i]][[1]]
#   Site_df$Temp <- as.numeric(Site_df$Temp)
#   name <- SiteID[[i]]
#   site_ls <- vector("list", length = length(Year_array))
#   for (j in 1:length(Year_array)) {
#     surv_df <- data.frame(matrix(nrow = 3, ncol = 3))
#     colnames(surv_df) <- c("Year", "Survey", "mean_temp")
#     YearData <- SurveyDates %>% filter(SiteID == Temperature_list[[i]][[2]] & Year == Year_array[[j]])
#     for (k in 1:length(Survey)) {
#       surv_subset <- YearData %>% filter(Visit == Survey[[k]]) 
#       survey.date <- surv_subset$Date
#       TwoWeeks <- survey.date - days(14)
#       time_interval <- interval(TwoWeeks, survey.date)
#       data <- Site_df %>% filter(Date %within% time_interval)
#       Site <- name
#       surv_df$Year <- Year_array[[j]]
#       surv_df[k,2] <- Survey[[k]]
#       surv_df[k,3] <- mean(data$Temp)
#     }
#     site_ls[[j]] <- surv_df
#   }
#   AvgTempsList[[i]] <- assign(name, site_ls)
# }
# 
# names(AvgTempsList) <- SiteID
# 
# AvgTempsList2 <- vector("list", length = length(AvgTempsList))
# names(AvgTempsList2) <- names(AvgTempsList)
# for (i in 1:length(AvgTempsList)) {
#   AvgTempsList2[[i]] <-  do.call(rbind, AvgTempsList[[i]])
# }
# 
# 
# 
# 
# SumTempData <- bind_rows(AvgTempsList2, .id="id")
# SumTempData <- SumTempData %>% mutate_all(~ifelse(is.nan(.), NA, .))
# colnames(SumTempData)[1] <- "SiteID"
# TempData_2022 <- SumTempData %>% filter(Year == "2022")
# TemData2022_OccMod <- TempData_2022 %>% pivot_wider(names_from = Survey, values_from = mean_temp)
# LastRow <- c("06-02",rep(NA, 4)) #need to add in dumby row for 06-02
# TemData2022_OccMod <- rbind(TemData2022_OccMod, LastRow)
# 
# TempData_2024 <- SumTempData %>% filter(Year == "2024")
# TemData2024_OccMod <- TempData_2024 %>% pivot_wider(names_from = Survey, values_from = mean_temp)
# LastRow <- c("06-02",rep(NA, 4)) #need to add in dumby row for 06-02
# TemData2024_OccMod <- rbind(TemData2024_OccMod, LastRow)



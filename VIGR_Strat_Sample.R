#Description-----------------------------------------
#code to collect a stratified random sample to determine
# which samples to test first
#  02 Dec 2024
#RCS

#Initialize -----------------------------------------
library(dplyr)
library(ggplot2)

# Load functions--------------------------------------


# Global Variables-------------------------------------
Swab_data <- read.csv('SwabDataMaster.csv')

# Program Body------------------------------------------
swabs_2024 <- Swab_data %>% filter(Year == "2024")
swabs_2022 <- Swab_data %>% filter(Year == "2022")

# If we want to do a stratified sample including Survey
strat_sample_2024 <- swabs_2024 %>%
  filter(!is.na(Survey.Number)) %>%
  group_by(SiteID, Survey.Number) %>%
  sample_n(size = 3)

strat_sample_2022 <- swabs_2022 %>%
  filter(!is.na(Survey.Number)) %>%
  group_by(SiteID, Survey.Number) %>%
  sample_n(size = 3, replace = T)

#This results in ~ 423 samples to be tested (not including controls + lab NEGs)

# Create the DF of samples to be tested!
strat_df <- strat_sample_2022 %>% full_join(strat_sample_2024)

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
strat_sample <- Swab_data %>%
  filter(!is.na(Survey.Number)) %>%
  group_by(SiteID, Year, Survey.Number) %>%
  sample_n(size = 1)

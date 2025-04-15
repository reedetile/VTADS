
library(ggplot2)
library(dplyr)
library(stringr)
library(gt)
swabs <-  read.csv("SwabDataMaster.csv")

swabs$Species <- str_trim(swabs$Species, "both")
swabs$SiteID <- str_replace_all(swabs$SiteID, " ","")
swabs <- filter(swabs, Species != "")


spp_info <-  swabs %>% group_by(Species) %>% summarize(count = n())

gt(spp_info)

spp_count <- ggplot(data = spp_info, aes(x = Species, y = count))+
  geom_bar(stat = "identity")+
  theme_classic()
spp_count

# number of ind captured at each site over each the whole study
site_info <- swabs %>% group_by(SiteID, Species) %>% summarize(count = n())
site_info$Species <-  as.factor(site_info$Species)
site_plot <- ggplot(site_info, aes(x = SiteID, y = count, fill = Species))+
  geom_bar(position = "stack",stat = "identity")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  ggtitle(label = "Species sampling distrubution by Site")
site_plot

# number of ind captured at each VISIT
visit_info <- swabs %>% group_by(SiteID, Year, Survey.Number, Species) %>% summarize(count = n())
visit_info$SiteVisitYear <- paste(visit_info$SiteID,visit_info$Survey.Number,visit_info$Year, sep="_")
visit_plot <- ggplot(visit_info, aes(x = SiteVisitYear, y = count, fill = Species))+
  geom_bar(position = "stack", stat = "identity")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  ggtitle(label = "Species sampling distrubution by Visit")
visit_plot

swabs %>% filter(SiteID == "10-02", Survey.Number == "2", Year == "2024")

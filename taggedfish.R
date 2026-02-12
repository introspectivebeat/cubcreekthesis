library(tidyverse)
library(scales)

#load data
fishdat <- read.csv("taggedfish_data.csv", header = TRUE)
fishdat <- fishdat[, -c(10:18)]
fishsummary <- read.csv("taggedfish_summary.csv", header = TRUE)
fishsummary <- fishsummary[-1,]

#basic histograms
hist(fishdat$Wt_g, breaks = 20)
hist(fishdat$TL_mm, breaks = 20)

#individual species dataframes
unique(fishdat$Species)

weccdat <- fishdat %>% 
  filter(Species == "WECC")
yebudat <- fishdat %>% 
  filter(Species == "YEBU")
grsudat <- fishdat %>% 
  filter(Species == "GRSU")
longdat <- fishdat %>% 
  filter(Species == "LONG")
leucdat <- fishdat %>% 
  filter(Species == "Leuciscidae")
cypdat <- fishdat %>% 
  filter(Species == "Cyprinella")
bluedat <- fishdat %>% 
  filter(Species == "BLUE")
lmbdat <- fishdat %>% 
  filter(Species == "LMB")
dosudat <- fishdat %>% 
  filter(Species == "DOSU")
blshdat <- fishdat %>% 
  filter(Species == "BLSH")
brmadat <- fishdat %>% 
  filter(Species == "BRMA")
goshdat <- fishdat %>% 
  filter(Species == "GOSH")
resudat <- fishdat %>% 
  filter(Species == "RESU")
grwadat <- fishdat %>% 
  filter(Species == "GRWA")
lepomisdat <- fishdat %>% 
  filter(Species == "Lepomis")
blcr <- fishdat %>% 
  filter(Species == "BLCR")


tagcount <- data.frame(
  species=c("Leuciscidae spp.","Cyprinella spp.","Bluntface Shiner","Golden Shiner",
         "Western Creek Chubsucker", "Yellow Bullhead", "Brown Madtom", "Lepomis spp.",
         "Green Sunfish", "Bluegill", "Dollar Sunfish", "Longear Sunfish",
         "Redear Sunfish", "Largemouth Bass", "Black Crappie"),  
  tagged=c(13, 7, 69, 12, 12, 66, 1, 9, 89, 159, 67, 50, 2, 18, 1)
)

ggplot(tagcount, aes(x = reorder(species, -tagged), y=tagged)) + 
  geom_bar(stat = "identity", fill = "orange2")+
  xlab("Species")+
  ylab("Number Tagged")+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

untagged_count <- data.frame(
  species = c("Leuciscidae spp.", "Green Sunfish", "Blackspotted Topminnow", 
              "Cyprinella spp.", "Etheostoma spp.", "Largemouth Bass", "Yellow Bullhead",
              "Western Mosquitofish", "Bluegill", "Orangefin Shiner", "Ribbon Shiner",
              "Dollar Sunfish", "Blackstripe Topminnow", "Golden Shiner", "Bluntface Shiner",
              "Spotted Bass", "Black Crappie", "Lepomis spp.", "Longear Sunfish", "Gizzard Shad",
              "Brown Madtom", "Cypress Darter"),
  count = c(208, 107, 127, 9, 8, 8, 19, 65, 89, 9, 29, 43, 4, 1, 38, 1, 1, 6, 3, 
            1, 2, 3)
)

ggplot(untagged_count, aes(x = reorder(species, -count), y=count)) + 
  geom_bar(stat = "identity", fill = "orange2")+
  xlab("Species")+
  ylab("Number Caught")+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

#end of proj

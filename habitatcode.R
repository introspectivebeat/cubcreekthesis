library(tidyverse)
library(scales)
library(geomtextpath)
library(colorBlindness)
library(ggridges)

poddat <- read.csv("poddatfull.csv", header = TRUE)
poddat <- poddat[-c(9:17, 21, 30:32),]

coverdat <- read.csv("coverdat.csv", header = TRUE)

##### depth and velocity #####
availdat <- read.csv("availabilitydat.csv", header = TRUE)
availdat$Vel_m <- as.numeric(availdat$Vel_m)
availonly <- availdat %>% 
  filter(Type == "Availability")
useonly <- availdat %>% 
  filter(Type == "Use")

ggplot(useonly, aes(x = Depth_m))+
  geom_histogram(bins = 6, color = "black", fill = "grey")+
  theme_bw()

ggplot(availonly, aes(x = Depth_m))+
  geom_histogram(bins = 10, color = "black", fill = "grey")+
  theme_bw()


ggplot(availdat, aes(x = Type, y = Depth_m, fill = Type))+
  geom_violin(linewidth = 0.8, alpha = 0.2)+
  geom_boxplot(width = 0.1)+
  scale_fill_manual(values = c("palevioletred", "peachpuff3"))+
  xlab("")+
  ylab("Depth (m)")+
  theme_bw()

ggplot(availdat, aes(x = Type, y = Vel_m, fill = Type))+
  geom_violin(linewidth = 0.8, alpha = 0.2)+
  geom_boxplot(width = 0.1)+
  scale_fill_manual(values = c("palevioletred", "peachpuff3"))+
  xlab("")+
  ylab("Velocity (m/s)")+
  theme_bw()



ggplot(availonly, aes(x = Depth_m))+
  geom_histogram(bins = 25, fill = "peachpuff3")+
  geom_vline(xintercept = mean(useonly$Depth_m, na.rm = TRUE), color = "darkcyan")+
  theme_bw()

ggplot(availonly, aes(x = Vel_m))+
  geom_histogram(bins = 25, fill = "peachpuff3")+
  geom_vline(xintercept = mean(useonly$Vel_m, na.rm = TRUE), color = "darkcyan")+
  theme_bw()


#DEPTH ridgeline:
ggplot(availdat, aes(x = Depth_m, y = Type, height = stat(density))) + 
  geom_density_ridges(stat = "binline", bins = 15, scale = 0.99, fill = "peachpuff3", 
                      draw_baseline = TRUE)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 1))) +
  xlab("Depth (m)")+
  ylab("")+
  theme_ridges(center_axis_labels = TRUE, grid = TRUE)

#VELOCITY ridgeline:
ggplot(availdat, aes(x = Vel_m, y = Type, height = stat(density))) + 
  geom_density_ridges(stat = "binline", bins = 15, scale = 0.99, fill = "peachpuff3", 
                      draw_baseline = TRUE)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 1))) +
  xlab("Velocity (m/s)")+
  ylab("")+
  theme_ridges(center_axis_labels = TRUE, grid = TRUE)

##### cover #####
mypal <- c("peachpuff3", "honeydew3", "palevioletred", "salmon", "darkcyan", "darkseagreen", "darkorange3", "plum3")

ggplot(data=coverdat, aes(x=Cover, y=Count, fill = Species)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = mypal)+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

coveravail <- data.frame(Cover = availdat$Cover) 
# Create frequency table
coverfreqavail <- coveravail %>%
  count(Cover) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

coveruse <- data.frame(Cover = poddat$Cover) 
# Create frequency table
coverfrequse <- coveruse %>%
  count(Cover) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

add_row(coverfrequse, Cover = "Submerged Vegetation", n = 0, Proportion = 0)
add_row(coverfreqavail, Cover = "Overhanging Bank", n = 0, Proportion = 0)

freqtot <- full_join(coverfreqavail, coverfrequse, by = "Cover") 

print(freqtot)

freqtotwide <- data.frame(Cover = c("Boulder", "Boulder", "Coarse Woody Debris", 
                                    "Coarse Woody Debris", "Detritus", "Detritus", "Emergent Vegetation", 
                                    "Emergent Vegetation", "None", "None", "Overhanging Vegetation",
                                    "Overhanging Vegetation", "Overhanging Bank", 
                                    "Overhanging Bank", "Submersed Vegetation", 
                                    "Submersed Vegetation"),
                          Group = c("Use", "Availability", "Use", "Availability",
                                    "Use", "Availability",
                                    "Use", "Availability", "Use", "Availability", 
                                    "Use", "Availability", "Use", "Availability",
                                    "Use", "Availability"), 
                          Frequency = c(0.04761905, 0.029154519, 0.09523810, 0.029154519,
                                        0, 0.002915452, 0.09523810, 0.029154519, 
                                        0.38095238, 0.804664723, 0.23809524, 0.087463557,
                                        0.14285714, 0.008746356, 0, 0.026239067))
grouporder <- c("Use", "Availability")
coverorder <- c("Boulder", "Coarse Woody Debris", "Detritus", "Emergent Vegetation", 
                "Overhanging Vegetation", "Overhanging Bank", "Submersed Vegetation", "None")
freqtotwide$Group <- factor(freqtotwide$Group, levels = grouporder)
freqtotwide$Cover <- factor(freqtotwide$Cover, levels = coverorder)

ggplot(data=freqtotwide, aes(x=Cover, y=Frequency, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("peachpuff3", "palevioletred"))+
  ylim(0, 1)+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()


##### substrate #####
subavail <- data.frame(Substrate = availdat$Substrate) 
# Create frequency table
subfreqavail <- subavail %>%
  count(Substrate) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

subuse <- data.frame(Substrate = poddat$Substrate) 
# Create frequency table
subfrequse <- subuse %>%
  count(Substrate) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

unique(subfreqavail$Substrate)
unique(subfrequse$Substrate)

freqsubtot <- full_join(subfreqavail, subfrequse, by = "Substrate") 

print(freqsubtot)

freqsubtotwide <- data.frame(Substrate = c("Boulder", "Boulder", "Sand", "Sand",
                                        "Silt", "Silt"),
                          Group = c("Use", "Availability", "Use", "Availability",
                                    "Use", "Availability"), 
                          Frequency = c(0.04761905, 0.02623907, 0.14285714, 0.14868805,
                                        0.80952381, 0.82507289))
grouporder <- c("Use", "Availability")
suborder <- c("Silt", "Sand", "Boulder")
freqsubtotwide$Group <- factor(freqsubtotwide$Group, levels = grouporder)
freqsubtotwide$Cover <- factor(freqsubtotwide$Substrate, levels = coverorder)

ggplot(data=freqsubtotwide, aes(x=Substrate, y=Frequency, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("peachpuff3", "palevioletred"))+
  ylim(0, 1)+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

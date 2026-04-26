library(tidyverse)
library(scales)
library(geomtextpath)
library(colorBlindness)
library(ggridges)
library(dplyr)

poddat <- read.csv("poddatfull.csv", header = TRUE)
poddat <- poddat[-c(9:17, 21, 30:32, 35),] #filter to only live observations

#coverdat <- read.csv("coverdat.csv", header = TRUE)

##### depth and velocity #####
availdat <- read.csv("availabilitydat.csv", header = TRUE)
str(availdat)

availonly <- availdat %>% 
  filter(Type == "Availability")
useonly <- availdat %>% 
  filter(Type == "Use")

##### ks test #####
ks.test(availonly$Depth_m, useonly$Depth_m)

ks.test(availonly$Vel_ms, useonly$Vel_ms)

ggplot(availdat, aes(x = Type, y = Depth_m, fill = Type))+
  geom_violin(linewidth = 0.8, alpha = 0.2)+
  geom_boxplot(width = 0.1)+
  scale_fill_manual(values = c("palevioletred", "peachpuff3"))+
  xlab("")+
  ylab("Depth (m)")+
  coord_flip()+
  theme_bw()


ggplot(useonly, aes(x = Depth_m))+
  geom_histogram(bins = 20, color = "black", fill = "grey")+
  theme_bw()

ggplot(availonly, aes(x = Depth_m))+
  geom_histogram(bins = 20, color = "black", fill = "grey")+
  theme_bw()

ggplot(availdat, aes(x = Type, y = Vel_ms, fill = Type))+
  geom_violin(linewidth = 0.8, alpha = 0.2)+
  geom_boxplot(width = 0.1)+
  scale_fill_manual(values = c("palevioletred", "peachpuff3"))+
  xlab("")+
  ylab("Velocity (m/s)")+
  coord_flip()+
  theme_bw()



ggplot(availonly, aes(x = Depth_m))+
  geom_histogram(bins = 20, fill = "grey50")+
  geom_vline(xintercept = mean(useonly$Depth_m, na.rm = TRUE), color = "darkcyan")+
  theme_bw()

ggplot(availonly, aes(x = Vel_ms))+
  geom_histogram(bins = 20, fill = "grey50")+
  geom_vline(xintercept = mean(useonly$Vel_m, na.rm = TRUE), color = "darkcyan")+
  theme_bw()


#DEPTH ridgeline:
ggplot(availdat, aes(x = Depth_m, y = Type, height = stat(density))) + 
  geom_density_ridges(stat = "binline", bins = 15, scale = 0.99, fill = "grey50", 
                      draw_baseline = TRUE)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 1))) +
  xlab("Depth (m)")+
  ylab("")+
  theme_ridges(center_axis_labels = TRUE, grid = TRUE)

#VELOCITY ridgeline:
ggplot(availdat, aes(x = Vel_ms, y = Type, height = stat(density))) + 
  geom_density_ridges(stat = "binline", bins = 15, scale = 0.99, fill = "grey50", 
                      draw_baseline = TRUE)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 1))) +
  xlab("Velocity (m/s)")+
  ylab("")+
  theme_ridges(center_axis_labels = TRUE, grid = TRUE)

##### cover #####
mypal <- c("peachpuff3", "honeydew3", "palevioletred", "salmon", "darkcyan", "darkseagreen", "darkorange3", "plum3")

unique(availdat$Cover)

coveravail <- data.frame(Cover = availonly$Cover) 
# Create frequency table
coverfreqavail <- coveravail %>%
  count(Cover) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

coveruse <- data.frame(Cover = useonly$Cover) 
# Create frequency table
coverfrequse <- coveruse %>%
  count(Cover) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

unique(coverfreqavail$Cover)
unique(coverfrequse$Cover)

add_row(coverfrequse, Cover = "Submersed Vegetation", n = 0, Proportion = 0)

freqtot <- full_join(coverfreqavail, coverfrequse, by = "Cover") 

print(freqtot)

freqtotwide <- data.frame(Cover = c("Boulder", "Boulder", "Coarse Woody Debris", "Coarse Woody Debris",
                                    "Emergent Vegetation", "Emergent Vegetation", "None", 
                                    "None", "Undercut Bank", "Undercut Bank", "Overhanging Vegetation",
                                    "Overhanging Vegetation", "Submersed Vegetation", "Submersed Vegetation"),
                          Group = c("Use", "Availability", "Use", "Availability", "Use", "Availability", 
                                    "Use", "Availability", "Use", "Availability", "Use", "Availability", 
                                    "Use", "Availability"),
                          Frequency = c(0.04761905, 0.018947368, 0.09523810, 0.018947368,
                                        0.09523810, 0.128421053, 0.38095238, 0.747368421,
                                        0.14285714, 0.002105263, 0.23809524, 0.065263158, 0,
                                        0.018947368))


grouporder <- c("Use", "Availability")
coverorder <- c("Boulder", "Coarse Woody Debris", "Emergent Vegetation",
                "Overhanging Vegetation", "Submersed Vegetation", "Undercut Bank", "None")
freqtotwide$Group <- factor(freqtotwide$Group, levels = grouporder)
freqtotwide$Cover <- factor(freqtotwide$Cover, levels = coverorder)

ggplot(data=freqtotwide, aes(x=Cover, y=Frequency, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("darkgrey", "black"))+
  ylim(0, 1)+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()


##### substrate #####
subavail <- data.frame(Substrate = availonly$Substrate) 
# Create frequency table
subfreqavail <- subavail %>%
  count(Substrate) %>%
  mutate(Proportion = prop.table(n)) # Add proportions

subuse <- data.frame(Substrate = useonly$Substrate) 
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
                          Frequency = c(0.04761905, 0.01684211, 0.14285714, 0.12000000,
                                        0.80952381, 0.86315789))
grouporder <- c("Use", "Availability")
suborder <- c("Silt", "Sand", "Boulder")
freqsubtotwide$Group <- factor(freqsubtotwide$Group, levels = grouporder)
freqsubtotwide$Cover <- factor(freqsubtotwide$Substrate, levels = coverorder)

ggplot(data=freqsubtotwide, aes(x=Substrate, y=Frequency, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("darkgrey", "black"))+
  ylim(0, 1)+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()


##### fisher test #####
# Sediment
colnames(availdat)
sedimentdata <- availdat %>% select(Substrate, Type)

#contingency table
contingency_table_sed <- table(sedimentdata$Substrate, sedimentdata$Type)
print(contingency_table_sed)

#Fisher test on cont table
fisher_test_sed <-  fisher.test(contingency_table_sed)

print(fisher_test_sed)

#Cover
colnames(availdat)
coverdata <- availdat %>% select(Cover, Type)

unique(coverdata$Cover)

coverdata[coverdata$Cover %in% "Emergent Vegetation",]$Cover <- "Eveg"
coverdata[coverdata$Cover %in% "Misc",]$Cover <- "None"
coverdata[coverdata$Cover %in% "Overhanging Vegetation",]$Cover <- "Rveg"
coverdata[coverdata$Cover %in% "Overhanging Bank",]$Cover <- "Ubank"
coverdata[coverdata$Cover %in% "Coarse Woody Debris",]$Cover <- "CWD"
unique(coverdata$Cover)

#contingency table
contingency_table_cov <- table(coverdata$Cover, coverdata$Type)
print(contingency_table_cov)


#Fisher test on cont table
fisher_test_cov <-  fisher.test(contingency_table_cov)

print(fisher_test_cov)


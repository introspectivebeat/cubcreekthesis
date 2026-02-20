library(tidyverse)
library(scales)
library(geomtextpath)
library(colorBlindness)
poddat <- read.csv("poddatfull.csv", header = TRUE)
poddat <- poddat[-c(9:17, 21, 29:33),]

coverdat <- read.csv("coverdat.csv", header = TRUE)

##### depth and velocity #####
availdat <- read.csv("availabilitydat.csv", header = TRUE)
availdat <- availdat[c(1:250),]

ggplot(availdat, aes(x = Depth_ft))+
  geom_histogram(color = "grey25", fill = "peachpuff2")+
  geom_vline(xintercept = 1.4, color = "darkorange3", linewidth = 1.5)+
  geom_vline(xintercept = 0.6, color = "darkcyan", linewidth = 1.5)+
  geom_vline(xintercept = 2.8, color = "darkcyan", linewidth = 1.5)+
  xlab("Depth (ft)")+
  ylab("Transect Point Counts")+
  theme_bw()

ggplot(availdat, aes(x = Velocity_ft_s))+
  geom_histogram(color = "black", fill = "peachpuff2")+
  geom_vline(xintercept = 0.178, color = "darkorange3", linewidth = 1.5)+
  geom_vline(xintercept = -0.071, color = "darkcyan", linewidth = 1.5)+
  geom_vline(xintercept = 0.639, color = "darkcyan", linewidth = 1.5)+
  xlab("Velocity (ft/s)")+
  ylab("Transect Point Counts")+
  theme_bw()

##### cover #####
mypal <- c("peachpuff2", "honeydew3", "palevioletred", "salmon", "darkcyan", "darkseagreen", "darkorange3", "plum3")

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

add_row(coverfrequse, Cover = "CWD", n = 0, Proportion = 0)
add_row(coverfreqavail, Cover = "Obank", n = 0, Proportion = 0)

freqtot <- full_join(coverfreqavail, coverfrequse, by = "Cover") 

print(freqtot)

freqtotwide <- data.frame(Cover = c("Boulder", "Boulder", "Coarse Woody Debris", 
                                    "Coarse Woody Debris", "Emergent Vegetation", 
                                    "Emergent Vegetation", "None", "None", "Overhanging Vegetation",
                                    "Overhanging Vegetation", "Submersed Vegetation", 
                                    "Submersed Vegetation", "Overhanging Bank", 
                                    "Overhanging Bank"),
                          Group = c("Use", "Availability", "Use", "Availability",
                                    "Use", "Availability", "Use", "Availability", 
                                    "Use", "Availability", "Use", "Availability",
                                    "Use", "Availability"), 
                          Frequency = c(0.05555556, 0.036, 0, 0.012, 0.11111111, 0.016,
                                         0.38888889, 0.872, 0.27777778, 0.060, 0, 
                                         0.004, 0.16666667, 0))
grouporder <- c("Use", "Availability")
coverorder <- c("Boulder", "Coarse Woody Debris", "Emergent Vegetation", "Overhanging Vegetation", 
                "Submersed Vegetation", "Overhanging Bank", "None")
freqtotwide$Group <- factor(freqtotwide$Group, levels = grouporder)
freqtotwide$Cover <- factor(freqtotwide$Cover, levels = coverorder)

ggplot(data=freqtotwide, aes(x=Cover, y=Frequency, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("peachpuff2", "palevioletred"))+
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
                          Frequency = c(0.05555556, 0.032, 0.11111111, 0.184,
                                        0.83333333, 0.784))
grouporder <- c("Use", "Availability")
suborder <- c("Silt", "Sand", "Boulder")
freqsubtotwide$Group <- factor(freqsubtotwide$Group, levels = grouporder)
freqsubtotwide$Cover <- factor(freqsubtotwide$Substrate, levels = coverorder)

ggplot(data=freqsubtotwide, aes(x=Substrate, y=Frequency, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("peachpuff2", "palevioletred"))+
  ylim(0, 1)+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

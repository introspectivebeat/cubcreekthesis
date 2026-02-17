library(tidyverse)
library(scales)
library(geomtextpath)
poddat <- read.csv("pod_data.csv", header = TRUE)
poddat <- poddat[-c(9:17, 21),]

coverdat <- read.csv("coverdat.csv", header = TRUE)

ggplot(data=coverdat, aes(x=Cover, y=Count, fill = Species)) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

availdat <- read.csv("availabilitydat.csv", header = TRUE)
availdat <- availdat[c(1:250),]

ggplot(availdat, aes(x = Depth_ft))+
  geom_histogram(color = "black", fill = "grey")+
  geom_vline(xintercept = 1.4, color = "orange2", linewidth = 1)+
  geom_vline(xintercept = 0.6, color = "red", linewidth = 1)+
  geom_vline(xintercept = 2.8, color = "red", linewidth = 1)+
  theme_bw()

ggplot(availdat, aes(x = Velocity_ft_s))+
  geom_histogram(color = "black", fill = "grey")+
  geom_vline(xintercept = 0.178, color = "orange2", linewidth = 1)+
  geom_vline(xintercept = -0.071, color = "red", linewidth = 1)+
  geom_vline(xintercept = 0.639, color = "red", linewidth = 1)+
  theme_bw()

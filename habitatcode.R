library(tidyverse)
library(scales)
poddat <- read.csv("pod_data.csv", header = TRUE)
poddat <- poddat[-c(9:17, 21),]

coverdat <- read.csv("coverdat.csv", header = TRUE)

ggplot(data=coverdat, aes(x=Cover, y=Count, fill = Species)) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = label_wrap(5))+
  theme_bw()

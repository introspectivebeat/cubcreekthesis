library(tidyverse)

precipdata <- read.csv("precipdat_may2025feb2026.csv")

str(precipdata)

precipdata$Date <- as.Date(precipdata$Date, format = "%m/%d/%y")

str(precipdata)

precipdatamm <- precipdata

precipdatamm$Rain_mm <- precipdata$Rain..S.RGE.21198201.21166249.1..in.CC.WS2*25.4

daily_precip_summary <- precipdatamm %>%
  group_by(date = date(Date)) %>% # Extract the date part and group by it
  summarize(Daily_Sum = sum(Rain_mm, na.rm = TRUE)) # Calculate the sum for each day

# View the result
print(daily_precip_summary)

eventpoints <- date(c("2025-06-21", "2025-07-08", "2025-07-10", "2025-12-10", 
                      "2025-12-31", "2026-01-07", "2026-01-27", "2026-02-09"))

event_df <- data.frame(Date = eventpoints)

event_marks <- precipdatamm[precipdatamm$Date %in% eventpoints, ]

ggplot(daily_precip_summary, aes(x = date, y = Daily_Sum))+
  geom_bar(stat = "identity")+
  xlab("Date")+
  ylab("Precipitation (mm)")+
  scale_x_date(date_minor_breaks = "1 day")+
  theme_bw()


ggplot(precipdatamm, aes(x = Date, y = Rain_mm)) +
  geom_line(group = 1) + 
  # Use the filtered data frame here
  labs(
    x = "Date and Time",
    y = "Measured Value"
  ) +  
  scale_x_date(date_minor_breaks = "1 day") +
  theme_bw()

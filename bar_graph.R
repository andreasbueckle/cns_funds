library(tidyverse)
library(lubridate)

# Get this file from Andi as needed
raw <- read_csv("data/HBG-Funding - July 1, 2022.csv")

renamed <- raw %>%
  rename(
    total = `total$`,
    cns = `cns$`,
    remaining = `remaing$on`
  )
renamed



data <- renamed %>%
  mutate(
    remaining_scaled = remaining / 1000,
    days_remaining = as.double(difftime(mdy(end),ymd("2022-7-1"), units = "days")),
    start_date_UNIX = as.numeric(as.POSIXct(mdy(renamed$start))),
    end_date_UNIX = as.numeric(as.POSIXct(mdy(renamed$end)))
  ) %>%
  filter(
    remaining_scaled != 0
  )
data

data = data %>% 
  filter(!is.na(project))
data

# ggplot(data, aes(x = project, y=days_remaining, fill = remaining_scaled)) +
  
  ggplot(data, aes(y = project, x=as.Date("2022-7-1") + days_remaining, fill = remaining_scaled)) +  
  scale_fill_distiller(palette = "RdPu", direction=1) +
  coord_cartesian(xlim=c(as.Date("2022-7-1"), as.Date("2027-7-1")))+
  geom_bar(stat= "identity",width =.5) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "2 week",
               date_labels = "%b %Y")+
  theme_minimal() +
  ylab("Days remaining") +
  xlab("Project") +
  labs(fill = "Remaining funds in 1000$") +
  ggtitle("Remaining CNS Funds as of July 1, 2022") +
  geom_text(aes(label = paste(remaining_scaled, "$")), nudge_x = 5, size = 3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
  ggplot(data, aes(x = as_datetime(start_date_UNIX), xend = as_datetime(end_date_UNIX), y = project, yend = project, color = remaining_scaled)) +
    geom_segment(stat = "identity", size = log(data$remaining_scaled )) +
    scale_fill_distiller(palette = "RdPu", direction=1) +
    theme_minimal() +
    ylab("Project") +
    xlab("Date") +
    geom_vline(xintercept = as_datetime(as.numeric(as.POSIXct(ymd(Sys.Date())))), color = "red", size = 1.2, alpha = 0.5) +
    labs(color = "Remaining funds in 1000$") +
    ggtitle("Remaining CNS Funds") +
    theme(legend.position = "right") +
    geom_label(size = 2, label = paste(data$remaining_scaled,"K") , x = as_datetime((data$end_date_UNIX + data$start_date_UNIX) / 2))
  
  
  
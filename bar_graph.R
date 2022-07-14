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
    days_remaining = as.double(difftime(mdy(end),ymd("2022-7-1"), units = "days"))
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
.
  
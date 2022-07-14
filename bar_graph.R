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
    days_remaining = yday(mdy(renamed$end))
  ) %>%
  filter(
    remaining_scaled != 0
  )
data

renamed

# ggplot(data, aes(x = project, y=days_remaining, fill = remaining_scaled)) +
  
  ggplot(data, aes(x = project, y=(ymd("2022-07-01") + days_remaining), fill = remaining_scaled)) +  
  
  # scale_fill_distiller(palette = "RdPu", direction=-1) +
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_gradient(low="blue", high="red")+
  # scale_fill_brewer(direction=-1)+
  geom_bar(stat= "identity",width =.5) +
    ylim(ymd("2022-07-01"), ymd("2023-07-01"))+

  coord_flip() +
  theme_minimal() +
  ylab("Date") +
  xlab("Project") +
  labs(fill = "Remaining funds in 1000$") +
  ggtitle("Remaining CNS Funds") +
  geom_text(aes(label = paste(remaining_scaled, "$")), nudge_y = 15, size = 3)

library(tidyverse)
library(lubridate)

# Get this file from Andi as needed
raw <- read_csv("HBG-Funding - Feb2, 2022.csv")

renamed <- raw %>%
  rename(
    total = `total$`,
    cns = `cns$`,
    remaining = `remaing$on`
  )
renamed

data <- renamed %>%
  select(
    -c("...8", "...9")
  ) %>%
  mutate(
    remaining_scaled = remaining / 1000,
    days_remaining = yday(mdy(renamed$end))
  )
data


ggplot(data, aes(x = project, y = remaining_scaled, fill = days_remaining)) +
  scale_fill_distiller(palette = "RdPu") +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Remaining funds in 1000 USD") +
  xlab("Project") +
  labs(fill = "Remaining days of funding") +
  ggtitle("Remaining CNS Funds") +
  geom_text(aes(label = remaining_scaled), nudge_y = 15, size = 3)

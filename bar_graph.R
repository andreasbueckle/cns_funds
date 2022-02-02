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
  ) %>%
  filter(
    remaining_scaled != 0
  )
data

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(colour = factor(cyl))) +
  scale_colour_brewer(palette = "BuPu")

ggplot(data, aes(x = project, y = days_remaining, fill = remaining_scaled)) +
  # scale_fill_distiller(palette = "RdPu", direction=-1) +
  scale_fill_brewer(palette = "Dark2") +
  # scale_fill_brewer(direction=-1)+
  geom_bar(stat = "identity", width = data$remaining_scaled / 1000) +
  coord_flip() +
  theme_minimal() +
  ylab("Remaining days to project end") +
  xlab("Project") +
  labs(fill = "Remaining funds") +
  ggtitle("Remaining CNS Funds") +
  geom_text(aes(label = paste(remaining_scaled, "$")), nudge_y = 15, size = 3)

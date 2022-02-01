library(tidyverse)

# Get this file from Andi as needed
raw = read_csv("HBG-Funding - Sheet1.csv")

renamed = raw %>% 
  rename(
  total = `total$`,
  cns = `cns$`,
  remaining = `remaing$on`
) %>% 
  mutate(
    remaining = total - cns
  )
renamed

data = renamed %>% 
  filter(!is.na(remaining) & remaining != 0) %>% 
  mutate(
    remaining_scaled = remaining/1000
  )
data

  ggplot(data, aes(x = project, y = remaining_scaled, fill = project)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Remaining funds in 1000 USD")+
  xlab("Project")+
  ylim(0, max(data$remaining_scaled))+
  labs(fill = "Project")+
  ggtitle("Remaining CNS Funds")
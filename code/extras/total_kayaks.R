


library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)



kayaks <- readRDS(here("data_files/working_rds/long_tallies_from_raw")) %>% 
  filter(species == "KAYAK") %>% 
  group_by(date) %>% 
  summarise(total.kayaks = sum(tally)) %>% 
  mutate(season = ifelse(month(date) == 12, year(date), year(date) - 1))


ggplot(kayaks, aes(x = season, y = total.kayaks)) +
  geom_point() +
  stat_smooth(color = "blue", fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", fill = "red", alpha = 0.25) +
  theme_bw() +
  scale_x_continuous(minor_breaks = seq(2000, 2023, by = 1)) +
  labs(title = "Total kayaks observed each waterbird survey on Tomales Bay",
       y = "Total kayaks",
       x = "Season")

ggsave(here("figures_output/total_kayaks.png"))

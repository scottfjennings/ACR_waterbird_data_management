

## basic waterbirds summaries, initially created Jan 2019 for David Whimpheimer data request

## uses WATERBIRDS_full_long.csv, which is created by make_new_waterbirds_dbase.R

library(tidyverse)
library(lubridate)
options("scipen"=999)


wbirds_seas_surv <-  wbird %>% 
  group_by(species, survey.num, season) %>% 
  summarise(summed.num = sum(count)) %>% 
  ungroup()

wbirds_seas_surv %>% 
  filter(survey.num == 1) %>% 
  rename(Total.count.spp.seas = summed.num) %>% 
  write.csv(., "data_files/derived_data/Waterbird_CBC_total_spp_seas.csv", row.names = F)

wbirds_seas_surv %>% 
  filter(survey.num == 1) %>% 
  group_by(season) %>% 
  summarise(Total.count.seas = sum(summed.num)) %>% 
  write.csv(., "data_files/derived_data/Waterbird_CBC_total_seas.csv", row.names = F)

wbirds_seas_surv %>% 
  filter(survey.num == 1) %>% 
  group_by(species) %>% 
  summarise(Total.count.seas = sum(summed.num)) %>% 
  write.csv(., "data_files/derived_data/Waterbird_CBC_total_seas.csv", row.names = F)

#--
wbird_summ_spp_seas <- wbirds_seas_surv %>% 
  group_by(species, season) %>% 
  summarise(mean.num = round(mean(summed.num), 2),
            max.num = max(summed.num),
            min.num = min(summed.num)) %>% 
  ungroup() %>% 
  arrange(species, season)

wbird_summ_spp_seas %>% 
  rename(Mean.spp.seas.count = mean.num, Max.spp.seas.count = max.num, Min.spp.seas.count = min.num) %>% 
  write.csv(., "data_files/derived_data/Waterbird_mean_spp_seas_counts.csv", row.names = F)
#--
wbird_summ_spp <- wbird_summ_spp_seas %>% 
  group_by(species) %>% 
  summarise(spp.mean.num = round(mean(mean.num), 2),
            spp.max.num = max(mean.num),
            spp.min.num = min(mean.num)) 
  
wbird_summ_spp %>% 
  rename(Mean.spp.count = spp.mean.num, Max.spp.count = spp.max.num, Min.spp.count = spp.min.num) %>% 
  write.csv(., "data_files/derived_data/Waterbird_mean_spp_counts.csv", row.names = F)
#--
wbirds_seas_summed <- wbird %>% 
  group_by(date) %>% 
  summarise(summed.num = sum(count)) %>% 
  mutate(season = ifelse(month(date) > 6, year(date), year(date) - 1)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  summarise(seas.mean.num = round(mean(summed.num), 2),
            seas.max.num = max(summed.num),
            seas.min.num = min(summed.num)) 
  
wbird_summ_spp %>% 
  rename(Mean.spp.count = spp.mean.num, Max.spp.count = spp.max.num, Min.spp.count = spp.min.num) %>% 
  write.csv(., "data_files/derived_data/Waterbird_mean_spp_counts.csv", row.names = F)

wbird_trend2 <- wbird_summ %>% 
  group_by(species) %>% 
  summarise(trend = round(coef(lm(mean.num ~ season))[2], 2),
            upper95ci = round(confint(lm(mean.num ~ season))[2,2], 2),
            lower95ci = round(confint(lm(mean.num ~ season))[2,1], 2))
          

write.csv(wbird.summ, "wbird_all_spp_basic_trends/wbird_trends_summary.csv", row.names = F)


wbird.plotter <- function(spp){
foo <- wbirds.date.summed %>% 
  filter(species == spp)
  
  ggplot(data = foo, aes(x = season, y = summed.num)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Number of individuals") +
  ggtitle(spp)

ggsave(paste("wbird_all_spp_basic_trends/", spp, "_plot.jpg", sep = ""), width = 5, height = 5, units = c("in"))  
}
wbird.plotter("BUFF")


map(wbirds.spp.list, wbird.plotter)

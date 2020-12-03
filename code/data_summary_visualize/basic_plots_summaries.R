

###
library(tidyverse)
options(scipen = 999)


source("code/waterbird_utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.r")

# read cleaned data
wbirds4analysis <- readRDS("data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day() 



# sum baywide abundance for each species and survey date, and for all species combined, then combine those dfs

make_baywide_season_survey <- function(zwbirds4analysis) { 
wbird_baywide_spp_season_survey <- wbirds4analysis %>% 
  group_by(study.year, date, alpha.code) %>% 
  summarise(baywide.count = sum(section.final)) %>% 
  arrange(study.year, date, alpha.code) %>% 
  ungroup()

wbird_baywide_all_season_survey <- wbirds4analysis %>% 
  group_by(study.year, date) %>% 
  summarise(baywide.count = sum(section.final)) %>% 
  arrange(study.year, date) %>% 
  ungroup() %>% 
  mutate(alpha.code = "all")

wbird_baywide_season_survey <- rbind(wbird_baywide_all_season_survey, wbird_baywide_spp_season_survey)
}


wbird_baywide_season_survey <- make_baywide_season_survey(wbirds4analysis)



# extract CBC data
wbird_baywide_season_survey %>% 
  filter(survey.num == 1) %>% 
  select(-survey.num) %>% 
  write.csv(., "data_files/derived_data/waterbird_CBC_species_totals_by_year.csv", row.names = F)

# baywide mean birds per season (mean across surveys)

wbird_baywide_mean_season <- wbird_baywide_season_survey %>% 
  group_by(study.year, alpha.code) %>% 
  summarise(mean.abund = mean(baywide.count),
            med.abund = median(baywide.count)) %>% 
  ungroup()

wbird_abund_rank <- wbird_baywide_mean_season %>% 
  filter(alpha.code != "all") %>% 
  arrange(study.year, -mean.abund) %>% 
  group_by(study.year) %>% 
  mutate(abund.rank = row_number()) %>% 
  ungroup()


wbird_5most_abund_others <- wbird_abund_rank %>% 
  mutate(abund.rank.spp = ifelse(abund.rank <=5, alpha.code, "all.others")) 


wbird_raw_means_plotter <- function(zspp) {
wbird_baywide_mean_season %>% 
    filter(alpha.code == zspp) %>% 
  pivot_longer(cols = contains("abund"), names_to = "mean.median") %>% 
  mutate(mean.median = ifelse(mean.median == "mean.abund", "Mean", "Median")) %>% 
 ggplot() +
   geom_point(aes(x = season, y = value, color = mean.median)) +
  stat_smooth(aes(x = season, y = value, color = mean.median)) + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  ylab("Number of individuals per survey") +
  scale_x_continuous(limits = c(1989, 2018), breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  xlab("") +
    ggtitle(zspp)
  ggsave(paste("figures_output/raw_data_plots/", zspp, "_raw.png", sep = ""), height = 6, width = 8)

}


wbird_raw_means_plotter("SUSC")
map(top5spp_each_year$abund.rank.spp, wbird_raw_means_plotter)

ggsave("figures_output/raw_data_plots/all_spp_raw.png", height = 6, width = 8)


#

# stacked bar charts ----
# these not working great because too many species are in top 5 most abundant or 90% cumulative abundance; needs work
# 5 most abundant species, each year

wbird_abund_rank <- wbird_baywide_mean_season %>% 
  filter(alpha.code != "all") %>% 
  arrange(season, -mean.abund) %>% 
  group_by(season) %>% 
  mutate(abund.rank = row_number()) %>% 
  ungroup()


wbird_5most_abund_others <- wbird_abund_rank %>% 
  mutate(abund.rank.spp = ifelse(abund.rank <=5, alpha.code, "all.others")) %>% 
  group_by(season, abund.rank.spp) %>% 
  summarise(abundance = sum(mean.abund)) %>% 
  ungroup()

top5spp_each_year <- wbird_5most_abund_others %>% 
  filter(abund.rank.spp != "all.others") %>% 
  distinct(abund.rank.spp) %>% 
  data.frame()

ggplot(wbird_5most_abund_others, aes(fill=abund.rank.spp, y=abundance, x=season)) + 
    geom_bar(position="stack", stat="identity") +
  theme_classic() +
  xlab("Year") +
  ylab("Total birds counted") +
  theme(legend.title = element_blank()) +
  #scale_fill_grey(start = 0, end = 0.8) +
  scale_fill_brewer(palette = "Set2", aesthetics = "fill") +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  ggtitle("Figure 2")




# spp composing 90% of birds counted  

year_tot_obs <- wbird_baywide_mean_season %>% 
  filter(alpha.code != "all") %>% 
  group_by(season, alpha.code) %>% 
  summarise(total = sum(mean.abund)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  arrange(season, -total) %>% 
  mutate(cumu.sum = cumsum(total)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  mutate(year.sum = sum(total),
         year.cum.prop = cumu.sum/year.sum,
         year.prop = total/year.sum) %>% 
  ungroup()

year_tot_9 <- year_tot_obs %>% 
  filter(year.cum.prop >= 0.9) %>% 
  group_by(season) %>% 
  filter(year.cum.prop == min(year.cum.prop)) %>% 
  dplyr::select(season, min.over90 = year.cum.prop)


year_tot_obs2 <- year_tot_obs %>% 
  full_join(., year_tot_9, by = c('season')) %>% 
  mutate(alpha.code2 = ifelse(year.cum.prop > min.over90 | alpha.code %in% c("DUCK"), "Remaining spp", as.character(alpha.code))) %>% 
  group_by(season, alpha.code2) %>% 
  summarise(total2 = sum(total)) %>% 
  arrange(season, -total2) %>% 
  ungroup() %>% 
  rename(alpha.code = alpha.code2) %>% 
  bird_taxa_filter(join_taxa = c("alpha.code", "alpha.code")) %>% 
  mutate(common.name = ifelse(is.na(common.name), "Remaining spp.", common.name)) 

year_tot_obs2$common.name <- factor(year_tot_obs2$common.name, c("Dunlin", "Least Sandpiper", "Marbled Godwit", "Western Sandpiper", "Sanderling", "Willet", "Dowitcher spp.", "Remaining spp."))
 

ggplot(year_tot_obs2, aes(fill=common.name, y=total2, x=season)) + 
    geom_bar(position="stack", stat="identity") +
  theme_classic() +
  xlab("Year") +
  ylab("Total birds counted") +
  theme(legend.title = element_blank()) +
  #scale_fill_grey(start = 0, end = 0.8) +
  #scale_fill_brewer(palette = "Set2", aesthetics = "fill") +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  ggtitle("Figure 2")

ggsave("figures_output/species_proportions_color.png", width = 7, height = 5, units = "in", dpi = 300)


 
   


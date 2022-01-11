

library(tidyverse)
library(lubridate)
library(birdnames)
library(here)
library(RODBC)

useless_groupies <- c("AMCOGRSCLESCBUFF", "COMERBME", "GOOSE", "MERG", "MURRELET", "SWAN", "UNTE", "DUCK")

useful_groupies <- c("LOON", "RTPALO", "CORM", "HEGR", "WCGR", "PCLO")

options("scipen"=999)
source(here("code/utils.r"))
source(here("code/waterbird_cleaning2_split_groups.r"))

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/birdnames_support/data/custom_bird_list")

wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")

db <- "C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/waterbirds_v2.0.accdb"

spp_table <- read_species_table(db) 

wbirds <- query_waterbirds(db) %>% 
  clean_waterbirds() %>% 
  sppindex_to_alpha(spp_table = spp_table) %>%
  mutate(alpha.code = update_alpha(alpha.code)) %>% # from birdnames
  fix_precount_block_names() %>% 
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% # from birdnames
  separate(block, into = c("transect", "section"), remove = F) %>% 
  mutate(section = gsub("sec", "", section))  %>% 
  select(date, block, section, transect, alpha.code, count) # we only need a few columns


negs <- read.csv(here("data_files/negatives.csv")) 

test_negs <- negs %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         block = gsub("e.m|w.m", "m", block)) %>% 
  group_by(date, block, alpha.code) %>% 
  summarise(negative = sum(negative, na.rm = TRUE),
            positive = sum(positive, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(alpha.code = update_alpha(alpha.code))%>% 
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% 
  left_join(., wbirds)
  

test_negs_wide <- test_negs %>% 
  pivot_wider(id_cols = c(date, transect, alpha.code), names_from = section, values_from = c(negative, positive, count)) %>% 
  select(date, alpha.code, transect, contains("_1"), contains("_2"), contains("_3"), contains("_4")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(transect = factor(transect, levels = c("west", "middle", "east"))) %>% 
  arrange(date, alpha.code, transect)



#-----

wide_blocks <- wbirds %>% 
  widen_block_tallies()  %>%
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp"))

pooled_dates <- wide_blocks %>%
  distinct(date, alpha.code, group.spp) %>% 
  filter(!is.na(group.spp)) %>% 
  separate(group.spp, c(paste("group.spp", seq(1, 4)))) %>% 
  pivot_longer(contains("group.spp")) %>% 
  rename(pooled.alpha.code = alpha.code, alpha.code = value) %>% 
  filter(!is.na(alpha.code)) %>%
  mutate(alpha.code = update_alpha(alpha.code)) %>% 
  select(-name)



wide_blocks_pooled <- full_join(wide_blocks, pooled_dates) %>% 
  mutate(pooled.alpha.code = ifelse(!is.na(group.spp), alpha.code, pooled.alpha.code)) %>% 
  arrange(date, pooled.alpha.code, transect)


working <- wide_blocks_pooled %>% 
  group_by(date, )



date_spp_transect <- pooled_dates %>% 
  distinct(date, alpha.code) %>% 
  filter(!is.na(alpha.code)) %>%
  mutate(alpha.code = update_alpha(alpha.code)) %>% 
  uncount(7) %>% 
  group_by(date, alpha.code) %>% 
  mutate(transect.num = row_number()) %>% 
  ungroup() %>% 
  full_join(., data.frame(transect.num = seq(1, 7),
                          transect = c("middle", 
                                       "east", 
                                       "west", 
                                       "inverness", 
                                       "bivalve", 
                                       "millertonbivalve", 
                                       "walkercreek")))


pooled_date_constituents <- wide_blocks %>% 
  filter(is.na(group.spp)) %>% 
  select(-group.spp) %>% 
  full_join(pooled_dates) %>% 
  full_join(date_spp_transect) %>% 
  mutate(across(contains("section."), ~replace_na(., 0))) %>% 
  select(date, transect, alpha.code, pooled.alpha.code, contains(".1"), contains(".2"), contains(".3"), contains(".4")) %>% 
  arrange(date, pooled.alpha.code, alpha.code, transect)


tallies_carried_forward <- pooled_date_constituents %>% 
  mutate(forward.1 = ifelse(section.1 < 0, section.1, 0),
         forward.2 = forward.1 + section.2,
         forward.3 = forward.2 + section.3,
         forward.4 = forward.3 + section.4)%>%
  mutate(departed = ifelse(forward.4 < 0, forward.4, 0)) %>% 
  select(date, transect, alpha.code, pooled.alpha.code, contains(".1"), contains(".2"), contains(".3"), contains(".4"), departed)

  
   %>% 
  mutate(pooled.forward.1 = ifelse(pooled.section.1 < 0, pooled.section.1, 0),
         pooled.forward.2 = pooled.forward.1 + pooled.section.2,
         pooled.forward.3 = pooled.forward.2 + pooled.section.3,
         pooled.forward.4 = pooled.forward.3 + pooled.section.4,
         pooled.departed = ifelse(pooled.forward.4 < 0, pooled.forward.4, 0)) 



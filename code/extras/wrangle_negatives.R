


library(tidyverse)
library(stringr)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source(here("code/waterbird_cleaning3_reconcile_negatives.r"))

wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")

# read negatives, do basic wrangling, join with wbirds ----
negatives <- read.csv(here("data_files/entered_raw/negatives.csv")) %>% 
  mutate(alpha.code = toupper(alpha.code),
         middle.side = ifelse(grepl("middle", transect), transect, NA),
         middle.side = gsub(".middle", "", middle.side),
         transect = ifelse(grepl("middle", transect), "middle", transect),
         date = mdy(date),
         section = as.character(section),
         alpha.code = update_alpha(alpha.code)) %>% 
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% 
  mutate(across(c(negative, positive, emi.xed), ~replace_na(., 0))) %>% 
  arrange(date, section, transect, alpha.code)


# emi records data a little differently, crossing out already entered positive or negative tallies as they get canceled out by new equivalent negative or positive tallies, respectively. any crossed out value on a sheet with emi as recorder is entered as a positive value in emi.xed, and negative and positive need to be adjusted by those values
negatives <- negatives %>%  
  mutate(negative = negative - emi.xed,
         positive = positive + emi.xed) %>% 
  select(-emi.xed, -notes) 




# because the 2 middle transects are combined in the WATERBIRDS db, its easiest to handle side transects and middle transects separately

# first side transects
e_w_negatives <- negatives %>% 
  left_join(readRDS(here("data_files/wbirds"))) %>% 
  select(-block, -middle.side, -middle.sub.total) %>% 
  filter(transect %in% c("east", "west")) %>% 
  mutate(count = replace_na(count, 0),
         calculated.positive = case_when(count > 0 ~ count + abs(negative),
                                         TRUE ~ 0),
         calculated.negative = case_when(count < 0 ~ count - positive,
                                         TRUE ~ negative))

# then middle

mid_neg <- negatives %>% 
  filter(transect == "middle") %>% 
  mutate(across(c(negative, positive), ~replace_na(., 0))) %>% 
  mutate(calculated.positive = case_when(middle.sub.total >= 0 ~ middle.sub.total + abs(negative),
                                         TRUE ~ positive),
         calculated.negative = case_when(middle.sub.total < 0 ~ middle.sub.total - positive,
                                         TRUE ~ negative))

mid_neg_wide <- mid_neg %>% 
  select(date, transect, section, alpha.code, middle.side, calculated.positive, calculated.negative) %>% 
  pivot_longer(cols = c(calculated.positive, calculated.negative)) %>%
  mutate(name = paste(middle.side, name, sep = ".")) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(date, transect, section, alpha.code), names_from = name, values_from = value) %>% 
  left_join(readRDS(here("data_files/wbirds"))) %>% 
  mutate(across(c(contains("calculated"), count), ~replace_na(., 0)),
         calculated.count = (e.calculated.positive + e.calculated.negative) + (w.calculated.positive + w.calculated.negative),
         e.neg = ifelse(e.calculated.positive != 0 | e.calculated.negative != 0, TRUE, FALSE),
         w.neg = ifelse(w.calculated.positive != 0 | w.calculated.negative != 0, TRUE, FALSE),
         check = ifelse(count != calculated.count & e.neg == TRUE & w.neg == TRUE, TRUE, FALSE))

middle_negatives <- mid_neg_wide %>% 
  mutate(calculated.positive = e.calculated.positive + w.calculated.positive,
         calculated.negative = e.calculated.negative + w.calculated.negative) %>% 
  select(date, transect, section, alpha.code, calculated.positive, calculated.negative)


# combine

true_pos_neg <- full_join(e_w_negatives, middle_negatives) %>%
  full_join(., long_tallies_pos_neg) %>% 
  mutate(calculated.positive = ifelse(is.na(calculated.positive), pos.tally, calculated.positive),
         calculated.negative = ifelse(is.na(calculated.negative), neg.tally, calculated.negative)) %>% 
  select(date, transect, section, alpha.code, calculated.positive, calculated.negative) 

zz_split_unsplit_unpooled_block <- readRDS(here("data_files/split_unsplit_unpooled_block")) %>% 
  filter(date %in% distinct(true_pos_neg, date)$date) %>% 
  mutate(section = gsub("sec", "", section)) %>% 
  anti_join(true_pos_neg) %>% 
  select(everything(), -block) %>% 
  rename(calculated.positive = count) %>% 
  mutate(calculated.negative = 0)


pos_neg_block <- true_pos_neg %>% 
  bind_rows(zz_split_unsplit_unpooled_block) 

pos_neg_section <- pos_neg_block %>% 
  group_by(date, alpha.code, section) %>% 
  summarise(positive = sum(calculated.positive),
            negative = sum(calculated.negative)) %>% 
  ungroup() %>% 
  mutate(section = paste("sec", section, sep = ""))

# add date X alpha.code X section combos from database that aren't represented in pos_neg_section


pos_neg_subtracted_forward <- pos_neg_section %>%
  widen_block_pos_neg() %>% 
  subtract_forward_add_back()




saveRDS(pos_neg_subtracted_forward, here("data_files/pos_neg_subtracted_forward"))


subtracted_added_viewer(pos_neg_subtracted_forward, zdate = "2009-02-08", zalpha.code = "BRAC")

subtracted_added_viewer(pos_neg_subtracted_forward, zdate = "2019-01-12", zalpha.code = "DCCO")

subtracted_added_viewer(pos_neg_subtracted_forward, zdate = "2013-02-09", zalpha.code = "BRAC")

subtracted_added_viewer(pos_neg_subtracted_forward, zdate = "2015-02-14", zalpha.code = "BRAC")

subtracted_added_viewer(xx_subtracted_forward, zdate = "2011-12-17", zalpha.code = "DCCO")

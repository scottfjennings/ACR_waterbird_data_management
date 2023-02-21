

library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source(here("code/clean_from_access/waterbird_cleaning2_split_groups.R"))
source(here("code/utils.R"))

# handling grouped species ----

# then go from block sums to section sums
section_sums <- block_sums %>% 
  ungroup() %>% 
  group_by(date, alpha.code, section) %>%
  summarise(section.pos = sum(positive),
            section.neg = sum(negative)) %>% 
  ungroup() %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) # from birdnames



pooled <- section_sums %>% 
  filter(!is.na(group.spp) & !group.spp %in% c("kayak", "casl", "hase"))  %>% 
  distinct(date, alpha.code, group.spp) %>% 
  separate(group.spp, c(paste("group.spp", seq(1, 4)))) %>% 
  pivot_longer(contains("group.spp")) %>% 
  select(date, pooled.alpha.code = alpha.code, alpha.code = value) %>% 
  filter(!is.na(alpha.code)) %>%
  mutate(alpha.code = update_alpha(alpha.code))

pooled_all_sec <- pooled %>% 
  group_by(date, alpha.code) %>% 
  uncount(6) %>% 
  ungroup() %>% 
  mutate(section = rep(c("1", "2a", "2b", "3", "4", "5"), nrow(pooled))) %>% 
  left_join(section_sums %>% select(-group.spp)) %>% 
  arrange(date, section, pooled.alpha.code) %>% 
  mutate(across(c("section.pos", "section.neg"), ~replace_na(.,0)))

constituent_ratios <- pooled_all_sec %>% 
  group_by(date, section, pooled.alpha.code) %>% 
  mutate(section.all.constituents = sum(section.pos)) %>% 
  ungroup() %>% 
  group_by(date, pooled.alpha.code, alpha.code) %>% 
  mutate(spp.bay.total = sum(section.pos)) %>% 
  ungroup() %>% 
  group_by(date, pooled.alpha.code) %>% 
  mutate(bay.all.constituents = sum(section.pos)) %>% 
  ungroup() %>% 
  mutate(section.proportion = section.pos/section.all.constituents,
         bay.proportion = spp.bay.total/bay.all.constituents)



allocated_pooled_block_long <- section_sums %>% 
  filter(!is.na(group.spp)) %>% 
  rename("pooled.alpha.code" = alpha.code,
         "pooled.section.pos" = section.pos,
         "pooled.section.neg" = section.neg) %>% 
  select(-group.spp) %>% 
  pivot_longer(cols = contains("pooled.section"), names_to = "which.tally", values_to = "tally") %>% 
  filter(tally != 0) %>% 
  inner_join(constituent_ratios %>% select(date, alpha.code, section, pooled.alpha.code, section.all.constituents, section.proportion, bay.all.constituents, bay.proportion)) %>% 
  arrange(date, section, pooled.alpha.code, which.tally, alpha.code) %>% 
  mutate(allocated.tally = case_when(tally < section.all.constituents ~ round(tally * section.proportion, 0),
                                     tally >= section.all.constituents & tally < bay.all.constituents ~ round(tally * bay.proportion, 0)),
         allocation.scale = case_when(tally < section.all.constituents ~ "section",
                                     tally >= section.all.constituents & tally < bay.all.constituents ~ "bay",
                                     TRUE ~ "none"))

allocation_report <- allocated_pooled_block_long %>% 
  mutate(pos.neg = ifelse(grepl("pos", which.tally), "positive", "negative"),
         allocation.report = ifelse(allocation.scale != "none", 
                                    paste(allocated.tally, "of the", pos.neg, tally, "birds IDed as", pooled.alpha.code, "were allocated to", alpha.code), 
                                    paste("none of the", pos.neg, tally, "birds IDed as", pooled.alpha.code, "were allocated to", alpha.code, "because there were more", pooled.alpha.code, "than birds IDed to species"))) %>% 
  group_by(date, section, pooled.alpha.code, pos.neg) %>% 
  summarise(allocation.report.out = paste(allocation.report, collapse = ", and ")) %>% 
  ungroup() %>% 
  mutate(allocation.report.out = paste("In section", section, ",", allocation.report.out)) %>% 
  arrange(date, section, pooled.alpha.code, pos.neg)


# combine allocated, unallocated, and unpooled
# first the allocated
# need to sum birds allocated to species from different pooled groups and pivot back to columns for positive and negative
allocated_pooled_block <- allocated_pooled_block_long %>% 
  filter(allocation.scale != "none") %>% 
  group_by(date, section, alpha.code, which.tally) %>% 
  summarise(total.allocated = sum(allocated.tally),
            allocated.from = paste(pooled.alpha.code, collapse = "_")) %>% 
  mutate(which.tally = gsub("pooled.", "", which.tally)) %>% 
  pivot_wider(id_cols = c(date, section, alpha.code, allocated.from), names_from = which.tally, values_from = total.allocated) %>% 
  mutate(across(contains("allocated.sec"), ~replace_na(., 0))) %>% 
  arrange(date, section, allocated.from, alpha.code)


# next unallocated
# mostly just need to manage column names and pivot back to columns for positive and negative
unallocated_pooled_block <- allocated_pooled_block_long %>% 
  filter(allocation.scale == "none") %>% 
  distinct(date, section, pooled.alpha.code, tally, which.tally) %>% 
  mutate(which.tally = gsub("pooled.", "", which.tally)) %>% 
  pivot_wider(id_cols = c(date, section, pooled.alpha.code), names_from = which.tally, values_from = tally) %>% 
  rename(alpha.code = pooled.alpha.code) %>% 
  mutate(allocated.from = "unallocated")



# join allocated to original data
# then finally combine the allocated with the original data
section_sums_allocated <- section_sums %>% 
  filter(is.na(group.spp)) %>% 
  select(-group.spp) %>% 
  bind_rows(allocated_pooled_block) %>% 
  bind_rows(unallocated_pooled_block) %>% 
  mutate(across(c(section.pos, section.neg), ~replace_na(., 0))) %>% 
  arrange(date, section, alpha.code)
  
  


# and finally combine birds ID as species and those allocated by ratio from pooled, and select just the needed columns

data_with_allocated <- section_sums_allocated %>% 
  group_by(date, section, alpha.code) %>% 
  summarise(section.positive = sum(section.pos),
         section.negative = sum(section.neg)) %>% 
  ungroup() %>% 
  arrange(date, section, alpha.code)



##

# The logic of splitting is as follows:  

# * There are several species of waterbirds that may not be identified to species but that can be assigned to a POOLED group (e.g. LOON, CORM). # For each pooled group on each day, we want to calculate the ratios of constituent species in that group (e.g. ratio of DCCO:BRAC:PECO on a given day that some birds were assigned to CORM). We need to do this at the section and bay scales.
 

# * POOLED birds are then allocated to the constituent species of that POOLED group based on the ratios of positively identified birds in each constituent species. Allocation is only done if the number of positively identified birds in the group is greater than the number of undifferentiated birds.

# * The bay is divided into 4 sections, numbered from south to north

# * If there are enough identified birds at the section scale, then the ratio of IDed birds in each species counted in that section is used to divvy the lumped birds. The group cutoff value is 50 for grebes, 100 for all other groups.

# * If there are not enough IDed birds in the Section, then the ratio for IDed birds across the entire Bay is used. NOTE: IT APPEARS KELLY AND TAPPAN 1998 DID NOT DO THIS EXTRA STEP. DOING THIS EXTRA STEP INCLUDES MORE INDIVIDUAL BIRDS, BUT AT THE COST OF MORE LIBERAL ASSUMPTIONS ABOUT RATIOS OF IDENTIFIED AND UNIDENTIFIED BIRDS BEING CONSTANT THROUGHOUT THE ENTIRE BAY. THERE ARE AT LEAST SOME OCCASIONS WHERE THE CONSTITUENT SPP RATIO IN THE SECTION WITH POOLED BIRDS IS QUITE DIFFERENT THAN THE RATIO IN THE WHOLE BAY (E.G., ) THE FOLLOWING CODE INDICATES WHETHER AN ALLOCATION WAS MADE BASED ON SECTION OR BAY RATIOS, SO THAT THE USER CAN DECIDE WHICH DATA TO INCLUDE IN ANALYSIS

# Unless otherwise indicated, the functions are defined in waterbird_cleaning2_split_groups.R.  



library(tidyverse)
library(stringr)
library(lubridate)
library(here)
library(birdnames)

# we use the same logic to split groups birds for cleaning_from_access and cleaning_from_raw_tallies,
# so some functions written for clean_from_access can be used here too
# source those functions
source(here("code/clean_from_access/waterbird_cleaning2_split_groups.R"))

options(scipen = 999)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")
# wbird_keep_taxa_gulls <- c("Anseriformes", "Alcidae", "Laridae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Suliformes")






# The first part of step 2 is to sum the number of birds in each of the four sections of the bay. In the final mutate() line here we add a column "group.spp" for constituent species in each POOLED group, if applicable.  
# sum_section_tallies_finals() incorporates birds counted in the precount zones into the appropriate sections.





# from 1_wrangle_tallies.R
# has the total positive and negative tallies for each block
block_sums <- readRDS(here("data_files/block_sums")) %>% 
  rename("alpha.code" = species) %>% # for compatability with birdnames
  bird_taxa_filter(keep_taxa = wbird_keep_taxa)

# need to go from block sums to section sums
section_sums <- block_sums %>% 
  ungroup() %>% 
  mutate(section = ifelse(section == 5, 4, section)) %>% 
  group_by(date, alpha.code, section) %>%
  summarise(section.pos = sum(positive),
            section.neg = sum(negative)) %>% 
  ungroup() %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) # from birdnames
  


# a difference between access and raw tally data is that in the later we still have true positive and negative tallies separated.
# we want to use the positives plus absolute value of the negatives to calculate bay proportions of positively IDed birds
# we assign that to "section.tally" to make names match those needed by functions from clean_from_access/waterbird_cleaning2_split_groups.R
constituent_ratios <- section_sums %>% 
  make_pooled_date_constituents() %>% 
  ungroup() %>% 
  mutate(section.tally = section.pos + abs(section.neg)) %>% 
  calc_section_constituent_sum_proportion() %>% 
  calc_bay_constituent_sum_proportion()




# And then we peel off just the POOLED bird data from wbirds, join with the ratio table, and allocate POOLED birds to constituent species based on the appropriate ratio.

# can't use get_pooled_block() and allocate_pooled_block() because now want to handle positive and negative tallies separately
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
         allocation.report = paste(allocated.tally, "of the", pos.neg, tally, "birds IDed as", pooled.alpha.code, "were allocated to", alpha.code)) %>% 
  group_by(date, section, pooled.alpha.code) %>% 
  summarise(allocation.report.out = paste(allocation.report, collapse = ", and ")) %>% 
  ungroup() %>% 
  mutate(allocation.report.out = paste("In section", section, ",", allocation.report.out))

write.csv(allocation_report, here("data_files/entered_raw_combined/allocation_report.csv"), row.names = FALSE)


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
  
  


# and finally select just the needed columns

data_with_allocated <- section_sums_allocated %>% 
  group_by(date, section, alpha.code) %>% 
  summarise(section.positive = sum(section.pos),
         section.negative = sum(section.neg)) %>% 
  arrange(date, section, alpha.code)


saveRDS(data_with_allocated, here("data_files/entered_raw_combined/data_with_allocated"))

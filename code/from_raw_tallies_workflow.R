



library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source(here("code/clean_from_raw_tallies/1_wrangle_tallies.R"))
source(here("code/clean_from_access/waterbird_cleaning2_split_groups.R"))
source(here("code/clean_from_raw_tallies/3_handle_negatives.R"))
source(here("code/utils.R"))


wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")
wbird_keep_taxa_gulls <- c("Anseriformes", "Alcidae", "Laridae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Suliformes")

# for tallies entered one per row ----
# pattern = "_p" gets only sheets that have been proofed
tally_files = list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data", full.names = TRUE, pattern = "_p") %>%
   stringr::str_subset(., "template", negate = TRUE)


# wrangle tallies ----

# read_raw_tallies(tally_files[1]) %>% view()

# to process all raw data 
system.time(
long_tallies <- map_df(tally_files, read_raw_tallies)
)

# to process a single day of data
long_tallies <- read_raw_tallies(tally_files[66])




# change starboard and port to east and west
long_tallies2 <- long_tallies %>% 
  mutate(block = gsub("starboard", "e", block),
         block = gsub("port", "w", block))


wbird_clean <- long_tallies2 %>% 
  filter(block != "mid.bay.w.1.and.2") %>% 
  mutate(block = gsub("mid.bay.", "middle_", block),
         block = gsub("\\.\\.", "\\.", block),
         block = ifelse(grepl("cypress|millerton|walker", block), gsub("\\.", "", block), block)) %>% 
  separate(block, c("transect", "section"), remove = FALSE, sep = "\\.") %>% 
  mutate(section = case_when(transect == "cypressgrove" ~ "2",
                             transect == "millertonbiv" ~ "1",
                             transect == "bivalve" ~ "1",
                             transect == "walkercreek" ~ "3",
                             transect == "inverness" ~ "1",
                             TRUE ~ as.character(section))) %>%
    mutate(alpha.code = update_alpha(species),
           alpha.code = case_when(alpha.code == "HEGU" ~ "HERG", # this is the 4-letter code for Herring Gull on many data sheets
                                  alpha.code == "HOER" ~ "HEGR", # this is the 4-letter code for Horned/Eared Grebe on early data sheets
                                  alpha.code == "PRLO" ~ "LOON", # on early data sheets there was a pooled Pacific Red-throated Loon box
                                  alpha.code == "REGU" ~ "RBGU", # this is the 4-letter code for Ring-billed Gull on many data sheets
                                  alpha.code == "SCOT" ~ "SCOTER", # SCOTER in custom_bird_list
                                  alpha.code == "SCAU" ~ "SCAUP", # SCAUP in custom_bird_list
                                  TRUE ~ as.character(alpha.code)))
# calculate total positive an negative for each species, block ----
block_sums <- wbird_clean %>% 
  mutate(pos.neg = ifelse(tally > 0, "positive", "negative")) %>% 
  group_by(date, alpha.code, section, transect, pos.neg) %>% 
  summarise(block.sum = sum(tally)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(date, alpha.code, section, transect), names_from = pos.neg, values_from = block.sum) %>% 
  mutate(across(c(positive, negative), ~replace_na(., 0)))

saveRDS(block_sums, here("data_files/working_rds/block_sums_from_raw"))



block_sums <- readRDS(here("data_files/working_rds/block_sums_from_raw"))

single_spp_old_neg_machine <- block_sums %>% 
  reduce_section_transect() %>% 
  fill_block_zeros() %>% 
  old_neg_machine_from_raw_block_sums()


pooled <- filter(custom_bird_list, !is.na(group.spp)) %>% 
  select(alpha.code, group.spp) %>% 
  left_join(single_spp_old_neg_machine) 

pooled_constituents <- pooled %>% 
  select(date, "pooled.spp" = alpha.code, group.spp) %>% 
  separate(group.spp, into = paste("alpha.code_", seq(1, 4), sep = "")) %>% 
  pivot_longer(cols = contains("alpha.code"), names_to = "which.constituent", values_to = "alpha.code") %>% 
  filter(!is.na(alpha.code)) %>% 
  left_join(single_spp_old_neg_machine) %>% 
  filter(transect == "section.sum") %>% 
  select(date, pooled.spp, which.constituent, alpha.code, contains("field.tally"))

pooled_constituents_long <- pooled_constituents %>% 
  pivot_longer(cols = contains("field.tally")) %>% 
  separate(name, c("tally.type", "section"), sep = "_") %>% 
  distinct()

summed_pooled_constituents <- pooled_constituents_long %>% 
  group_by(date, pooled.spp, section) %>% 
  summarise(pooled.section.sum = sum(value)) %>% 
  arrange(date, pooled.spp, section) %>% 
  pivot_longer(cols = pooled.section.sum) %>% 
  mutate(name = paste(name, section, sep = "_")) %>% 
  pivot_wider(id_cols = c(date, pooled.spp)) %>% 
  mutate(transect = "section.sum") %>% 
  rename("alpha.code" = pooled.spp)

pooled_solver <- pooled %>% 
  filter(transect == "section.sum") %>% 
  full_join(summed_pooled_constituents) %>% 
  select(date, alpha.code, group.spp, transect, contains("_1"), contains("_2"), contains("_3"), contains("_4"))




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




  
# handle negatives ----


# data_with_allocated <- readRDS(here("data_files/entered_raw_combined/data_with_allocated"))


carry_forward <- calculate_carried_forward(data_with_allocated) 


net_sums <- calculate_net_sums(data_with_allocated) %>% 
  rename("bay.total.net" = bay.total)

sub_forward_sums <- calculate_section_sums_subtract_forward(data_with_allocated)


comp_sums = full_join(net_sums, sub_forward_sums %>% select(-adj.section)) %>% 
  relocate(bay.total.net, .after = everything()) %>% 
  ungroup()


comp_sums %>% 
  distinct(date, alpha.code, bay.total, bay.total.net) %>% 
  mutate(total.diff = bay.total - bay.total.net) %>% 
  arrange(total.diff) %>% view()
  write.csv(paste(here("data_files/derived_data/"), "/CBC_",  year(Sys.Date()), ".csv", sep = ""), row.names = FALSE)

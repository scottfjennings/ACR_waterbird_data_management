

library(tidyverse)
library(lubridate)
library(here)
library(RODBC)

library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


options("scipen"=999)
source(here("code/clean_from_access/waterbird_cleaning1_data_read_format.r"))

source("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/code/clean_from_access/waterbird_cleaning1_data_read_format.r")

wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")


# Step 1. Read data and basic cleaning

db <- "C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/waterbirds_v2.0.accdb"

# read in just the species lookup table from the access db
spp_table <- read_species_table(db) 
# write.csv(spp_table, here("data_files/spp_table.csv"), row.names = FALSE)

obs <- read_wbird_table(db, "tbl_WATERBIRDS_observation")



# query 
access_section_sums <- query_waterbirds(db) %>% 
  clean_waterbirds() %>% # basic field names and formatting
  sppindex_to_alpha(spp_table = spp_table) %>% # the query sets some alpha.codes to the lookup number in the species table instead of the 4-letter code
  mutate(alpha.code = update_alpha(alpha.code)) %>% # from birdnames
  fix_precount_block_names() %>% # assign precounts to the correct section: bivalve, millertonbivalve and inverness into section 1; cypressgrove into section 2: walkercreek into section 3
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% # from birdnames; filter to just waterbird species
  separate(block, into = c("transect", "section"), remove = F) %>% 
  mutate(section = gsub("sec", "", section))  %>% 
  group_by(date, section, alpha.code) %>% 
  summarise(access.section.sum = sum(count)) %>% 
  ungroup()

## from raw tallies

data_with_allocated <- readRDS(here("data_files/entered_raw_combined/data_with_allocated"))
data_with_allocated <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/entered_raw_combined/data_with_allocated")

# need to go from block sums to section sums
tally_section_sums_allocated <- data_with_allocated %>% 
  ungroup() %>% 
  mutate(section = ifelse(section == 5, 4, section),
         section.sum = section.positive + section.negative) %>% 
  group_by(date, alpha.code, section) %>%
  summarise(tally.section.sum.allocated = sum(section.sum)) %>% 
  ungroup() 



# tally section sums
# has the total positive and negative tallies for each block
#tally_section_sums_raw <- readRDS(here("data_files/block_sums")) %>% 
tally_section_sums_raw <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/block_sums") %>% 
  rename("alpha.code" = species) %>% # for compatability with birdnames
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% 
  ungroup() %>% 
  mutate(section = ifelse(section == 5, 4, section),
         section.sum = positive + negative)  %>% 
  group_by(date, alpha.code, section) %>%
  summarise(tally.section.sum.raw = sum(section.sum)) %>% 
  ungroup() 


compare_section_sums <- full_join(access_section_sums, tally_section_sums_allocated) %>%
  full_join(tally_section_sums_raw) %>% 
  right_join(., distinct(tally_section_sums_raw, date)) %>% 
  mutate(sum.diffs.alloc = access.section.sum - tally.section.sum.allocated,
         sum.diffs.raw = access.section.sum - tally.section.sum.raw)


filter(compare_section_sums, sum.diffs.alloc != 0) %>% nrow()
filter(compare_section_sums, sum.diffs.raw != 0) %>% nrow()


filter(compare_section_sums, abs(sum.diffs.raw) > 5 
       & abs(sum.diffs.raw/tally.section.sum.raw) > .05
       ) %>% 
  arrange(sum.diffs.raw) %>% #group_by(year(date)) %>% summarise(tot.diff = sum(abs(sum.diffs.raw))) %>% 
  view()

big.grebes = c("WEGR", "CLGR", "WCGR")
scaups = c("LESC", "GRSC", "SCAUP")

filter(compare_section_sums, alpha.code %in% c("BUFF"), date == "2011-12-17", section == 3)
filter(compare_section_sums, alpha.code %in% scaups, date == "2000-12-16", section == 1)



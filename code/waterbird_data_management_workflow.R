

# there are 3 main steps to preparing the waterbird data for use:
# 1. Read in and do basic cleaning
# 2. Split pooled birds (birds IDed to higher taxonomic groups than species)
# 3. Reconcile negatives (birds that flew forward of the boats)

library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source(here("code/extras/utils.R"))


wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")
wbird_keep_taxa_gulls <- c("Anseriformes", "Alcidae", "Laridae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Suliformes")

# 1. Read in data and do basic cleaning ----

# from raw .xlsx ----
# As of 2023, data will be read from .xlsx files (different file for each survey date) with pairs of columns for each block (species and tally) and raw tallies entered one per row
# reading raw tallies uses funtions from here:
source(here("code/read_clean/1_wrangle_tallies.R"))
# get a list of all files
# pattern = "_p" gets only sheets that have been proofed; line to remove any template files should be redundant because of pattern = "_p", but keeping it here for completeness
tally_files = list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data", full.names = TRUE, pattern = "_p") %>%
   stringr::str_subset(., "template", negate = TRUE)

# can then loop through that file list to read all raw data 
# the read_raw_tallies function from 1_wrangle_tallies.R reads in each .xlsx file and converts the data to long format; wrapping in map_df() combines the long version of each file into a single data frame
system.time(
long_tallies <- map_df(tally_files, read_raw_tallies)
)

# reading all the data takes a while so do it once and save
saveRDS(long_tallies, here("data_files/working_rds/long_tallies_from_raw"))

# can also read a single day of data from tally files
long_tallies <- read_raw_tallies(tally_files[66])

# or by supplying the entire file path
long_tallies <- read_raw_tallies("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data/20221217_p.xlsx")

# then either proceed with processing that day alone
# or merge with the rest of the data; calling distinct helps avoid trouble with the same date ending up in long_tallies more than once
long_tallies <- long_tallies %>% 
  bind_rows(readRDS(here("data_files/working_rds/long_tallies_from_raw"))) %>% 
  distinct()

# save this new version, if desired
saveRDS(long_tallies, here("data_files/working_rds/long_tallies_from_raw"))

# if previously read in and saved, can start here ----
long_tallies <- readRDS(here("data_files/working_rds/long_tallies_from_raw"))

# first take care of some quirks that came from the older data

wbird_clean <- long_tallies %>% 
  # change starboard and port to east and west
  mutate(block = gsub("starboard", "e", block),
         block = gsub("port", "w", block)) %>%
  # delete extra columns entered one particular day (see waterbird_raw_entry_tracking.xlsx for details)
  filter(block != "mid.bay.w.1.and.2") %>% 
  mutate(block = gsub("mid.bay.", "middle_", block),
         # remove ".." from block names
         block = gsub("\\.\\.", "\\.", block),
         # remove "." from precount block names so can separate on . in next step
         block = ifelse(grepl("cypress|millerton|walker", block), gsub("\\.", "", block), block)) %>% 
  separate(block, c("transect", "section"), remove = FALSE, sep = "\\.") 


# fix species codes
wbird_clean <- wbird_clean %>%
  rename("alpha.code" = species) %>% 
    mutate(alpha.code = update_alpha(alpha.code),
           alpha.code = case_when(alpha.code == "HEGU" ~ "HERG", # this is the 4-letter code for Herring Gull on many data sheets
                                  alpha.code == "HOER" ~ "HEGR", # this is the 4-letter code for Horned/Eared Grebe on early data sheets
                                  alpha.code == "PRLO" ~ "LOON", # on early data sheets there was a pooled Pacific Red-throated Loon box
                                  alpha.code == "REGU" ~ "RBGU", # this is the 4-letter code for Ring-billed Gull on many data sheets
                                  alpha.code == "SCOT" ~ "SCOTER", # SCOTER in custom_bird_list
                                  alpha.code == "SCAU" ~ "SCAUP", # SCAUP in custom_bird_list
                                  TRUE ~ as.character(alpha.code))) %>%
  bird_taxa_filter(keep_taxa = wbird_keep_taxa_gulls)



# assign section numbers to precounts
wbird_clean <- wbird_clean %>% 
  assign_precount_section()

# collapse to just a single row for each date X species X block
block_pos_neg <- make_block_pos_neg(wbird_clean) 
  # and combine section 5 into 4 if desired
block_pos_neg <- block_pos_neg %>% 
  combine_section_4_5() %>% 
  combine_middle() %>% 
  combine_section_2()



# 2. Split pooled ----
# this step uses functions from here:
source(here("code/split_pooled/2_split_pooled.R"))

complete_block_pos_neg <- block_pos_neg %>% 
  fill_block_zeros()

constituent_ratios <- complete_block_pos_neg %>% 
  add_pooled_alpha() %>% 
  calculate_section_bay_ratios_pos_neg() %>% 
  select(date, pooled.alpha.code, alpha.code, section, contains("combined"), contains("ratio"))

# separate positive and negative tallies of pooled birds into constituent species
# NOTE: CURRENTLY THIS DOES NOT WORK ON GULLS. Need to add gulls to custom_bird_list to separate GULL
allocated <- allocate_pooled_block_pos_neg(complete_block_pos_neg, constituent_ratios)
# check if any records were not assigned an allocation.scale
filter(allocated, is.na(allocation.scale)) %>% view()
# check if the allocation process added any fake birds
filter(allocated, tally != (tot.allocated + unallocated)) %>% view()

# generate "report" table describing the allocation for each block 
allocation_report <- make_allocation_report(allocated)
write.csv(allocation_report, here("data_files/entered_raw_combined/allocation_report.csv"), row.names = FALSE)


# bind together the unpooled (birds IDed to species), allocated and unallocated
# this keeps unpooled, allocated and unallocated in separate rows in case you want to view in that structure
bound_unpooled_allocated_unallocated <- bind_unpooled_allocated_unallocated(complete_block_pos_neg, allocated)

# and finally add up the unpooled and allocated to get back to a single row for each date X block X species (including unallocated pooled birds)
wbirds_allocated <- combine_unpooled_allocated_unallocated(bound_unpooled_allocated_unallocated)


# 3. Reconcile negatives ----
# this step uses functions from here:
source(here("code/handle_negatives/3_handle_negatives.R"))
source(here("code/handle_negatives/negative_machine_logic_and_structure.R"))


# reproduce the original Negative Waterbird Machine

# use block_pos_neg from the end of step 1 above to compare directly to old .xlsx files to help confirm negative machine code is working as expected 
neg_machine <- block_pos_neg %>% 
# or use wbirds_allocated from the end of step 2 above to process data with pooled birds allocated to species (i.e. to process data for the ACR database)
#neg_machine <- wbirds_allocated %>%
  block_pos_neg_to_net_final() %>% 
  #old_neg_machine_logic_and_structure() # %>% 
   new_neg_machine_logic_and_structure()



# This function recreates the logic of the Negative Waterbird Machine and formats the data in a way that can be directly compared to the .xlsx files.
# spot check against filled negative machines
 filter(neg_machine, alpha.code == "BRAC", date == "2009-02-08") %>% view()
 filter(neg_machine, alpha.code == "GRSC", date == "2014-12-20") %>% select(-contains("2a"), -contains("2b")) %>% view()

# This output format is NOT the format you will want to proceed with analysis. 
# If you want to proceed with analysis of the baywide total for each species and date, you need to do 
filter(neg_machine, transect == "section.sum") %>% select(date, alpha.code, bay.total) %>% 
  saveRDS(here("data_files/working_rds/new_neg_machine_bay_total"))

# If you want to proceed with analysis of the section sums for each species and date, you need to do 
zz <- filter(neg_machine, transect == "section.sum") %>% select(date, alpha.code, contains("final.section.data.record"))
 
# If you want to proceed with analysis of the block sums for each species and date, you need to do 
zz <- filter(neg_machine, !transect %in% c("section.sum", "cumulative.net.field.tally")) %>% select(date, transect, alpha.code, contains("final.section.data.record"))


# compare old and new negative machine

old_new_neg_machine_total <- full_join(readRDS(here("data_files/working_rds/new_neg_machine_bay_total")) %>% rename("new.bay.total" = bay.total),
                                       readRDS(here("data_files/working_rds/old_neg_machine_bay_total")) %>% rename("old.bay.total" = bay.total)) %>% 
  mutate(old.new.diff.absolute = new.bay.total - old.bay.total,
         old.new.diff.proportion = new.bay.total / old.bay.total)



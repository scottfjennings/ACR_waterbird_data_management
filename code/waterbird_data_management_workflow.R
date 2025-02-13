

# there are 3 main steps to preparing the waterbird data for use. In this workflow, these steps are accomplished using functions which are defined in separate files.  

# 1. Read in and do basic cleaning - as of Jan 2023 the data exist in 3 different locations. 
    # The raw tally files contain a separate file for each survey date. The data are entered in a wide format with "species" and "tally" column pairs for each survey block, and the tally columns have every individual tally as they appear on the data sheets. This format makes for efficient data entry and proofing but needs additional wrangling before it can be efficiently cleaned and managed. These data were entered and proofed in 2022-23 and should be considered the cleanest version of the data. These data are as free as possible from any arithmetic errors or inconsistent handling of negatives. The data in this format allow the same handling of pooled and negatives across all years. To read these data use 1_read_clean_from_raw_tallies.R
    # The access database has the historically cleaned data. The data cleaning process to create this database appears to have changed through the history of the project, and these changes were apparently never fully documented. If you want to read data from this database, use functions in 1_read_clean_from_access.R
    # The Negative Machine files are .xlsx files used to calculate net negatives that remain at the end of section 4. The raw data in these files is likely unreliable, so it is unlikely that you will want to read it in, but if you do, use 1_read_clean_from_NegMachine.R

# 2. Split pooled birds (birds IDed to higher taxonomic groups than species)
    # This step uses functions defined in 2_split_pooled.R

# 3. Reconcile negatives (birds that flew forward of the boats) - there are 2 possible methods to handle negatives, depending on whether you assume birds flying forward of the boats land in the current section or fly past the current section.
    # The Negative Machine method, which was apparently used from about 2000 onward, assumes that negatives land in the current section and thus should be subtracted from the current section count. The data can be processed with this method of handing negatives using the functions defined in 3_negative_machine.R
    # If you assume negatives fly beyond the current section then you would subtract them from future sections and NOT from the current section. To process the data with thie method use functions defined in 3_subtract_forward_negatives.R. NOTE: as of 1/31/2023 this may not be fully functional.


# first load the necessary packages
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(here)
library(birdnames)

# working locally
# custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")
# working on Azure
custom_bird_list <- readRDS("C:/Users/scott.jennings.EGRET/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

# some general utility functions that may be used in more than one step in this workflow
source(here("code/utils.R"))

# keep_taxa lists to be used in birdnames::bird_taxa_filter()
# all waterbirds but no gulls
wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")
# waterbirds and gulls
wbird_keep_taxa_gulls <- c("Anseriformes", "AMCO", "Alcidae", "Laridae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Suliformes")




# 1. Read in data and do basic cleaning ----
# 1.1. read from raw tallies ----
# from raw .xlsx ----
# As of 2023, data will be read from .xlsx files (different file for each survey date) with pairs of columns for each block (species and tally) and raw tallies entered one per row


# set the location of where raw tally data are stored. this generally should be V, but you may also choose to work from a local copy.
raw_tally_location <- "V:/Waterbirds_data/waterbirds_raw_data_entry/"
raw_tally_location <- paste(here("data_files"), "/", sep = "")


# reading raw tallies uses funtions from here:
source(here("code/1_read_clean_from_raw_tallies.R"))

# read_raw_tallies() workflow 1 MOST LIKELY METHOD: adding a single day to the existing long_tallies  --
# OPTIONAL (you may just know which date is missing) check if there are any entered_raw_data files that haven't been processed into long_tallies
# if any dates have raw.date == TRUE and long.tally.date != TRUE, need to process that date's file individually, below
full_join(distinct(readRDS(here("data_files/working_rds/long_tallies_from_raw")), date) %>% 
            mutate(long.tally.date = TRUE),
          data.frame(raw.data = list.files(paste(raw_tally_location, "entered_raw_data", sep = ""), full.names = TRUE, pattern = "_p") %>%
                       stringr::str_subset(., "template", negate = TRUE)) %>%
            mutate(raw.dates = str_replace(raw.data, paste(raw_tally_location, "entered_raw_data/", sep = ""), ""),
                   raw.dates = str_replace(raw.dates, "_p.xlsx|_p2.xlsx", ""),
                   date = paste(str_sub(raw.dates, 1, 4), str_sub(raw.dates, 5, 6), str_sub(raw.dates, 7, 8), sep = "-"),
                   date = ymd(date),
                   raw.date = TRUE) %>%
            select(date, raw.date)) %>%
  view() 


# read in that date's file by supplying the entire file path
new_long_tallies <- read_raw_tallies(paste(raw_tally_location, "entered_raw_data/20250208_p2.xlsx", sep = ""))

# then merge with the rest of the data; need to deal with same date ending up in long_tallies more than once. distinct() drops data
prior_long_tallies <- readRDS(here("data_files/working_rds/long_tallies_from_raw"))
# run this line if you have made edits to a particular date's data that was already in "data_files/working_rds/long_tallies_from_raw" 
prior_long_tallies <- filter(prior_long_tallies, date != "2025-01-11")

long_tallies_from_raw <- new_long_tallies %>% 
  bind_rows(prior_long_tallies)


# read_raw_tallies() workflow 2: re-do all raw data files through read_raw_tallies():
# first need to get a list of all files
# pattern = "_p" gets only sheets that have been proofed; line to remove any template files should be redundant because of pattern = "_p", but keeping it here for completeness
tally_files = list.files(paste(raw_tally_location, "entered_raw_data", sep = ""), full.names = TRUE, pattern = "_p") %>%
   stringr::str_subset(., "template", negate = TRUE)

# can then loop through that file list to read all raw data 
# the read_raw_tallies function from 1_wrangle_tallies.R reads in each .xlsx file and converts the data to long format; wrapping in map_df() combines the long version of each file into a single data frame
system.time(
  long_tallies_from_raw <- map_df(tally_files, read_raw_tallies)
)

# 2023-7-10 run on Azure
#   user  system elapsed 
# 226.37   58.27  561.37 
#   user  system elapsed 
# 237.18   61.75  543.44
# 2023-07-19 Azure run
# user  system elapsed 
# 271.50  117.86  695.53 


# whichever workflow you choose, save this new 
saveRDS(long_tallies_from_raw, here("data_files/working_rds/long_tallies_from_raw"))
#
# 1.1a read from Access ----
library(RODBC)
source(here("code/1_read_clean_from_access.R"))
spp_table <- read.csv(here("data_files/spp_table.csv")) 

wbirds <- query_waterbirds(here("data_files/waterbirds_v2.0.accdb")) %>% 
  rename("alpha.code" = Species) %>% 
  sppindex_to_alpha(spp_table = spp_table)

filter(wbirds, alpha.code == "KIEI")

distinct(wbirds, Year, Month, Day) %>% 
  mutate(date = paste(Year, Month, Day, sep = "-"),
         date = as.Date(date)) %>% 
  write.csv(here("data_files/survey_dates_old_db.csv"), row.names = FALSE)

# 1.2. basic data cleaning ----
# if previously read in and saved, can start here ---
long_tallies_from_raw <- readRDS(here("data_files/working_rds/long_tallies_from_raw"))

# take care of some quirks that came from the older data
wbird_clean <- long_tallies_from_raw %>% 
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
  bird_taxa_filter(keep_taxa = wbird_keep_taxa)

# note, if you do not have birdnames::bird_taxa_filter, you can replicate this filtering step with a list of species codes that should be included.
# these codes are saved at data_files/waterbird_keep_species.csv
# you can replace the bird_taxa_filter() line with this line:
# right_join(read.csv(here("data_files/waterbird_keep_species.csv")))

# assign section numbers to precounts
wbird_clean <- wbird_clean %>% 
  assign_precount_section()

# collapse to just a single row for each date X species X block
block_pos_neg <- make_block_pos_neg(wbird_clean) 

#block_pos_neg %>% 
#  filter(transect %in% c("bivalve", "cypressgrove", "walkercreek"), date == "2023-12-16") %>% 
#  write.csv(here("data_files/derived_data/non_cbc_precount_2023.csv"), row.names = FALSE)
  

# and combine sections/transects if desired (comment in or out the lines you don't want)
# don't need to run this next chunk if you don't want to combine any sections

block_pos_neg <- block_pos_neg %>% 
  combine_section_4_5() #%>% # combine sections 4 and 5 into section 4
#  combine_middle() %>% # combine middle east and middle west into middle
#  combine_section_2() # combine sections 2a and 2b into section 2

# NOTE: Combining the middle transects effects calculation of the baywide total. I don't fully understand this yet, but it appears that when there are more negatives in one middle transect than there are positives in the other, combining transects generally yields a lower baywide total than keeping the middle transects separate. These differences do not seem large enough to yield meaningfully different trend estimates, but this may be important for other analyses.


# 2. Split pooled ----
# this step uses functions from here:
source(here("code/2_split_pooled.R"))

# 
complete_block_pos_neg <- block_pos_neg %>% 
  fill_block_zeros()

constituent_ratios <- complete_block_pos_neg %>% 
  add_pooled_alpha() %>% 
  calculate_section_bay_ratios_pos_neg() %>% 
  select(date, pooled.alpha.code, alpha.code, section, contains("combined"), contains("ratio"))

# separate positive and negative tallies of pooled birds into constituent species
# NOTE: CURRENTLY THIS DOES NOT WORK ON POOLED GULLS. Need to add gulls to custom_bird_list to separate GULL
allocated <- allocate_pooled_block_pos_neg(complete_block_pos_neg, constituent_ratios)
# check if any records were not assigned an allocation.scale THIS SHOULD HAVE 0 RECORDS
filter(allocated, is.na(allocation.scale)) %>% nrow()
# check if the allocation process added any fake birds THIS SHOULD HAVE 0 RECORDS
filter(allocated, tally != (tot.allocated + unallocated)) %>% nrow()

# generate "report" table describing the allocation for each block 
allocation_report <- make_allocation_report(allocated)
write.csv(allocation_report, here("data_files/entered_raw_combined/allocation_report.csv"), row.names = FALSE)

#  --##----##----##----##-- START OPTIONAL  --##----##----##----##--
read.csv(here("data_files/entered_raw_combined/allocation_report.csv")) %>% 
  filter(date %in% c("2023-12-16", "2024-01-27", "2024-02-10"), pooled.alpha.code == "SCAUP") %>% View()

read.csv(here("data_files/entered_raw_combined/allocation_report.csv")) %>% 
  filter(date %in% c("2023-12-16", "2024-01-27", "2024-02-10"), pooled.alpha.code == "SCAUP") %>%
  group_by(date) %>% 
  summarise(tot = sum(abs(tally)))
#  --##----##----##----##-- END OPTIONAL --##----##----##----##--

# bind together the unpooled (birds IDed to species), allocated and unallocated
# this keeps unpooled, allocated and unallocated in separate rows in case you want to view in that structure
bound_unpooled_allocated_unallocated <- bind_unpooled_allocated_unallocated(complete_block_pos_neg, allocated)

# and finally add up the unpooled and allocated to get back to a single row for each date X block X species (including unallocated pooled birds)
wbirds_allocated <- combine_unpooled_allocated_unallocated(bound_unpooled_allocated_unallocated)


# 3. Reconcile negatives ----
# this step uses functions from here:
# source(here("code/handle_negatives/3_handle_negatives.R"))
source(here("code/3_negative_machine.R"))


# reproduce the original Negative Waterbird Machine

# use block_pos_neg from the end of step 1 above to compare directly to old .xlsx files to help confirm negative machine code is working as expected 
#neg_machine <- block_pos_neg %>% 
# or use wbirds_allocated from the end of step 2 above to process data with pooled birds allocated to species (i.e. to process data for the ACR database)
neg_machine <- wbirds_allocated %>%
  block_pos_neg_to_net_final() %>% # in utils.R
#  filter(section != 5) %>% # include this line if you're processing the CBC count area only
  #old_neg_machine_logic_and_structure() # %>% 
   new_neg_machine_logic_and_structure()

saveRDS(neg_machine, here("data_files/working_rds/new_neg_machine_all"))

#  --##----##----##----##-- START OPTIONAL  --##----##----##----##--
# confirming negative machine working as expected
# This function recreates the logic of the Negative Waterbird Machine and formats the data in a way that can be directly compared to the .xlsx files.
# spot check against filled negative machines
filter(neg_machine, alpha.code == "BRAC", date == "2009-02-08") %>% view()
filter(neg_machine, alpha.code == "GRSC", date == "2014-12-20") %>% select(-contains("2a"), -contains("2b")) %>% view()
#  --##----##----##----##-- END OPTIONAL  --##----##----##----##--




#  --##----##----##----##-- START OPTIONAL  --##----##----##----##--
# if you want to run the negative machine with section 5 do this chunk
section5_dates <- filter(long_tallies, str_detect(block, "5")) %>% distinct(date)

neg_machine <- wbirds_allocated %>%
  block_pos_neg_to_net_final() %>% # in utils.R
  filter(date %in% section5_dates$date) %>% 
  CBC_neg_machine_logic_and_structure()

saveRDS(neg_machine, here("data_files/working_rds/new_neg_machine_all_CBCsec5"))
#neg_machine <- readRDS(here("data_files/working_rds/new_neg_machine_all_CBCsec5"))
#  --##----##----##----##-- END OPTIONAL  --##----##----##----##--

# 4. Saving various versions of the data
neg_machine <- readRDS(here("data_files/working_rds/new_neg_machine_all"))

# This neg_machine format is NOT the format you will want to proceed with analysis. 
# The baywide total for each species is generally the format used for overall trend evaluation
# Save the baywide total for each species and date 
filter(neg_machine, transect == "section.sum") %>% select(date, alpha.code, bay.total) %>% 
  saveRDS(here("data_files/working_rds/new_neg_machine_bay_total"))
 
# but the species totals by section or block may also be desired for certain analyses
# Save the section sums for each species and date 
filter(neg_machine, transect == "section.sum") %>% select(date, alpha.code, contains("final.section.data.record")) %>% 
  saveRDS(here("data_files/working_rds/new_neg_machine_section_totals"))
 
# Save the block sums for each species and date 
filter(neg_machine, !transect %in% c("section.sum", "cumulative.net.field.tally")) %>% select(date, transect, alpha.code, contains("final.section.data.record")) %>% 
  saveRDS(here("data_files/working_rds/new_neg_machine_block_totals"))



  
  

readRDS(here("data_files/working_rds/new_neg_machine_bay_total")) %>% 
  filter(date == "2023-12-16") %>% 
  write.csv(here("data_files/derived_data/CBC_2023.csv"), row.names = FALSE)

filter(neg_machine, transect == "section.sum", date == "2023-12-16") %>% select(date, alpha.code, bay.total) %>% 
  full_join(read.csv(here("data_files/derived_data/CBC_2023.csv")) %>%
              mutate(date = as.Date(date)) %>% 
              rename("cbc.total" = bay.total)) %>% 
  mutate(section.5.birds = bay.total - cbc.total)  %>% 
  write.csv(here("data_files/derived_data/baytotal_CBC_2023.csv"), row.names = FALSE)


#  --##----##----##----##-- START OPTIONAL  --##----##----##----##--
# 5. compare old and new negative machine

old_new_neg_machine_total <- full_join(readRDS(here("data_files/working_rds/new_neg_machine_bay_total")) %>% rename("new.bay.total" = bay.total),
                                       readRDS(here("data_files/working_rds/old_neg_machine_bay_total")) %>% rename("old.bay.total" = bay.total)) %>% 
  mutate(old.new.diff.absolute = new.bay.total - old.bay.total,
         old.new.diff.proportion = new.bay.total / old.bay.total)

filter(old_new_neg_machine_total, old.new.diff.proportion != 1) %>% 
  count(alpha.code) %>% 
  filter(n > 10) %>% 
  select(alpha.code) %>% 
  left_join(old_new_neg_machine_total) %>% 
  select(date, alpha.code, contains("bay.total")) %>% 
  pivot_longer(cols = contains("bay.total"), names_to = "which.abundance", values_to = "abundance") %>% 
  mutate(season = ifelse(month(date) == 12, year(date), year(date)-1),
         which.abundance = ifelse(which.abundance == "new.bay.total", "Middle transects separate", "Middle transects combined")) %>% 
  group_by(season, alpha.code, which.abundance) %>% 
  mutate(p75.abund = quantile(abundance, 0.75)) %>% 
  ggplot() +
  geom_point(aes(x = season, y = p75.abund, color = which.abundance), size = 0.5) +
  stat_smooth(aes(x = season, y = p75.abund, color = which.abundance), size = 0.5, formula = y ~ x + I(x^2), method = "lm") +
  facet_wrap(~alpha.code, scales = "free_y") +
  labs(color = "",
       y = "75th percentile abundance",
       title = "Species with most frequent difference\nfrom middle transects combined vs. separate\nusing the old negative machine method.") +
  theme_bw() +
    theme(legend.position="bottom")

ggsave(here("figures_output/old_new_negmachine_compare.png"), width = 8, height = 6)  

#  --##----##----##----##-- END OPTIONAL  --##----##----##----##--





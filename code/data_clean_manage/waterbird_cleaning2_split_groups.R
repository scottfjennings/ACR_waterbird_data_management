

# this is the second of 4 steps to prep waterbird data for use.
# note that waterbird_cleaning1 is sourced below to read the data from access and do basic house-keeping on variable names

# this file splits lumped birds into constituent species

# The logic:
# There are several species of waterbirds that may not be identified to species but that can be assigned to a group (e.g. LOON, CORM).  

# Undifferentiated birds are allocated to the constituent species of that group based on the ratios of positively identified birds in each constituent species. Allocation is only done if the number of positively identified birds in the group is greater than the number of undifferentiated birds.

# The bay is divided into 4 sections, numbered from south to north

# If there are enough* identified birds at the section scale, then the ratio of IDed birds in each species counted in that section is used to divvy the lumped birds

# If there are not enough IDed birds in the Section, then the ratio for IDed birds across the entire Bay is used. 

# NOTE: IT APPEARS KELLY AND TAPPAN 1998 DID NOT DO THIS EXTRA STEP. DOING THIS EXTRA STEP INCLUDES MORE INDIVIDUAL BIRDS, BUT AT THE COST OF MORE LIBERAL ASSUMPTIONS ABOUT RATIOS OF IDENTIFIED AND UNIDENTIFIED BIRDS BEING CONSTANT THROUGHOUT THE ENTIRE BAY. THE FOLLOWING CODE INDICATES WHETHER AN ALLOCATION WAS MADE BASED ON SECTION OR BAY RATIOS, SO THAT THE USER CAN DECIDE WHICH DATA TO INCLUDE IN ANALYSIS

# * group cutoff value is 50 for grebes, 100 for all other groups
# ----

# JK, Rothenback analysis notes, from: S:\Databases\Waterbirds_data\Dropbox files from Christine 20141111\Data Manipularion.doc
# Counts started in December 1989. Missing data are: Jan+Feb 1991, Feb 1996, Jan 1998, Feb 2000, Feb 2001 *2 in Jan*, Dec 2002, Feb 2005 *2 in Jan*, (Feb 2010 *2 in Jan*, Jan + Dec 2012). () = outside herring survey dates. 1996 is missing February's data, and also has the earliest survey date in January (6th).
# Sections 6, 7, 8 were combined with the rest of the data until December of 1990. Section 5 was combined with the rest of the data until December 1991. Section 9 was combined until January 1992. 
# Section 5: missing 01/93, 12/96, 02/06, 
# Section 7: missing 02/04
# Section 8: missing 12/91, 12/11
# Section 9: missing 02/04

# included PECO in CORM allocation, included all 3 scoters in SCOTER allocation
# For bayabun and baydens – excluded all raptors, terns, gulls, and kingfisher.






# 1 packages and data ----
library(tidyverse)
# options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/water_birds/code/data_clean_manage/waterbird_cleaning1_variable_names.R") # this loads data from access, does basic house keeping on variable names


# new functions ----
# make_section_tallies()----
# reproducing row 24 of the xlsx spreadsheet
# calculate total positive birds ("sec.final") and total positive minus negative ("sec.tally)
make_section_tallies <- function(wbird_df) {
  section_tallies <- wbird_df %>% 
  mutate(final.data.record = ifelse(count < 0, 0, count)) %>% # pink cells in xlsx
  #select(date, block, transect, section, section, alpha.code, count, final.data.record) %>% 
  arrange(date, alpha.code, section) %>% 
  group_by(date, alpha.code, section) %>% 
  summarise(sec.tally = sum(count),
            sec.final = sum(final.data.record)) %>% 
  ungroup() 
}


# data ----
# data should come from waterbirds_cleaning1_variable_names.R

# get just the fields needed
wbirds <- wbirds %>% 
  select(date, block, section, alpha.code, count)


# 2 steps to add all 0s for no birds observed
wbirds_wide <- wbirds %>% 
  mutate(alpha.code = paste("spp", alpha.code, sep = ".")) %>% 
  pivot_wider(id_cols = c("date", "block", "section"), names_from = "alpha.code", values_from = "count")
  
wbirds_long_0s <- wbirds_wide %>% 
    replace(., is.na(.), 0) %>% 
  pivot_longer(cols = contains("spp."), names_to = "alpha.code", values_to = "count") %>% 
  mutate(alpha.code = gsub("spp.", "", alpha.code))
  
  
sec_tallies <- wbirds_long_0s %>% 
  make_section_tallies() %>% # defined above
  is_spp_group() # in bird_utility_functions.R

no_try_split <- sec_tallies %>% 
  filter(is.spp.group == T, is.na(group.spp), sec.tally != 0, sec.final != 0)

saveRDS(no_try_split, "data_files/working_rds/no_try_split")
# this expands grouped data so that there is a record for each constituent species
expand_groups <- function(zsection_tallies) {
group_tallies_wide <- sec_tallies %>% 
  dplyr::filter(!is.na(group.spp)) %>% 
  separate(group.spp, c(paste("group.spp", seq(1, 4))))

group_tallies_long <- group_tallies_wide %>% 
  pivot_longer(contains("group.spp")) %>% 
  select(date, section, group.alpha.code = alpha.code, group.sec.tally = sec.tally, group.sec.final = sec.final, alpha.code = value) %>% 
  filter(!is.na(alpha.code))
return(group_tallies_long)
}


# put single species and expanded group data side-by-side
single_spp_by_group <- expand_groups(sec_tallies) %>% 
  full_join(., filter(sec_tallies, is.spp.group == F) %>% select(-group.spp, -is.spp.group)) %>% 
  filter(!is.na(sec.final))


# this calculates ratios of constituent species based both on the sum of the positive counts and the sum of the positive and absolute value negative counts.
# allocating lumped birds based on the sum of the positive and abs(negative) counts seems to provide a more-realistic allocation when there are a large number of negatives of one species and just a few of the other species; in this case most of the lumped birds go to the species with the nigh negative count. See for example CORM on 2015-02-14 section 4 and CORM on 2008-01-12 sections 3 and 4. Seems like this really only happens when we have big numbers of BRAC flying around.
groupies_allocated <- single_spp_by_group %>% 
  # first calculate by-section ratios
  group_by(date, section, group.alpha.code) %>% 
  mutate(sum.abs.tally = sum(abs(sec.tally)), # by section total positively IDed birds by species group, including absolute value negatives
         abs.tally.ratio = abs(sec.tally)/sum.abs.tally) %>% 
  ungroup() 

  # add baywide ratios
groupies_allocated <- groupies_allocated %>% 
 group_by(date, group.alpha.code, alpha.code) %>% # !!!! IMPORTANT, need group and spp grouping here since some sp occur in >1 group 
  mutate(bay.abs.tally = sum(abs(sec.tally))) %>%  # baywide total positively IDed birds by species, including absolute value negatives. numerator for ratio
  ungroup() %>% 
  group_by(date, group.alpha.code) %>% 
  mutate(sum.abs.tally.bay = sum(abs(sec.tally)), # baywide total positively IDed birds by species group, including absolute value negatives. denominator for ratio
         abs.tally.ratio.bay = bay.abs.tally/sum.abs.tally.bay) %>% 
  ungroup()%>% 
  mutate(group.cut = ifelse(group.alpha.code %in% c("HEGR", "WCGR"), 50, 100))

  # determine which ratio (section or bay) to allocate by
# if # positive ID in section > # grouped in section, use section ratio
# if # positive ID in section < # grouped in section < # positive in bay, use bay ratio

groupies_allocated <- groupies_allocated %>% 
         mutate(allocation.scale = case_when(sum.abs.tally <= group.cut & abs(group.sec.tally) <= sum.abs.tally ~ "no.allocation",
                                     sum.abs.tally > group.cut & abs(group.sec.tally) <= sum.abs.tally ~ "section",
                                     sum.abs.tally > group.cut & abs(group.sec.tally) <= sum.abs.tally & sum.abs.tally.bay > group.cut ~ "bay",
                                     abs(group.sec.tally) > sum.abs.tally & abs(group.sec.tally) <= sum.abs.tally.bay ~ "bay",
                                     abs(group.sec.tally) >= sum.abs.tally.bay ~ "no.allocation"),
         new.tally = sec.tally + allocated.tally,
         tally.diff = new.tally - sec.tally) %>% 
  ungroup()


  # allocate by appropriate ratio
groupies_allocated <- groupies_allocated %>% 
  mutate(group.cut = ifelse(group.alpha.code %in% c("HEGR", "WCGR"), 50, 100),
         allocated.tally = case_when(sum.abs.tally <= group.cut & abs(group.sec.tally) <= sum.abs.tally ~ 0,
                                     sum.abs.tally > group.cut & abs(group.sec.tally) <= sum.abs.tally ~ group.sec.tally * abs.tally.ratio,
                                     sum.abs.tally > group.cut & abs(group.sec.tally) <= sum.abs.tally & sum.abs.tally.bay > group.cut ~ group.sec.tally * abs.tally.ratio.bay,
                                     abs(group.sec.tally) > sum.abs.tally & abs(group.sec.tally) <= sum.abs.tally.bay ~ group.sec.tally * abs.tally.ratio.bay,
                                     abs(group.sec.tally) >= sum.abs.tally.bay ~ 0),
         allocated.tally = round(allocated.tally, 0),
         allocation.scale = case_when(sum.abs.tally <= group.cut & abs(group.sec.tally) <= sum.abs.tally ~ "no.allocation",
                                     sum.abs.tally > group.cut & abs(group.sec.tally) <= sum.abs.tally ~ "section",
                                     sum.abs.tally > group.cut & abs(group.sec.tally) <= sum.abs.tally & sum.abs.tally.bay > group.cut ~ "bay",
                                     abs(group.sec.tally) > sum.abs.tally & abs(group.sec.tally) <= sum.abs.tally.bay ~ "bay",
                                     abs(group.sec.tally) >= sum.abs.tally.bay ~ "no.allocation"),
         new.tally = sec.tally + allocated.tally,
         tally.diff = new.tally - sec.tally) %>% 
  ungroup() %>% 
  group_by(date, group.alpha.code, section) %>% 
         mutate(alloc.re.grouped = ifelse(allocation.scale == "no.allocation", group.sec.tally, sum(allocated.tally))) %>% 
  ungroup() %>% 
  mutate(alloc.test = alloc.re.grouped - group.sec.tally)
  
# some summaries to check the allocation 

total_grouped <- sec_tallies %>% 
  filter(is.spp.group == T) %>% 
  group_by(alpha.code) %>% 
  summarise(total.grouped = sum(abs(sec.tally))) %>% 
  rename(group.alpha.code = alpha.code)


allocated_summary <- groupies_allocated %>% 
    filter(allocation.scale != "no.allocation") %>% 
    #distinct(date, section, group.alpha.code, allocated.tally, allocation.scale) %>% 
    group_by(group.alpha.code, allocation.scale)%>% 
    replace(., is.na(.), 0) %>% 
    summarise(total = sum(abs(allocated.tally))) %>% 
  ungroup()

  unallocated <- groupies_allocated %>% 
    filter(allocation.scale == "no.allocation") %>% 
    distinct(date, section, group.alpha.code, group.sec.tally, allocation.scale)
  
  unallocated_summary <- unallocated %>% 
    group_by(group.alpha.code, allocation.scale) %>% 
    summarise(total = sum(abs(group.sec.tally))) %>% 
    ungroup()
  
  un_allocated_summary <- rbind(unallocated_summary, allocated_summary) %>% 
    pivot_wider(names_from = allocation.scale, values_from = total) %>% 
    full_join(total_grouped) %>% 
    replace(., is.na(.), 0) %>% 
    mutate(derived.total.grouped = no.allocation + section + bay)
 
saveRDS(un_allocated_summary, "data_files/working_rds/un_allocated_summary")
  
  
   
  filter(groupies_allocated, group.alpha.code == "PCLO") %>% 
    arrange(date, group.alpha.code, section) %>% 
    #select(date, section, group.alpha.code, group.sec.tally, alpha.code, sum.abs.tally, group.cut, abs.tally.ratio, sum.abs.tally.bay, abs.tally.ratio.bay, allocated.tally, allocation.scale, alloc.re.grouped, alloc.test) %>% 
    View()
  
  
  groupies_allocated %>% 
    filter(allocation.scale == "no allocation" , group.sec.tally < 0) 

  
  # write file showing allocation work to disk
saveRDS(groupies_allocated, "data_files/working_rds/groupies_allocated")


# combine allocated with data for birds IDed to species
grouped_split4adding <- groupies_allocated %>% 
  filter(allocation.scale != "no allocation") %>% 
  select(date, section, alpha.code, count = allocated.tally) %>% 
  mutate(count.from = "allocated")


wbirds_sppid_allocated <- wbirds %>% 
  is_spp_group() %>% 
  filter(is.na(group.spp)) %>% 
  select(date, section, alpha.code, count) %>% 
  mutate(count.from = "field") %>% 
  rbind(grouped_split4adding) %>% 
  group_by(date, section, alpha.code) %>% 
  make_section_tallies()



saveRDS(wbirds_sppid_allocated, "data_files/working_rds/wbirds_sppid_allocated")
# wbirds_sppid_allocated <- readRDS("data_files/working_rds/wbirds_sppid_allocated")


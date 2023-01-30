
##

# The logic of splitting is as follows:  

# * There are several species of waterbirds that may not be identified to species but that can be assigned to a POOLED group (e.g. LOON, CORM). # For each pooled group on each day, we want to calculate the ratios of constituent species in that group (e.g. ratio of DCCO:BRAC:PECO on a given day that some birds were assigned to CORM). We need to do this at the section and bay scales.
 

# * POOLED birds are then allocated to the constituent species of that POOLED group based on the ratios of positively identified birds in each constituent species. Allocation is only done if the number of positively identified birds in the group is greater than the number of undifferentiated birds.

# * The bay is divided into 4 sections, numbered from south to north

# * If there are enough identified birds at the section scale, then the ratio of IDed birds in each species counted in that section is used to divvy the lumped birds. The group cutoff value is 50 for grebes, 100 for all other groups.

# * If there are not enough IDed birds in the Section, then the ratio for IDed birds across the entire Bay is used. NOTE: IT APPEARS KELLY AND TAPPAN 1998 DID NOT DO THIS EXTRA STEP. DOING THIS EXTRA STEP INCLUDES MORE INDIVIDUAL BIRDS, BUT AT THE COST OF MORE LIBERAL ASSUMPTIONS ABOUT RATIOS OF IDENTIFIED AND UNIDENTIFIED BIRDS BEING CONSTANT THROUGHOUT THE ENTIRE BAY. THERE ARE AT LEAST SOME OCCASIONS WHERE THE CONSTITUENT SPP RATIO IN THE SECTION WITH POOLED BIRDS IS QUITE DIFFERENT THAN THE RATIO IN THE WHOLE BAY (E.G., ) THE FOLLOWING CODE INDICATES WHETHER AN ALLOCATION WAS MADE BASED ON SECTION OR BAY RATIOS, SO THAT THE USER CAN DECIDE WHICH DATA TO INCLUDE IN ANALYSIS

# Unless otherwise indicated, the functions are defined in waterbird_cleaning2_split_groups.R.  





# The first part of step 2 is to sum the number of birds in each of the four sections of the bay. In the final mutate() line here we add a column "group.spp" for constituent species in each POOLED group, if applicable.  
# sum_section_tallies_finals() incorporates birds counted in the precount zones into the appropriate sections.


#' add_pooled_alpha
#' 
#' adds a field pooled.alpha.code to the waterbird data frame to identify which positively IDed species belong to which pooled group. if a species belongs to >1 pooled group, then the record is duplicated (e.g. for PALO a record is made for LOON and PCLO). Consequently, the output of this function SHOULD ONLY BE USED FOR CALCULATING RATIOS OF POSITIVELY IDed SPECIES FOR EACH GROUP
#' 
#'
#' @param df data frame with alpha.code field. should generally be cleaned, by-block data e.g. the output from fill_block_zeros()
#'
#' @return data frame with same structure as df but with extra field pooled.alpha.code. 
#' @export
#'
#' @examples
add_pooled_alpha <- function(df) {
pooled_constituents <- filter(custom_bird_list, !is.na(group.spp)) %>% 
  select(alpha.code, group.spp) %>% 
  separate(group.spp, c(paste("group.spp", seq(1, 4)))) %>% 
  pivot_longer(contains("group.spp")) %>% 
  select(pooled.alpha.code = alpha.code, alpha.code = value) %>% 
  filter(!is.na(alpha.code)) %>% 
  inner_join(df)
}


#' calculate_section_bay_ratios_pos_neg
#' 
#' Calculate ratios of positively IDed birds in each pooled group at the section and bay scales. This function chooses whichever is greater between the positive and negative fields. Function requires df to have pooled.alpha.code.
#'
#' @param df data frame with date, alpha.code, pooled.alpha.code, section, transect, positive and negative fields. Needs pooled.alpha.code so should be output from add_pooled_alpha()
#'
#' @return data frame with same fields as df plus fields representing section and bay totals and ratios
#' @export
#'
#' @examples
calculate_section_bay_ratios_pos_neg <- function(df) {
constituent_ratios <- df %>% 
  mutate(ratio.value = ifelse(abs(negative) > positive, abs(negative), positive)) %>% 
  group_by(date, pooled.alpha.code, alpha.code, section) %>% 
  summarise(section.value = sum(ratio.value))  %>% 
  group_by(date, pooled.alpha.code, section) %>% 
  mutate(combined.section.value = sum(section.value)) %>%
  ungroup() %>% 
  group_by(date, pooled.alpha.code, alpha.code) %>% 
  mutate(bay.value = sum(section.value)) %>%
  ungroup() %>%
  group_by(date, pooled.alpha.code) %>% 
  mutate(combined.bay.value = sum(section.value)) %>%
  ungroup() %>% 
  mutate(section.ratio = section.value/combined.section.value,
         bay.ratio = bay.value/combined.bay.value) %>% 
    mutate_at(c('section.ratio','bay.ratio'), ~replace_na(.,0)) %>%
  arrange(date, pooled.alpha.code, section, alpha.code)

}





# And then we peel off just the POOLED bird data from wbirds, join with the ratio table, and allocate POOLED birds to constituent species based on the appropriate ratio.
# can't use get_pooled_block() and allocate_pooled_block() because now want to handle positive and negative tallies separately
#' allocate_pooled_block_pos_neg
#' 
#' Divy birds IDed to pooled groups into constituent species based on the ratios of positively IDed birds in each section. This divies positive and negative birds separately, operates at the block level, and selects the section or bay ratio based on the following rules:
#' 
#' If there are more positively IDed birds than pooled birds in a section, and there were more than 50 (grebes) or 100 (all others) positively IDed birds in that section, then use that section ratio.
#' 
#' If there were fewer positively IDed birds than pooled birds in a section and/or there were fewer than 50 (grebes) or 100 (all others) positively IDed birds in that section, then use the entire bay ratio.
#' 
#' If there were fewer positively IDed birds than pooled birds in the whole bay and/or there were fewer than 50 (grebes) or 100 (all others) positively IDed birds in the whole bay, then leave birds as pooled.
#'
#' @param block_df 
#' @param zratios 
#'
#' @return
#' @export
#'
#' @examples
allocate_pooled_block_pos_neg <- function(block_df, zratios) {
  
allocated_pooled_block_pos_neg <- block_df %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(!is.na(group.spp)) %>% 
  rename("pooled.alpha.code" = alpha.code,
         "pooled.block.pos" = positive,
         "pooled.block.neg" = negative) %>% 
  select(-group.spp) %>% 
  pivot_longer(cols = contains("pooled.block"), names_to = "which.tally", values_to = "tally") %>% 
  filter(tally != 0) %>% 
  inner_join(zratios) %>% 
  arrange(date, section, pooled.alpha.code, which.tally, alpha.code) %>% 
  mutate(abund.cutoff = ifelse(pooled.alpha.code %in% c("HEGR", "WCGR"), 50, 100)) %>% 
  mutate(allocation.scale = case_when(tally < combined.section.value & combined.section.value > abund.cutoff ~ "section",
                                      tally < combined.section.value & combined.section.value <= abund.cutoff & combined.bay.value > abund.cutoff ~ "bay",
                                      tally >= combined.section.value & tally < combined.bay.value & combined.bay.value > abund.cutoff ~ "bay",
                                      TRUE ~ "none"),
         allocated.tally = case_when(allocation.scale == "section" ~ tally * section.ratio,
                                     allocation.scale == "bay" ~ tally * bay.ratio,
                                     TRUE ~ 0),
         allocated.tally = ifelse(which.tally == "pooled.block.pos", floor(allocated.tally), ceiling(allocated.tally)))

  # calculate remaning unallocated birds
unallocated <- allocated_pooled_block_pos_neg %>% 
  distinct(date, section, transect, pooled.alpha.code, which.tally, tally) %>% 
  full_join(allocated_pooled_block_pos_neg %>% 
              group_by(date, section, transect, pooled.alpha.code, which.tally) %>% 
              summarise(tot.allocated = sum(allocated.tally)) %>%
              ungroup()) %>% 
  mutate(unallocated = tally - tot.allocated)

out_allocated <- full_join(allocated_pooled_block_pos_neg, unallocated) %>% 
  relocate(c(tot.allocated, unallocated), .after = everything())

return(out_allocated)

}



make_allocation_report <- function(df) {
allocation_report <- df %>% 
  mutate(pos.neg = ifelse(grepl("pos", which.tally), "positive", "negative"),
         allocation.report = paste(allocated.tally, "to", alpha.code)) %>% 
  group_by(date, section, transect, pooled.alpha.code, tally, tot.allocated, unallocated) %>% 
  summarise(allocation.report.out = paste(allocation.report, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(allocation.report.out = paste("In section ", section, ", ", transect, ", ", tally, " birds were IDed as ", pooled.alpha.code, ". ", 
                                       tot.allocated, " of these were allocated to species (", allocation.report.out, "), and ", unallocated, " remained unallocated.", sep = "")) %>% 
  arrange(date, pooled.alpha.code, section, transect)
}

       # combine allocated, unallocated, and unpooled
# first the allocated
# need to sum birds allocated to species from different pooled groups and pivot back to columns for positive and negative
bind_unpooled_allocated_unallocated <- function(block_df, allocated_df) {
  
allocated_pooled_block <- allocated_df %>% 
  filter(allocation.scale != "none",
         allocated.tally != 0) %>% 
  group_by(date, section, transect, alpha.code, which.tally) %>% 
  summarise(total.allocated = sum(allocated.tally),
            allocated.from = paste(pooled.alpha.code, collapse = "_")) %>% 
  #mutate(which.tally = gsub("pooled.", "", which.tally)) %>% 
  pivot_wider(id_cols = c(date, section, transect, alpha.code, allocated.from), names_from = which.tally, values_from = total.allocated) %>% 
  #mutate(across(contains("block."), ~replace_na(., 0))) %>% 
  arrange(date, section, allocated.from, alpha.code)

# next unallocated
# mostly just need to manage column names and pivot back to columns for positive and negative
unallocated_pooled_block <- allocated_df %>% 
  filter(unallocated != 0) %>% 
  distinct(date, section, transect, pooled.alpha.code, unallocated, which.tally) %>% 
  #mutate(which.tally = gsub("pooled.", "", which.tally)) %>% 
  pivot_wider(id_cols = c(date, section, transect, pooled.alpha.code), names_from = which.tally, values_from = unallocated) %>% 
  rename(alpha.code = pooled.alpha.code) %>% 
  #mutate(across(contains("block."), ~replace_na(., 0))) %>% 
  mutate(allocated.from = "unallocated")

allocated_unallocated <- bind_rows(allocated_pooled_block, unallocated_pooled_block) %>% 
  mutate(across(contains("block."), ~replace_na(., 0))) %>% 
  rename("positive" = pooled.block.pos,
         "negative" = pooled.block.neg)


# join allocated to original data
# then finally combine the allocated with the original data
section_sums_allocated <- block_df %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(is.na(group.spp)) %>% 
  select(-group.spp) %>% 
  bind_rows(allocated_unallocated) %>% 
  arrange(date, alpha.code, section, transect)

}

combine_unpooled_allocated_unallocated <- function(df) {
  
# and finally select just the needed columns
data_with_allocated <- section_sums_allocated %>% 
  group_by(date, section, transect, alpha.code) %>% 
  summarise(positive = sum(positive),
            negative = sum(negative)) %>% 
  arrange(date, alpha.code, section, transect)

}


#saveRDS(data_with_allocated, here("data_files/entered_raw_combined/data_with_allocated"))

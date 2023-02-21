

#######   JAN 26, 2023. %>% %>% %>% %>% THESE FUNCTIONS WERE WRITTEN FOR THE PRE-2022 DATA STRUCTURE AND MAY NOT WORK AND/OR ARE NOT NEEDED NOW. BUT I'M NOT READY TO DELETE THEM YET








#' Make pooled date constituents  THIS ONLY WORKS WITH PRE-2022 DATA; DOESN'T WORK WITH NEW SECTION IDs (2a AND 2b). ALSO FILLING IN THE 0s IS NOT NEEDED 
#' 
#' Peel off data for positively IDed birds in the constituent species of pooled groups for any dates that birds were recorded in pooled groups.
#' Then fill in 0s for any missing species X section combos
#'
#' @param df data frame with summed section tallies and finals
#'
#' @return data frame
#' @export
#'
#' @examples
make_pooled_date_constituents <- function(df) {
pooled_dates <- df %>% 
  filter(!is.na(group.spp)) %>% 
  distinct(date, alpha.code, group.spp) %>% 
  separate(group.spp, c(paste("group.spp", seq(1, 4)))) %>% 
  pivot_longer(contains("group.spp")) %>% 
  select(date, pooled.alpha.code = alpha.code, alpha.code = value) %>% 
  filter(!is.na(alpha.code)) %>%
  mutate(alpha.code = update_alpha(alpha.code)) %>% 
  uncount(4) %>% 
  group_by(date, pooled.alpha.code, alpha.code) %>% 
  mutate(section = row_number(),
         section = as.character(section))

pooled_date_constituents <- df %>% 
  select(-group.spp) %>% 
  right_join(pooled_dates) %>% 
  arrange(date, pooled.alpha.code, section, alpha.code) %>% 
  mutate(across(contains("section."), ~replace_na(., 0)))
return(pooled_date_constituents)
}


# calculate ratios of constituent species ----
# this calculates ratios of constituent species based both on the sum of the positive counts and the sum of the positive and absolute value negative counts.
# allocating lumped birds based on the sum of the positive and abs(negative) counts seems to provide a more-realistic allocation when there are a large number of negatives of one species and just a few of the other species; in this case most of the lumped birds go to the species with the high negative count. See for example CORM on 2015-02-14 section 4 and CORM on 2008-01-12 sections 3 and 4. Seems like this really only happens when we have big numbers of BRAC flying around.

#' Title
#'
#' @param df data frame output from make_pooled_date_constituents()
#'
#' @return
#' @export
#'
#' @examples
calc_section_constituent_sum_proportion <- function(df){
df <- df %>% 
  # first calculate by-section ratios
  group_by(date, section, pooled.alpha.code) %>% 
  mutate(section.all.constituents = sum(abs(section.tally)), # by section total positively IDed birds by species group, including absolute value negatives
         section.proportion = abs(section.tally)/section.all.constituents,
         section.proportion = ifelse(is.nan(section.proportion), 0, section.proportion)) %>% 
  ungroup() 
return(df)
}


#' Title
#'
#' @param df  data frame. intended to be output from calc_section_constituent_sum_proportion(), but also works on output from make_pooled_date_constituents()
#'
#' @return data frame
#' @export
#'
#' @examples
calc_bay_constituent_sum_proportion <- function(df){
df <- df %>% 
  group_by(date, pooled.alpha.code, alpha.code) %>% # !!!! IMPORTANT, need pooled.alpha.code, alpha.code grouping here since some sp occur in >1 group 
  mutate(bay.total = sum(abs(section.tally))) %>%  # baywide total positively IDed birds by species, including absolute value negatives. numerator for ratio
  ungroup() %>% 
  group_by(date, pooled.alpha.code) %>% 
  # baywide total positively IDed birds by species group, including absolute value negatives. denominator for ratio
  mutate(bay.all.constituents = sum(abs(section.tally)), 
         bay.proportion = bay.total/bay.all.constituents) %>% 
  ungroup()
return(df)
}






#' Get pooled block tallies - CURRENTLY NOT USED
#' 
#' 
#' Filter just data for pooled birds at the section scale and rename fields in prep for allocating to constituent species
#'
#' @param df data frame wbirds
#'
#' @return data frame with summed block tallies and finals
#' @export
#'
#' @examples
#' pooled_block <- get_pooled_block(wbirds)
get_pooled_block <- function(df) {
pooled <- df %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(!is.na(group.spp)) %>% 
  rename(pooled.alpha.code = alpha.code,
         pooled.count = count)
return(pooled)
}





#' Allocate section POOLED birds to constituent species
#' 
#' Set allocation scale (section or bay) for any sections with pooled birds based on the number of POOLED and unpooled birds in that section. 
#'
#' @param constituent_ratios data frame with the section and bay ratios of constituent species in each POOLED group for each day that POOLED group was recorded 
#' @param pooled_block the data for all birds assigned to POOLED groupsat the block scale
#'
#' @return data frame
#' @export
#' 
#' @details The allocation process yields non-integer values, which must be rounded to integer in some way. This rounding may lead to a different number of "allocated" birds than there were POOLED birds (generally either 1 more or 1 less). When there are 2 constituent species, rounding all values to the nearest integer is best, but when there are more than 2 constituent species than rounding DOWN may often be best. However, overall it seems best to apply 1 rounding rule to all records, regardless of constituent number. Rounding to the nearest integer yields the fewest extra or dropped birds. 
#'
#' @examples
#' allocated_pooled_block <- allocate_pooled_block(constituent_ratios, pooled_block)
#' 
allocate_pooled_block <- function(constituent_ratios, pooled_block) {
pooled_allocated <- right_join(constituent_ratios, pooled_block) %>% 
  #select(-section.tally, -section.final, -bay.total) %>% 
  mutate(group.cut = ifelse(pooled.alpha.code %in% c("HEGR", "WCGR"), 50, 100),
         allocation.scale = NA) %>%
# if there are more positive IDed birds (all constituents combined) in section than pooled birds in section, 
# and more positive IDed in the section than the group cutoff, use section ratio 
  mutate(allocation.scale = case_when(abs(section.all.constituents) >= group.cut & abs(section.all.constituents) >= abs(pooled.count) ~ "section",
                                      TRUE ~ as.character(allocation.scale))) %>%
  # if there are more pooled birds (all constituents combined) in section than positive IDed birds in section, 
  #  but fewer than positive IDed birds in entire bay
  # and more positive IDed in the bay than the group cutoff, use bay ratio
  mutate(allocation.scale = case_when(abs(bay.all.constituents) >= group.cut & 
                                          abs(bay.all.constituents) >= abs(pooled.count) &  
                                          (abs(section.all.constituents) < abs(pooled.count) |  abs(section.all.constituents) < group.cut) ~ "bay",
                                      TRUE ~ as.character(allocation.scale))) %>% 
  # when there are fewer positively IDed birds in entire bay than group cutoff, no allocation should be done
  mutate(allocation.scale = case_when(abs(pooled.count) >= abs(bay.all.constituents) | group.cut >= abs(bay.all.constituents) ~ "no.allocation",
                                      TRUE ~ as.character(allocation.scale))) %>% 
  select(date, section, contains("pooled"), alpha.code, everything()) %>% 
  mutate(allocated.count = case_when(allocation.scale == "section" ~ pooled.count * section.proportion,
                               allocation.scale == "bay" ~ pooled.count * bay.proportion,
                               allocation.scale == "no.allocation" ~ 0),
         allocated.count = round(allocated.count, 0)) %>% 
  group_by(date, block, pooled.alpha.code) %>% 
  mutate(total.allocated = sum(allocated.count)) %>% 
  ungroup() %>% 
  mutate(alloc.remainder = pooled.count - total.allocated)
return(pooled_allocated)
}


  

#' Combine the allocated and unallocated POOLED birds with the unpooled birds at the block scale
#'
#' @param wbirds output from sum_section_tallies_finals() with field group.spp added 
#' @param allocated_pooled_block output from allocate_pooled_block()
#'
#' @return data frame
#' @export
#'
#' @examples
#' split_unsplit_unpooled_block <- combine_split_unsplit_unpooled_block(wbirds, allocated_pooled_block)
#' 
 combine_split_unsplit_unpooled_block <- function(wbirds, allocated_pooled_block) {
# combine allocated with data for birds IDed to species

pooled_split <- allocated_pooled_block %>% 
  filter(allocation.scale != "no allocation", allocated.count != 0) %>% 
  select(date, block, alpha.code, count = allocated.count, allocation.scale)

pooled_unsplit <- allocated_pooled_block %>% 
  filter(allocation.scale == "no.allocation") %>% 
  select(date, block, alpha.code = pooled.alpha.code, count = pooled.count, allocation.scale)  %>%
  distinct()

unpooled <- wbirds %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(is.na(group.spp)) %>% 
  select(-group.spp, -section, -transect)

wbirds_sppid_allocated <- bind_rows(unpooled, pooled_split, pooled_unsplit)  %>% 
  group_by(date, block, alpha.code) %>% 
  summarise(count = sum(count)) %>% 
    ungroup() %>% 
    separate(block, c("transect", "section"), remove = FALSE)
return(wbirds_sppid_allocated)
}
    
  

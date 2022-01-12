




#' Expand pooled
#'
#' This expands grouped data so that there is a record for each constituent species.
#'
#' @param df data frame with section summed tallies and finals for each species
#'
#' @return data frame
#' @export
#'
#' @examples
expand_pooled <- function(df) {
group_tallies_wide <- df %>% 
  dplyr::filter(!is.na(group.spp)) %>% 
  separate(group.spp, c(paste("group.spp", seq(1, 4))))

group_tallies_long <- group_tallies_wide %>% 
  pivot_longer(contains("group.spp")) %>% 
  select(date, section, pooled.alpha.code = alpha.code, pooled.section.tally = section.tally, pooled.section.final = section.final, alpha.code = value) %>% 
  filter(!is.na(alpha.code)) %>%
  mutate(alpha.code = update_alpha(alpha.code))

expanded_pooled <- df %>% 
  filter(is.na(group.spp)) %>% 
  select(-group.spp) %>% 
  full_join(., group_tallies_long)%>%
  mutate(across(contains("section.sum"), ~replace_na(., 0)))

return(expanded_pooled)
}



#' Make pooled date constituents
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
#' @param df data frame wbirds
#'
#' @return data frame with summed block tallies and finals
#' @export
#'
#' @examples
get_pooled_block <- function(df) {
pooled <- df %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(!is.na(group.spp)) %>% 
  rename(pooled.alpha.code = alpha.code,
         pooled.count = count) %>% 
  group_by(date, section, pooled.alpha.code) %>% 
  mutate(pooled.section.tally = sum(pooled.count)) %>% 
  ungroup() %>% 
  group_by(date, pooled.alpha.code) %>% 
  mutate(pooled.bay.tally = sum(pooled.count)) %>% 
  ungroup()
return(pooled)
}



#' Title
#'
#' @param df data frame with section totals 
#'
#' @return data frame with summed section tallies and finals
#' @export
#'
#' @examples
get_pooled <- function(df) {
pooled <- df %>% 
  filter(!is.na(group.spp)) %>% 
  select(-group.spp, -section.final) %>% 
  rename(pooled.alpha.code = alpha.code,
         pooled.section.tally = section.tally) %>% 
  group_by(date, pooled.alpha.code) %>% 
  mutate(pooled.bay.total = sum(pooled.section.tally)) %>% 
  ungroup()
return(pooled)
}

  # determine which ratio (section or bay) to allocate by

#' Set allocation scale
#' 
#' Set allocation scale (section or bay) for any sections with pooled birds. 
#'
#' @param constituents_by_group data frame with the section and bay ratios of constituent species in each POOLED group for each day that POOLED group was recorded 
#' @param pooled the data for all birds assigned to POOLED groups, summarized to section
#'
#' @return data frame
#' @export
#'
#' @examples
allocate_pooled <- function(constituents_by_group, pooled) {
pooled_allocated <- right_join(constituents_by_group, pooled) %>% 
  #select(-section.tally, -section.final, -bay.total) %>% 
  mutate(group.cut = ifelse(pooled.alpha.code %in% c("HEGR", "WCGR"), 50, 100),
         allocation.scale = NA) %>%
# if there are more positive IDed birds (all constituents combined) in section than pooled birds in section, 
# and more positive IDed in the section than the group cutoff, use section ratio 
  mutate(allocation.scale = case_when(abs(section.all.constituents) >= group.cut & abs(section.all.constituents) >= abs(pooled.section.tally) ~ "section",
                                      TRUE ~ as.character(allocation.scale))) %>%
  # if there are more pooled birds (all constituents combined) in section than positive IDed birds in section, 
  #  but fewer than positive IDed birds in entire bay
  # and more positive IDed in the bay than the group cutoff, use bay ratio
  mutate(allocation.scale = case_when(abs(bay.all.constituents) >= group.cut & 
                                          abs(bay.all.constituents) >= abs(pooled.section.tally) &  
                                          (abs(section.all.constituents) < abs(pooled.section.tally) |  abs(section.all.constituents) < group.cut) ~ "bay",
                                      TRUE ~ as.character(allocation.scale))) %>% 
  # when there are fewer possitively IDed birds in entire bay than group cutoff, no allocation should be done
  mutate(allocation.scale = case_when(abs(pooled.section.tally) >= abs(bay.all.constituents) | group.cut >= abs(bay.all.constituents) ~ "no.allocation",
                                      TRUE ~ as.character(allocation.scale))) %>% 
  select(date, section, contains("pooled"), alpha.code, everything()) %>% 
  mutate(allocated.tally = case_when(allocation.scale == "section" ~ pooled.section.tally * section.proportion,
                               allocation.scale == "bay" ~ pooled.section.tally * bay.proportion,
                               allocation.scale == "no.allocation" ~ 0),
         allocated.tally = round(allocated.tally, 0))
return(pooled_allocated)
}



#' Title
#'
#' @param section_tallies_finals output from sum_section_tallies_finals() with field group.spp added 
#' @param allocated_pooled output from allocate_pooled()
#'
#' @return data frame
#' @export
#'
#' @examples
 combine_split_unsplit_unpooled <- function(section_tallies_finals, allocated_pooled) {
# combine allocated with data for birds IDed to species

pooled_split <- allocated_pooled %>% 
  filter(allocation.scale != "no allocation") %>% 
  rename(count = allocated.tally) %>% 
  sum_section_tallies_finals()

pooled_unsplit <- allocated_pooled %>% 
  filter(allocation.scale == "no.allocation") %>% 
  select(date, section, alpha.code = pooled.alpha.code, count = section.tally)  %>% 
  sum_section_tallies_finals()

unpooled <- section_tallies_finals %>% 
  filter(is.na(group.spp)) %>% 
  select(-group.spp)

wbirds_sppid_allocated <- bind_rows(unpooled, pooled_split, pooled_unsplit)  %>% 
  group_by(date, section, alpha.code) %>% 
  summarise(section.tally = sum(section.tally),
            section.final = sum(section.final))
return(wbirds_sppid_allocated)
}
  
  
  
  
  

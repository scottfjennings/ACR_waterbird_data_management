



# this file contains functions that duplicate the treatment of negatives in the old WBNegMachine.xlsx spreadsheets.
# That method is flawed, but these functions are retained for record keeping.

# this file carries negative tallies forward through sections, and if there are net negatives left over after section 4, these are added back in to the section 4 count.

# note that waterbird_cleaning1 is sourced below to read the data from access and do basic house-keeping on variable names

# section.num = row 11
# count = yellow boxes
# final.data.record = pink boxes, for section 4 = column I
# section.sum = row 24 under yellow boxes
# section.sum.final = row 24 under pink boxes
# carried.forward = row 25



# define functions ----

# reproducing row 24 of the xlsx spreadsheet
# calculate total positive birds ("section.final") and total positive minus negative ("section.tally)


#negatives_carried <- split_unsplit_unpooled %>% 
#  widen_section_tallies() %>% 
#  calc_carried_forward() %>% 
#  calc_bay_sum()


# A guide to column names  
# I tried to make field names here as similar as possible to those in the old xlsx files.  

# Pooled and single species data are treated the same until the expand_pooled() function is called; thus before then count and both section.sum... fields have data for single species and pooled birds. After expand_pooled() is called and the result is joined back tot he single species data, then single species and pooled data live in different field names
# pooled.alpha.code = Pooled group (keeping as alpha.code for consistency with all other bird name operations)

# section.tally = tallied (may contain negatives) of each constituent species by section
# section.final = final (no negatives) of each constituent species by section
# section.all.constituents = total of all constituents in a given pooled group, by section
# section.proportion = proportion of section.all.constituents made up of given constituent species, by section
# bay.total = final (no negatives) of each constituent species for the entire bay
# bay.all.constituents = total of all constituents in a given pooled group for the entire bay
# pooled.section.tally = tallied (may contain negatives) of pooled group birds by section
# pooled.section.final = final (no negatives) of pooled group birds by section
# pooled.bay.total = final (no negatives) of pooled group birds for the entire bay




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



#' Get pooled data.
#' 
#' Filter just data for pooled birds at the section scale and rename fields in prep for allocating to constituent species
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
         pooled.section.tally = section.tally) 
return(pooled)
}


#' Allocate section POOLED birds to constituent species
#' 
#' Set allocation scale (section or bay) for any sections with pooled birds based on the number of POOLED and unpooled birds in that section. 
#'
#' @param constituent_ratios data frame with the section and bay ratios of constituent species in each POOLED group for each day that POOLED group was recorded 
#' @param pooled the data for all birds assigned to POOLED groups, summarized to section
#'
#' @return data frame
#' @export
#'
#' @examples
allocate_pooled <- function(constituent_ratios, pooled) {
pooled_allocated <- right_join(constituent_ratios, pooled) %>% 
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




#' Combine the allocated and unallocated POOLED birds with the unpooled birds
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
  distinct() %>% 
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
  
  


#' Widen block tallies
#' 
#' Transform long version block tallies to wide version with columns for each block
#'
#' @param df data frame with block tally data. 
#'
#' @return
#' @export
#' 
#' @details 
#'
#' @examples
#' 
#' wide_blocks <- widen_block_tallies(wbirds)
#' 
widen_block_tallies <- function(df) {
df_wider <- df %>% 
#  select(date, alpha.code, block, section, transect, count) %>%
 # mutate(section = paste("blockcount.", section, sep = "")) %>% 
  pivot_wider(id_cols = c(date, transect, alpha.code), names_from = section, values_from = c("count", "positive", "negative", "draft.sec")) %>% 
  replace(is.na(.), 0) %>% 
  arrange(date, alpha.code, transect)

return(df_wider)
}





#' Widen section tallies
#' 
#' Transform long version section tallies to wide version with columns for each section.
#'
#' @param df data frame with section tally data. 
#'
#' @return
#' @export
#' 
#' @details Function is designed and intended to be run on a data frame with as many POOLED birds allocated to constituent species as possible (using workflow/functions defined in waterbird_cleaning2_split_groups.R). However, this can also be run on data that doesn't have POOLED birds allocated, if the user has a good reason to do so.
#'
#' @examples
widen_section_tallies <- function(df) {
df_longer <- df %>% 
  pivot_longer(cols = contains("section."), names_to = "sum.type", values_to = "sum") %>% 
  mutate(sec.name = paste(sum.type, section, sep = "_")) 


df_wider <- df_longer %>% 
  pivot_wider(id_cols = c(date, alpha.code), names_from = sec.name, values_from = sum) %>% 
  replace(is.na(.), 0) %>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

return(df_wider)
}







#' Calculate carried forward tallies
#'
#' @param df data frame with section tallies in wide format (output from widen_section_tallies())
#'
#' @return
#' @export
#' 
#' @details This calculates the "Cumulative Net Field Tally (including "Carried Forward" Negative Tally)" in row 25 of the xlsx files. This calculation is the way any negative birds in a given section are carried forward to the next section. The value is simply the sum of all birds for that species (positives and negatives) and whatever value was carried forward from previous sections. 
#' 
#' Note, there is a different rule for calculating section 1 carried forward than for the remaining sections.
#' 
#' Also note, this appears to treat carried forward negatives incorrectly. Net carried forward negatives that remain upon reaching section 4 are added back to the section 4 tally. However, if there were more positives than the net negatives before the negatives were encountered then those negatives were likely already counted and thus are double counted when they're added back into section 4. On the other hand, if those negatives cannot be explained by previously encountered positives, then we must conclude that those birds either came into the bay from the south or were missed on the first count. In this case those negatives are most appropriately added to the tally for a section farther south in the bay than section 4. Perhaps they should be added to the inverness/millerton precount areas? 
#'
#' @examples
calc_carried_forward <- function(df) {
  
tallies_carried_forward <- df %>% 
  mutate(sec.forward_1 = ifelse(section.tally_1 < 0, section.tally_1, 0),
         sec.forward_2 = ifelse(sec.forward_1 + section.tally_2 < 0, sec.forward_1 + section.tally_2, 0),
         sec.forward_3 = ifelse(sec.forward_2 + section.tally_3 < 0, sec.forward_2 + section.tally_3, 0),
         sec.forward_4 = ifelse(sec.forward_3 + section.tally_4 < 0, sec.forward_3 + section.tally_4, 0))%>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

}






calc_bay_sum <- function(df) {
  
  zcarried_forward <- df %>% 
    rename(sec.prelim_4 = section.final_4) %>% 
    mutate(birds2add = ifelse(sec.forward_4 < 0, abs(sec.forward_4), 0),
           section.final_4 = sec.prelim_4 + birds2add,
           bay.sum = section.final_1 + section.final_2 + section.final_3 + section.final_4)
  
}


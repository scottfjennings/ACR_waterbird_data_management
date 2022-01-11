

# this is the third of 4 steps to prep waterbird data for use.

# this file carries negative tallies forward through sections, and if there are net negatives left over after section 4, these are added back in to the section 4 count.

# note that waterbird_cleaning1 is sourced below to read the data from access and do basic house-keeping on variable names

# section.num = row 11
# count = yellow boxes
# final.data.record = pink boxes, for section 4 = column I
# section.sum = row 24 under yellow boxes
# section.sum.final = row 24 under pink boxes
# carried.forward = row 25


# source, loads packages, data ----


# define functions ----

# reproducing row 24 of the xlsx spreadsheet
# calculate total positive birds ("section.final") and total positive minus negative ("section.tally)


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
widen_block_tallies <- function(df) {
df_wider <- df %>% 
  select(date, alpha.code, block, section, transect, count) %>%
  mutate(section = paste("section.", section, sep = "")) %>% 
  pivot_wider(id_cols = c(date, transect, alpha.code), names_from = section, values_from = count) %>% 
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
#' @examples
calc_carried_forward <- function(df) {
  
tallies_carried_forward <- df %>% 
  mutate(sec.forward_1 = ifelse(section.tally_1 < 0, section.tally_1, 0),
         sec.forward_2 = ifelse(sec.forward_1 + section.tally_2 < 0, sec.forward_1 + section.tally_2, 0),
         sec.forward_3 = ifelse(sec.forward_2 + section.tally_3 < 0, sec.forward_2 + section.tally_3, 0),
         sec.forward_4 = ifelse(sec.forward_3 + section.tally_4 < 0, sec.forward_3 + section.tally_4, 0))%>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

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
#' @examples
subtract_forward <- function(df) {
  
tallies_carried_forward <- df %>% 
  mutate(sec.forward_1 = ifelse(section.tally_1 < 0, section.tally_1, 0),
         sec.draft_1 = abs(section.tally_1),
         sec.forward_2 = ifelse(sec.forward_1 + section.tally_2 < 0, sec.forward_1 + section.tally_2, 0),
         sec.draft_2 = abs(sec.forward_1 + section.tally_2),
         sec.forward_3 = ifelse(sec.forward_2 + section.tally_3 < 0, sec.forward_2 + section.tally_3, 0),
         sec.draft_3 = abs(sec.forward_2 + section.tally_3),
         sec.forward_4 = ifelse(sec.forward_3 + section.tally_4 < 0, sec.forward_3 + section.tally_4, 0),
         sec.draft_4 = abs(sec.forward_3 + section.tally_4)) %>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

}


split_unsplit_unpooled %>% 
  widen_section_tallies() %>% 
  subtract_forward()  %>% 
  calc_bay_sum() %>% view()


calc_bay_sum <- function(df) {
  
  zcarried_forward <- df %>% 
    rename(sec.prelim_4 = section.final_4) %>% 
    mutate(birds2add = ifelse(sec.forward_4 < 0, abs(sec.forward_4), 0),
           section.final_4 = sec.prelim_4 + birds2add,
           bay.sum = section.final_1 + section.final_2 + section.final_3 + section.final_4)
  
}






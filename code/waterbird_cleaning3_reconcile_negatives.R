


get_section_pos_neg <- function(df) {
  df <- df %>% 
    mutate(pos.count = ifelse(count > 0, count, 0),
           neg.count = ifelse(count < 0, count, 0)) %>% 
    group_by(date, alpha.code, section) %>% 
    summarise(positive = sum(pos.count),
           negative = sum(neg.count)) %>% 
    ungroup()
}

#' Widen block positive/negative
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
widen_block_pos_neg <- function(df) {
df_wider <- df %>% 
  pivot_wider(id_cols = c(date, alpha.code), names_from = section, values_from = c("positive", "negative")) %>% 
  replace(is.na(.), 0) %>% 
  arrange(date, alpha.code)

return(df_wider)
}







#' Calculate birds to subtract forward and/or add backward
#'
#' @param df data frame with section tallies in wide format (output from widen_section_tallies())
#'
#' @return
#' @export
#' 
#' @details This is an attempt to fix some problems in the logic of the original negative machine (and calc_carried_forward() which duplicates negative treatment of the negative machine).
#' 
#' we assume all birds flying past the boats originated on the bay (did not arrive to the bay behind the boats from the south). Thus, all birds flying past the boats have either already been counted or they were missed in an area that was counted. Either way, all negatives should be subtracted from future section counts, and they should never be added to section 4. If the negatives can be explained by positives in each section they should be assigned to  the section they were observed in. if they cannot be explained by previous positives they should be added back to section 1 (most parsimonious assumption is that they were missed in precount or moved south between end of precount and start of boat count).
#' 
#'
#' @examples
subtract_forward_add_back <- function(df) {
  

subtracted_forward <- df %>% 
  mutate(# section 1
         draft_sec1 = positive_sec1,  
         add.backward_sec1 = ifelse(positive_sec1 + negative_sec1 < 0, abs(positive_sec1 + negative_sec1), 0), 
         ahead_sec1 = negative_sec1,
         behind_sec1 = ifelse(positive_sec1 + negative_sec1 > 0, positive_sec1 + negative_sec1, 0),
         # section 2
         draft_sec2 = ifelse(positive_sec2 + ahead_sec1 > 0, positive_sec2 + ahead_sec1, 0), 
         add.backward_sec2 = add.backward_sec1 + ifelse(behind_sec1 + positive_sec2 + negative_sec2 < 0, abs(behind_sec1 + positive_sec2 + negative_sec2), 0),
         ahead_sec2 = negative_sec2 + ifelse(ahead_sec1 + positive_sec2 > 0, 0, ahead_sec1 + positive_sec2),
         behind_sec2 = ifelse(positive_sec2 + behind_sec1 + negative_sec2 > 0, positive_sec2 + behind_sec1 + negative_sec2, 0),
         # section 3
         draft_sec3 = ifelse(positive_sec3 + ahead_sec2 > 0, positive_sec3 + ahead_sec2, 0),
         add.backward_sec3 = add.backward_sec2 + ifelse(behind_sec2 + positive_sec3 + negative_sec3 < 0, abs(behind_sec2 + positive_sec3 + negative_sec3), 0),
         ahead_sec3 = negative_sec3 + ifelse(ahead_sec2 + positive_sec3 > 0, 0, ahead_sec2 + positive_sec3),
         behind_sec3 = ifelse(positive_sec3 + behind_sec2 + negative_sec3 > 0, positive_sec3 + behind_sec2 + negative_sec3, 0),
         # section 4
         draft_sec4 = ifelse(positive_sec4 + ahead_sec3 > 0, positive_sec4 + ahead_sec3, 0),draft_sec1 = positive_sec1,
         add.backward_sec4 = add.backward_sec3 + ifelse(behind_sec3 + positive_sec4 + negative_sec4 < 0, abs(behind_sec3 + positive_sec4 + negative_sec4), 0),
         ahead_sec4 = negative_sec4 + ifelse(ahead_sec3 + positive_sec4 > 0, 0, ahead_sec3 + positive_sec4),
         behind_sec4 = ifelse(positive_sec4 + behind_sec3 + negative_sec4 > 0, positive_sec4 + behind_sec3 + negative_sec4, 0)) %>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

}


#' View how negatives are subtracted or added for a single species on a single date
#'
#' Reshapes a single species X date record for easier viewing.
#'
#' @param date 
#' @param alpha.code 
#'
#' @return small table printed to console
#' @export
#'
#' @examples
#' subtracted_added_viewer(zdate = "2004-12-18", zalpha.code = "BRAC")
subtracted_added_viewer <- function(zdate, zalpha.code) {
  filter(subtracted_forward, date == zdate, alpha.code == zalpha.code) %>% 
  pivot_longer(cols = contains("_sec")) %>% 
  separate(name, c("varb", "section"),sep = "_") %>% 
  pivot_wider(id_cols = c("date", "alpha.code", "section"), names_from = varb, values_from = value) %>% 
  select(date, alpha.code, section, positive, negative, behind, ahead, add.backward, draft) 
}



#' Check results of subtracted forward
#' 
#' Uses logic that the total of known ahead and known behind birds should be equal to the sum of all birds assigned to each section (including those assigned back to section 1)
#'
#' @param df data frame output of subtract_forward_add_back()
#'
#' @return data frame with only the draft section, add.backward and known... columns, plus extra columns for totals derived from known birds and those assigned to each section, and the difference between those 2.
#' @export
#' @details check for diff.total != 0 to find possible scenarios where the current rules for handling negatives do no work.
#' @examples
check_subtracted_forward <- function(df) {
  
  df_check <- df %>% 
    select(date, alpha.code, contains("draft"), add.backward_sec4, ahead_sec4, behind_sec4) %>% 
    rename_all(list(~str_replace(., "draft_", ""))) %>% 
    rename_all(list(~str_replace(., "_sec4", ""))) %>% 
    mutate(total.by.section = sec1 + sec2 + sec3 + sec4 + add.backward,
           total.by.known = abs(ahead) + behind,
           diff.total = total.by.known - total.by.section)
    
}


make_wbirds4_analysis <- function(df) {
  
  df_out <- df %>% 
    mutate(draft_sec1 = draft_sec1 + add.backward_sec4) %>% 
    select(date, alpha.code, contains("draft")) %>% 
    pivot_longer(cols = contains("draft"), names_to = "section", values_to = "section.count") %>% 
    mutate(section = gsub("draft_sec", "", section))
  
  
}




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
#' set section.1.final to whichever is greater between the sum of the section 1 positives and the absolute value of the sum of the section 1 negatives.  
#' set section.1.forward to the net negative in section 1, or 0, whichever is smaller
#' set section.2.final to section.2.tally minus section.1.forward (any net negatives from section 1 have already been counted, so they should be subtracted from subsequent sections). If this value is less than 0, then set section.2.final = 0
#' set section.2.forward to section.2. 
#'
#' @examples
subtract_forward_add_back <- function(df) {
  

subtracted_forward <- df %>% 
  mutate(draft_sec1 = positive_sec1,
         add.backward_sec1 = abs(negative_sec1),
         #add.backward_sec1 = ifelse(positive_sec1 + negative_sec1 < 0, abs(positive_sec1 + negative_sec1), 0), pretty sure no ifelse needed here, should just be the section 1 negatives
         known.ahead_sec1 = negative_sec1,
         known.behind_sec1 = positive_sec1,
         # section 2
         draft_sec2 = ifelse(positive_sec2 + known.ahead_sec1 >= 0, positive_sec2 + known.ahead_sec1, 0), # subtract any neg_sec1, if more neg_sec1 than pos_sec2, draft_sec2 = 0
         add.backward_sec2 = add.backward_sec1 + ifelse(known.behind_sec1 < abs(negative_sec2), abs(known.behind_sec1 + negative_sec2), 0),
         known.ahead_sec2 = negative_sec2 + ifelse(known.ahead_sec1 + positive_sec2 > 0, 0, known.ahead_sec1 + positive_sec2),
         #known.behind_sec2 = positive_sec2 + ifelse(draft_sec1 + negative_sec2 > 0, draft_sec1 + negative_sec2, 0),
         known.behind_sec2 = positive_sec2 + ifelse(known.behind_sec1 + negative_sec2 > 0, known.behind_sec1 + negative_sec2, 0),
         # section 3
         draft_sec3 = ifelse(positive_sec3 + known.ahead_sec2 >= 0, positive_sec3 + known.ahead_sec2, 0),
         add.backward_sec3 = add.backward_sec2 + ifelse(known.behind_sec2 < abs(negative_sec3), abs(known.behind_sec2 + negative_sec3), 0),
         known.ahead_sec3 = negative_sec3 + ifelse(known.ahead_sec2 + positive_sec3 > 0, 0, known.ahead_sec2 + positive_sec3),
         #known.behind_sec3 = positive_sec3 + ifelse(draft_sec2 + negative_sec3 > 0, draft_sec2 + negative_sec3, 0),
         known.behind_sec3 = positive_sec3 + ifelse(known.behind_sec2 + negative_sec3 > 0, known.behind_sec2 + negative_sec3, 0),
         # section 4
         draft_sec4 = ifelse(positive_sec4 + known.ahead_sec3 >= 0, positive_sec4 + known.ahead_sec3, 0),
        
         add.backward_sec4 = add.backward_sec3 + ifelse(known.behind_sec3 < abs(negative_sec4), abs(known.behind_sec3 + negative_sec4), 0),
         
         known.ahead_sec4 = negative_sec4 + ifelse(known.ahead_sec3 + positive_sec4 > 0, 0, known.ahead_sec3 + positive_sec4),
         #known.behind_sec4 = positive_sec4 + ifelse(draft_sec3 + negative_sec4 > 0, draft_sec3 + negative_sec4, 0),
         known.behind_sec4 = positive_sec4 + ifelse(known.behind_sec3 + negative_sec4 > 0, known.behind_sec3 + negative_sec4, 0)) %>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

}








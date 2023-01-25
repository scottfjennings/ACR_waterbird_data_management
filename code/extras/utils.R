

#' Reduce by block waterbird data to just the original sections and transects
#' 
#'
#' @param zblock_sums data frame with at least section, transect, positive and negative fields
#' @param reduce2 if TRUE (default) combines section 2a and 2b into section 2
#' @param reduce5 if TRUE (default) combines section 5 into section 4
#' @param reduce.middle if TRUE (default) combines middle east and middle west into middle transect
#'
#' @return data frame with same structure as input
#' @export
#' 
#' @details adds positive and negative values separately
#'
#' @examples
#' 
#' zblock_pos_neg <- expand.grid(section = c("1", "2", "2a", "2b", "3", "4", "5"), transect = c("east", "middle.east", "middle east", "middle_east", "west"))
#' zblock_pos_neg2 <- reduce_section_transect(zblock_pos_neg)
reduce_section_transect <- function(zblock_pos_neg, reduce2 = TRUE, reduce5 = TRUE, reduce.middle = TRUE) {
df1 <- zblock_pos_neg %>% 
  mutate(section = as.character(section),
         transect = as.character(transect))

if(reduce2 == TRUE) {
  df1 <- df1 %>% mutate(section = ifelse(grepl("2", section), 2, section))
}
if(reduce2 == TRUE) {
  df1 <- df1 %>% mutate(section = ifelse(section == 5, 4, section))
}
if(reduce.middle == TRUE) {
  df1 <- df1 %>% mutate(transect = ifelse(grepl("middle", transect), "middle", transect))
}

df1 <- df1 %>% 
  group_by(date, alpha.code, section, transect) %>% 
  summarise(positive = sum(positive),
            negative = sum(negative)) %>% 
  ungroup()

}


# calculate net section tally and final section data record for each block.
# 

#' By block positive and negative to net total and net positive.
#' 
#' Calculate the net number of birds (positive - negative) and net positive (0 if net is negative) for each block. These are the values in the Yellow and Pink cells, repectively, in rows 15-22 of the NegMachine
#'
#' @param zblock_pos_neg data frame with the total positive and negative birds counted in separate columns 
#'
#' @return data frame with 
#' @export
#'
#' @examples
#' section_field_tally_final_data <- block_pos_neg_to_net_final(block_pos_neg)
block_pos_neg_to_net_final <- function(zblock_pos_neg) {
section_field_tally_final_data <- zblock_pos_neg %>% 
  mutate(net.section.field.tally = positive + negative,
         final.section.data.record = ifelse(net.section.field.tally < 0, 0, net.section.field.tally)) %>%
  select(-positive, -negative)
}


# 
#' fill in 0s to by block waterbird data 
#'
#' @param zblock_sums data frame with at least date, alpha.code, section, transect, positive, and negative fields
#'
#' @return
#' @export
#'
#' @examples
#' block_sums_buff <- expand.grid(section = c("1", "2", "3", "4", "5"), transect = c("east", "middle.east", "middle.west", "west"), date = "2022-01-24", alpha.code = "BUFF") %>% mutate(positive = rpois(20, 100), negative = -1 * rpois(20, 10))
#' block_sums_dcco <- expand.grid(section = c("2", "3", "4"), transect = c("east", "middle.east"), date = "2022-01-24", alpha.code = "DCCO") %>% mutate(positive = rpois(6, 100), negative = -1 * rpois(6, 10))
#' block_sums <- bind_rows(block_sums_buff, block_sums_dcco) %>% arrange(date, alpha.code, section, transect)
#' block_sums_filled <- fill_block_zeros(block_sums)
fill_block_zeros <- function(zblock_sums) {
zblock_sums_filled <- expand.grid(date = distinct(zblock_sums, date)$date,
                      alpha.code = distinct(zblock_sums, alpha.code)$alpha.code,
                      section = distinct(zblock_sums, section)$section,
                      transect = distinct(zblock_sums, transect)$transect) %>% 
  right_join(., distinct(zblock_sums, date, alpha.code)) %>% 
  right_join(., distinct(zblock_sums, section, transect)) %>% 
  full_join(zblock_sums) %>% 
  mutate(positive = ifelse(is.na(positive), 0, positive),
         negative = ifelse(is.na(negative), 0, negative)) %>% 
  arrange(date, alpha.code, transect, section)
}

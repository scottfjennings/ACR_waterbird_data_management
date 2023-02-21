


#' Assign precount section
#' 
#' Assign the appropriate section to the precount data, so that they can be merged with the corect section data.
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
assign_precount_section <- function(df) {
df <- df %>% 
  mutate(section = case_when(transect == "cypressgrove" ~ "2",
                             transect == "millertonbiv" ~ "1",
                             transect == "bivalve" ~ "1",
                             transect == "walkercreek" ~ "3",
                             transect == "inverness" ~ "1",
                             TRUE ~ as.character(section)))
}



#' Combine sections 2a and 2b into section 2
#'
#' @param df data frame with at least date, alpha.code, section, transect, positive, and negative fields
#'
#' @return
#' @export data frame with date, alpha.code, section, transect, positive, and negative fields
#'
#' @details adds positive and negative values separately
#' 
#' @examples
combine_section_2 <- function(df) {
  df <- df %>% 
    mutate(section = as.character(section),
           section = ifelse(grepl("2", section), 2, section))%>% 
  group_by(date, alpha.code, section, transect) %>% 
  summarise(positive = sum(positive),
            negative = sum(negative)) %>% 
  ungroup()
}


#' Title
#'
#' @param df data frame with at least date, alpha.code, section, transect, positive, and negative fields
#'
#' @return
#' @export data frame with date, alpha.code, section, transect, positive, and negative fields
#'
#' @details adds positive and negative values separately
#' 
#' @examples
combine_section_4_5 <- function(df) {
  df <- df %>% 
    mutate(section = as.character(section),
           section = ifelse(section == 5, 4, section))%>%
    group_by(date, alpha.code, section, transect) %>%
    summarise(positive = sum(positive),
              negative = sum(negative)) %>%
    ungroup()
}


#' Combine middle east and middle west transects into a single middle transect
#'
#' @param df data frame with at least date, alpha.code, section, transect, positive, and negative fields
#'
#' @return
#' @export data frame with date, alpha.code, section, transect, positive, and negative fields
#'
#' @details adds positive and negative values separately
#' 
#' @examples
combine_middle <- function(df)  {
  df <- df %>% 
    mutate(transect = as.character(transect),
           transect = ifelse(grepl("middle", transect), "middle", transect)) %>%
    group_by(date, alpha.code, section, transect) %>%
    summarise(positive = sum(positive),
              negative = sum(negative)) %>%
    ungroup()
}



#' Calculate by block positive and negative 
#'
#' @param zwbird_clean data frame with positive and negative tallies in the same column, called "tally"
#'
#' @return
#' @export
#'
#' @examples
make_block_pos_neg <- function(zwbird_clean) {
block_pos_neg <- zwbird_clean %>% 
  mutate(pos.neg = ifelse(tally > 0, "positive", "negative")) %>% 
  group_by(date, alpha.code, section, transect, pos.neg) %>% 
  summarise(block.sum = sum(tally)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(date, alpha.code, section, transect), names_from = pos.neg, values_from = block.sum) %>% 
  mutate(across(c(positive, negative), ~replace_na(., 0)))
}


# calculate net section tally and final section data record for each block.
# 

#' By block positive and negative to net total and net positive.
#' 
#' Calculate the net number of birds (positive - negative) and net positive (0 if net is negative) for each block. These are the values in the Yellow and Pink cells, repectively, in rows 15-22 of the NegMachine
#'
#' @param zblock_pos_neg data frame with the total positive and negative birds counted in separate columns (e.g. output from make_block_pos_neg()) 
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
  data.frame() %>% 
  right_join(., distinct(zblock_sums, date, alpha.code)) %>% 
  right_join(., distinct(zblock_sums, date, section, transect)) %>% 
  full_join(zblock_sums) %>% 
  mutate(positive = ifelse(is.na(positive), 0, positive),
         negative = ifelse(is.na(negative), 0, negative)) %>% 
  arrange(date, alpha.code, transect, section)
}





#' Make precount block names match format of main count blocks
#'
#' @param df data frame with at least a field named "block" 
#'
#' @return data frame with all fields in df
#' @export
#'
#' @examples
#' wbird_fix_precount_block_names()
fix_precount_block_names <- function(df) {
  df <- df %>% 
    mutate(block = gsub("east.cypress_grove", "cypressgrove.sec2", block),
           block = gsub("east.millerton_bivalve", "millertonbivalve.sec1", block),
           block = gsub("east.bivalve", "bivalve.sec1", block),
           block = gsub("east.walker_creek", "walkercreek.sec3", block),
           block = gsub("west.inverness", "inverness.sec1", block))
  
}


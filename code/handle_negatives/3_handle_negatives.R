







#' Title
#'
#' @param df generally data_with_allocated from step 2 splitting lumped species
#'
#' @return
#' @export
#'
#' @examples
#' readRDS(here("data_files/entered_raw_combined/data_with_allocated")) %>% 
#' calculate_carried_forward()
calculate_carried_forward <- function(df) {
carry_forward <- df %>% 
  pivot_longer(cols = c(section.positive, section.negative), names_to = "which.tally", values_to = "tally") %>% 
  mutate(which.tally = gsub("section\\.", "", which.tally),
         section = paste("sec", section, which.tally, sep = ".")) %>% 
  pivot_wider(id_cols = c(date, alpha.code), names_from = section, values_from = tally) %>% 
  mutate(across(contains("sec"), ~replace_na(., 0)),
         bay.total = (sec.1.positive + sec.1.negative) + (sec.2.positive + sec.2.negative) + (sec.3.positive + sec.3.negative) + (sec.4.positive + sec.4.negative)) %>% 
  arrange(date, alpha.code)

}




#' Calculate the net sum number of birds for each species at the section scale
#'
#' @param df generally data_with_allocated from step 2 splitting lumped species
#'
#' @return
#' @export
#'
#' @examples
calculate_net_sums <- function(df) {
net_sums <- df %>% 
  mutate(net.sum = section.positive + section.negative#,
         #section = paste("sec", section, sep = ".")
         ) %>% 
  #mutate(add.back = ifelse(section == 4 & net.sum < 0, abs(net.sum), 0)) %>% 
  group_by(date, alpha.code) %>% 
  mutate(bay.total = sum(net.sum)) %>% 
#  pivot_wider(id_cols = c(date, alpha.code, bay.total), names_from = section, values_from = net.sum) %>% 
#  mutate(across(contains("sec"), ~replace_na(., 0))) %>% 
#  dplyr::relocate(bay.total, .after = everything())%>% 
  arrange(date, alpha.code, section)
}



calculate_section_sums_subtract_forward <- function(df) {
subtracted_forward <- df %>% 
  arrange(date, alpha.code, section) %>% 
  group_by(date, alpha.code) %>% 
  mutate(adj.section = section.positive + lag(section.negative),
         adj.section.positive = ifelse(is.na(adj.section), section.positive, adj.section),
         adj.section.positive = ifelse(adj.section.positive < 0, 0, adj.section.positive),
         bay.total = sum(adj.section.positive, na.rm = TRUE))
}

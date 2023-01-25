
#' Title
#'
#' @param zblock_sums data frame with net positive and negative tallies for each block
#'
#' @return
#' @export
#'
#' @examples
neg_machine_logic_and_structure <- function(zsection_field_tally_final_data) {

# zsection_field_tally_final_data <- section_field_tally_final_data

zsection_field_tally_final_data <- zsection_field_tally_final_data %>% 
  mutate(section = as.character(section))


# calculate the section sum (row 24) and cumulative net field tally (row 25) for each section under the YELLOW cells.
# since this is iterative across sections, it is easiest to pivot wide (1 row for each date X species) for these calculations then pivot back longer to a row for each date X species X section 
field_tally_section_sums <- zsection_field_tally_final_data %>% 
  group_by(date, alpha.code, section) %>% 
  summarise(section.sum = sum(net.section.field.tally)) %>% 
  ungroup() %>% 
  mutate(section = paste("section.sum_", section, sep = "")) %>% 
  pivot_wider(id_cols = c(date, alpha.code), values_from = section.sum, names_from = section) %>% 
  mutate(cumulative.net.field.tally_1 = ifelse(section.sum_1 < 0, section.sum_1, 0),
         cumulative.net.field.tally_2 = cumulative.net.field.tally_1 + section.sum_2,
         cumulative.net.field.tally_3 = cumulative.net.field.tally_2 + section.sum_3,
         cumulative.net.field.tally_4 = cumulative.net.field.tally_3 + section.sum_4)%>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) %>% 
  pivot_longer(-c("date", "alpha.code")) %>% 
  separate(name, c("val.type", "section"), sep = "_") %>% 
  pivot_wider(id_cols = c(date, alpha.code, section), names_from = val.type, values_from = value) %>% 
  mutate(section = as.numeric(section))
 
# calculate proportion of total birds in each transect
# and distribute section 4 negatives to each transect
departed_allocated_by_transect <- zsection_field_tally_final_data %>% 
  # need to add precounts to the appropriate transects
  mutate(transect = case_when(transect %in% c("bivalve", "millertonbiv", "walkercreek", "cypressgrove") ~ "east",
                             transect == "inverness" ~ "west",
                             TRUE ~ as.character(transect))) %>% 
  group_by(date, alpha.code, transect) %>%
  summarise(transect.tot = sum(final.section.data.record)) %>% 
  ungroup() %>% 
  group_by(date, alpha.code) %>% 
  mutate(pre.bay.total = sum(transect.tot)) %>% 
  ungroup() %>% 
  mutate(bay.trans.proportion = transect.tot/pre.bay.total,
         # where there aren't birds in sections 1-3 to calculate proportion, allocate all sec 4 negatives to the middle transect
         bay.trans.proportion = ifelse(!is.nan(bay.trans.proportion), bay.trans.proportion, ifelse(transect == "middle", 1, 0))) %>%
  select(date, alpha.code, transect, bay.trans.proportion) %>% 
  # add in the sec 4 negatives
  right_join(field_tally_section_sums %>% filter(section == 4 & cumulative.net.field.tally < 0)) %>% 
  mutate(departed.allocated.by.transect = round(abs(cumulative.net.field.tally) * bay.trans.proportion, 0)) %>% 
  mutate(section = as.character(section))
  

wide_field_tally_final_data <- zsection_field_tally_final_data %>% 
  full_join(departed_allocated_by_transect %>% select(date, alpha.code, transect, section, departed.allocated.by.transect)) %>% 
  pivot_longer(cols = c(final.section.data.record, net.section.field.tally, departed.allocated.by.transect), names_to = "tally.type", values_to = "tally") %>% 
  filter(!is.na(tally)) %>% 
  mutate(tally.type = paste(tally.type, section, sep = "_")) %>% 
  pivot_wider(id_cols = c(date, alpha.code, transect), names_from = tally.type, values_from = tally) %>% 
  rename("prelim.section.data.record_4" = final.section.data.record_4) %>% 
  mutate(departed.allocated.by.transect_4 = replace_na(departed.allocated.by.transect_4, 0),
         final.section.data.record_4 = prelim.section.data.record_4 + departed.allocated.by.transect_4)



# now put it all together so it looks like the negative machine
# widen row 24 and 25 values under yellow cells  
wide_field_tally_section_sums <- field_tally_section_sums %>% 
  pivot_longer(cols = c(section.sum, cumulative.net.field.tally), names_to = "transect", values_to = "net.section.field.tally") %>% 
  pivot_longer(cols = net.section.field.tally, names_to = "tally.type", values_to = "tally") %>% 
  mutate(tally.type = paste(tally.type, section, sep = "_"))  %>% 
  pivot_wider(id_cols = c(date, alpha.code, transect), names_from = tally.type, values_from = tally)  

# widen row 24 and 25 values under pink cells
wide_final_section_sums <- zsection_field_tally_final_data %>%
  group_by(date, alpha.code, section) %>% 
  summarise(section.sum = sum(final.section.data.record)) %>% 
  ungroup() %>% 
  pivot_longer(cols = section.sum, names_to = "transect", values_to = "final.section.data.record") %>% 
  pivot_longer(cols = final.section.data.record, names_to = "tally.type", values_to = "tally") %>% 
  mutate(tally.type = paste(tally.type, section, sep = "_"))  %>% 
  pivot_wider(id_cols = c(date, alpha.code, transect), names_from = tally.type, values_from = tally) %>% 
  rename("prelim.section.data.record_4" = final.section.data.record_4)

total_final_4 <- wide_field_tally_final_data %>% 
  group_by(date, alpha.code) %>% 
  summarise(section.sum = sum(final.section.data.record_4, na.rm = TRUE)) %>% 
  pivot_longer(cols = section.sum, names_to = "transect", values_to = "final.section.data.record_4")

neg_machine_out <- full_join(wide_field_tally_section_sums, wide_final_section_sums) %>% 
  full_join(total_final_4) %>% 
  bind_rows(wide_field_tally_final_data) %>% 
  # add up the section final data sums and any added back section 4 negatives to get a baywide total
  mutate(bay.total = ifelse(transect == "section.sum", final.section.data.record_1 +
                                                       final.section.data.record_2 +
                                                       final.section.data.record_3 +
                                                       final.section.data.record_4, NA)) %>% 
  mutate(transect = factor(transect, levels = c("east", "middle", "west", "inverness", "bivalve", "millertonbiv", "walkercreek", "cypressgrove", "section.sum", "cumulative.net.field.tally"))) %>% 
  arrange(date, alpha.code, transect) %>% 
  select(date, alpha.code, transect, 
         net.section.field.tally_1, final.section.data.record_1, 
         net.section.field.tally_2, final.section.data.record_2,
         net.section.field.tally_3, final.section.data.record_3,
         net.section.field.tally_4, prelim.section.data.record_4, departed.allocated.by.transect_4, final.section.data.record_4, 
         bay.total)

}


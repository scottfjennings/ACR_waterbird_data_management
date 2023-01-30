
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source(here("code/extras/utils.R"))


  





# data_with_allocated <- readRDS(here("data_files/entered_raw_combined/data_with_allocated"))


carry_forward <- calculate_carried_forward(data_with_allocated) 


net_sums <- calculate_net_sums(data_with_allocated) %>% 
  rename("bay.total.net" = bay.total)

sub_forward_sums <- calculate_section_sums_subtract_forward(data_with_allocated)


comp_sums = full_join(net_sums, sub_forward_sums %>% select(-adj.section)) %>% 
  relocate(bay.total.net, .after = everything()) %>% 
  ungroup()


comp_sums %>% 
  distinct(date, alpha.code, bay.total, bay.total.net) %>% 
  mutate(total.diff = bay.total - bay.total.net) %>% 
  arrange(total.diff) %>% view()
  write.csv(paste(here("data_files/derived_data/"), "/CBC_",  year(Sys.Date()), ".csv", sep = ""), row.names = FALSE)

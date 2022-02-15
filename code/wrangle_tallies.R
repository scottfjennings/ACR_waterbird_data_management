

library(tidyverse)
library(stringr)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


# for tallies entered as character string in single cell ----
tallies <- read.csv(here("data_files/entered_raw/tallies.csv")) %>% 
  mutate(alpha.code = toupper(alpha.code)) %>% 
  filter(tally != "") %>% 
  mutate(row.index = row_number())


split_tallies <- function(zrow.index) {
tally1 <- filter(tallies,
                 row.index == zrow.index)

tally_ho <- str_split(tally1$tally, pattern = ",") %>% 
  unlist() %>% 
  data.frame() %>% 
  rename(tally = 1) %>% 
  mutate(row.index = tally1$row.index,
         tally = trimws(tally),
         tally = as.numeric(tally))
}


split_tallies <- map_df(tallies$row.index, split_tallies) %>% 
  full_join(tallies %>% select(-tally))

split_tallies_pos_neg <- split_tallies %>% 
  mutate(pos.neg = ifelse(tally < 0, "neg.tally", "pos.tally")) %>%
  group_by(date, transect, section, alpha.code, pos.neg) %>% 
  summarise(tally.sum = sum(tally)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(date, transect, section, alpha.code), names_from = pos.neg, values_from = tally.sum) %>% 
  mutate(net.tally = pos.tally + neg.tally)


write.csv(split_tallies_pos_neg, here("data_files/tracking_finding_problem_records/summed_tallies.csv"), row.names = FALSE)



# for tallies entered one per row ----


long_tallies <- read.csv(here("data_files/entered_raw/long_tallies.csv")) %>% 
  mutate(alpha.code = toupper(alpha.code))



long_tallies_pos_neg <- long_tallies %>% 
  mutate(pos.neg = ifelse(tally < 0, "neg.tally", "pos.tally"),
         transect = gsub("e\\.|w\\.", "", transect), # to combine the 2 middle transects
         section = ifelse(section == 5, 4, section)) %>% 
  group_by(date, transect, section, alpha.code, pos.neg) %>% 
  summarise(tally.sum = sum(tally)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(date, transect, section, alpha.code), names_from = pos.neg, values_from = tally.sum) %>% 
  mutate(pos.tally = replace_na(pos.tally, 0),
         neg.tally = replace_na(neg.tally, 0),
         net.tally = pos.tally + neg.tally)



# compare raw tallies to values already in db

wbirds_v_tallies <- long_tallies_pos_neg %>% 
  mutate(date = mdy(date),
         section = as.character(section)) %>% 
  left_join(., readRDS(here("data_files/wbirds"))) %>% 
  mutate(mismatch = count - net.tally) %>% 
  arrange(mismatch)


filter(wbirds_v_tallies, mismatch != 0) %>% view()

write.csv(here("data_files/tracking_finding_problem_records/tally_db_mismatch.csv"), row.names = FALSE)
                              
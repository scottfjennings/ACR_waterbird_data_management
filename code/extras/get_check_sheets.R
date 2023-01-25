




library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")




#' read raw tally files
#'
#' @param ztally_file path to a tally data sheet xlsx file
#'
#' @return
#' @export
#'
#' @details reads raw data xlsx file, fixes column names and reshapes to long format. Note, does not convert port/starboard transects to east/west
#'
#' @examples
get_check_sheets <- function(ztally_file) {

  survey_date_file <- gsub("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data/|\\_p.xlsx|\\_p2.xlsx", "", ztally_file) %>% 
  ymd()
  
raw_check <- read_excel(ztally_file, sheet = "check sheets", .name_repair = "universal") %>% 
  mutate_all(as.character) %>% 
  mutate(date = survey_date_file) 

long_check <- raw_check %>% 
  pivot_longer(-c("date", "species", "sheet")) %>% 
  filter(!is.na(value))

clean_check = long_check %>% 
  group_by(date, species, sheet) %>% 
  summarise(note = paste(value, collapse = ". "))

return(clean_check)
}




tally_files = list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data", full.names = TRUE, pattern = "_p") %>%
   stringr::str_subset(., "template", negate = TRUE)


raw_check_sheets <- map_df(tally_files, get_check_sheets)


tracking_check_sheets <- read_excel("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/waterbird_raw_entry_tracking.xlsx", sheet = "paper sheets to check", .name_repair = "universal") %>% 
  mutate(date = ymd(date))


all_check_sheets <- bind_rows(raw_check_sheets, tracking_check_sheets) %>% 
  distinct() %>% 
  arrange(date, sheet, species)


write.csv(all_check_sheets, "C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/all_check_sheets.csv", row.names = FALSE)

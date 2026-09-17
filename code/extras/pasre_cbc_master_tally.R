


library(tidyverse)
library(here)
library(readr)
library(birdnames)


wbird_keep_taxa <- c("AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")

custom_bird_list <- readRDS("C:/Users/scott.jennings.EGRET/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

cbc <- read_csv(
  "V:/Waterbirds_data/CBC Tallies/PRBC 2025 MASTER Tally Sheet - Field Checklist.csv",
  skip = 1,
  show_col_types = FALSE
)

# rename the blank/auto column if you want
names(cbc)[1] <- "common.name"


cbc_out <- cbc %>% 
  select(common.name, "Tomales Bay Boats") %>% 
  rename(bay.total = "Tomales Bay Boats") %>% 
  filter(!is.na(common.name), !is.na(bay.total)) %>% 
  mutate(common.name = case_when(common.name == "Brant (Black)" ~ "Brant",
                                 common.name == "Osprey*" ~ "Osprey",
                                 common.name == "scaup sp." ~ "Scaup spp",
                                 TRUE ~ common.name),
         alpha.code = translate_bird_names(common.name, "common.name", "alpha.code"),
         date = as.Date("2025-12-20"),
         bay.total = as.numeric(bay.total)) %>%
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% 
  select(-common.name) %>% 
  distinct()


saveRDS(cbc_out, here("data_files/working_rds/parsed_cbc_2025"))  



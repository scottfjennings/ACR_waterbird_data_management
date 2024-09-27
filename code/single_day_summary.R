

library(tidyverse)
library(here)
library(flextable)
library(birdnames)

# working locally
custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")
# working on Azure
custom_bird_list <- readRDS("C:/Users/scott.jennings.EGRET/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

zdate = "2024-01-27"

bay_total <- readRDS(here("data_files/working_rds/new_neg_machine_bay_total"))

lumped_scaup <- bay_total %>% 
  filter(alpha.code %in% c("SCAUP", "GRSC", "LESC")) %>% 
  group_by(date) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  ungroup() %>% 
  mutate(alpha.code = "lump.scaup")

bay_total_all <- bay_total %>%
  mutate(alpha.code = "all") %>% 
  group_by(date, alpha.code) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  bind_rows(bay_total) %>% 
  bind_rows(lumped_scaup)

not_zdate <- bay_total_all %>% 
  filter(date != zdate) %>% 
  group_by(alpha.code) %>% 
  summarise(min.count = min(bay.total),
            mean.count = round(mean(bay.total), 1),
            max.count = max(bay.total)) %>%
  ungroup() 


yes_zdate <- bay_total_all %>% 
              filter(date == "2024-01-27")


out_dat <- right_join(not_zdate, yes_zdate) %>% 
  mutate(common.name = case_when(alpha.code == "all" ~ "All species combined", 
                                 alpha.code == "lump.scaup" ~ "Combined scaup", 
                                 TRUE ~ translate_bird_names(alpha.code, "alpha.code", "common.name")),
         bay.total = replace_na(bay.total, 0)) %>% 
  select(common.name, bay.total, contains("count"), -date, -alpha.code) %>% 
  arrange(-bay.total) %>% 
  flextable::flextable() %>% 
  flextable::set_header_labels(common.name = "Species",
                               bay.total = "Total counted\nJan 27, 2024",
                               min.count = "min",
                               mean.count = "mean",
                               max.count = "max") %>% 
  flextable::add_header_row(values = c("", "Summary of prior single\nday values"), colwidths = c(2, 3)) %>% 
  flextable::autofit() %>% 
  align(j = 2:5, align = "center", part = "all") %>% 
  flextable::save_as_docx(path = here("figures_output/20240127_waterbirds.docx"))


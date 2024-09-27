

library(tidyverse)
library(here)
library(flextable)
library(birdnames)

# working locally
custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")
# working on Azure
custom_bird_list <- readRDS("C:/Users/scott.jennings.EGRET/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

zseason = "2023"

bay_total <- readRDS(here("data_files/working_rds/new_neg_machine_bay_total")) %>% 
  mutate(season = ifelse(month(date) == 12, year(date), year(date) - 1))

lumped_scaup <- bay_total %>% 
  filter(alpha.code %in% c("SCAUP", "GRSC", "LESC")) %>% 
  group_by(season, date) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  ungroup() %>% 
  mutate(alpha.code = "lump.scaup")

bay_total_all <- bay_total %>%
  mutate(alpha.code = "all") %>% 
  group_by(season, date, alpha.code) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  ungroup() %>% 
  bind_rows(bay_total) %>% 
  bind_rows(lumped_scaup)

not_zseason <- bay_total_all %>% 
  filter(season != zseason) %>% 
  group_by(alpha.code) %>% 
  summarise(min.count = min(bay.total),
            mean.count = round(mean(bay.total), 1),
            max.count = max(bay.total)) %>%
  ungroup() 


yes_zseason <- bay_total_all %>% 
  filter(season == zseason) %>% 
  mutate(md = paste(month(date, label = TRUE, abbr = TRUE), day(date))) %>% 
  pivot_wider(id_cols = alpha.code, names_from = md, values_from = bay.total)

md_cols <- yes_zseason %>% 
  select(-alpha.code) %>% 
  colnames()

yes_zseason <- yes_zseason %>% mutate_at(all_of(md_cols), replace_na, 0)

zseason.out = paste(zseason, as.numeric(str_sub(zseason,-2,-1)) + 1, sep = "/")

right_join(not_zseason, yes_zseason) %>% 
  mutate(common.name = case_when(alpha.code == "all" ~ "All species combined", 
                                 alpha.code == "lump.scaup" ~ "Combined scaup", 
                                 TRUE ~ translate_bird_names(alpha.code, "alpha.code", "common.name"))) %>% 
  select(common.name, md_cols, contains("count"), -alpha.code) %>% 
  arrange(-mean.count) %>% 
  flextable::flextable() %>% 
  flextable::set_header_labels(common.name = "Species",
                               min.count = "min",
                               mean.count = "mean",
                               max.count = "max") %>% 
  flextable::add_header_row(values = c("", paste("Total counted", zseason.out), "Summary of single day values\nall prior seasons"), colwidths = c(1, length(md_cols), 3)) %>% 
  flextable::autofit() %>% 
  align(j = 2:5, align = "center", part = "all") %>% 
  flextable::save_as_docx(path = here(paste("figures_output/", zseason, "_waterbirds.docx", sep = "")))


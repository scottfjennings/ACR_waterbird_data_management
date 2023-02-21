

library(xlsx)
library(tidyverse)
library(stringr)
library(lubridate)
library(here)
library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

neg_list <- list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/single_sp", full.names = TRUE)

neg_pooled_list <- list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/pooled", full.names = TRUE)

neg_list_1sp <- list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/Neg Tallies to be reviewd by SJ-check for dups/one_sp_date", full.names = TRUE)

neg_list_multisp <- list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/Neg Tallies to be reviewd by SJ-check for dups/multi_spp_date", full.names = TRUE)



# these 4 functions extract the raw data (field tallies) from the negative machines ----
# first the single species negative machines ----


get_machine_tallies <- function(zfile) {

negmachine <- read.xlsx(zfile, sheetIndex = 1, startRow = 13, endRow = 22, header = T) %>% 
  data.frame() 

tallies <- negmachine %>% 
  select(Transect, contains("tall")) %>% 
  pivot_longer(cols = contains("tall"), names_to = "section", values_to = "tally") %>% 
  mutate(section = gsub("Net", "", section),
         section = gsub("Section.", "", section),
         section = gsub(".field.tally", "", section),
         section = gsub("\\.", "", section)) %>% 
  filter(!is.na(Transect)) %>% 
  filter(!(is.na(tally) & Transect %in% c("INVERNESS", "BIVALVE", "MILLERTON", "WALKER", "CGP"))) %>% 
  mutate(date.alpha = zfile,
         date.alpha = gsub("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/single_sp/NegWBMachine", "", date.alpha),
         date.alpha = gsub(".xlsx", "", date.alpha))


}

wbird_from_neg <- map_df(neg_list, get_machine_tallies)  %>% 
  separate(date.alpha,  
           into = c("date", "alpha.code"),
           sep = 8) %>% 
  mutate(neg.machine = "single",
         date = ymd(date))


# now the pooled negative machines ----


get_pooled_machine_tallies <- function(zfile) {

negmachine <- read.xlsx(zfile, sheetIndex = 1, startRow = 13, endRow = 22, header = T) %>% 
  data.frame() 

tallies <- negmachine %>% 
  select(Transect, contains("tall")) %>% 
  pivot_longer(cols = contains("tall"), names_to = "xl.head", values_to = "tally") %>% 
  filter(!is.na(Transect)) %>% 
  filter(!(is.na(tally) & Transect %in% c("INVERNESS", "BIVALVE", "MILLERTON", "WALKER", "CGP"))) %>% 
  filter(!grepl("lost.birds", xl.head)) %>% 
  mutate(date.alpha = zfile,
         date.alpha = gsub("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/pooled/NegWBMachinePOOLED", "", date.alpha),
         date.alpha = gsub(".xlsx", "", date.alpha))


}



wbird_from_neg_pooled <- map_df(neg_pooled_list, get_pooled_machine_tallies)

wbird_from_neg_pooled <- wbird_from_neg_pooled  %>% 
  separate(date.alpha,  
           into = c("date", "alpha.code.pool"),
           sep = 8) %>% 
  mutate(alpha.code1 = case_when(grepl("DCCO", xl.head) ~ "DCCO",
                                grepl("PECO", xl.head) ~ "PECO",
                                grepl("BRCO", xl.head) ~ "BRCO", 
                                grepl("GRSC", xl.head) ~ "GRSC", 
                                grepl("LESC", xl.head) ~ "LESC", 
                                grepl("COLO", xl.head) ~ "COLO", 
                                grepl("PALO", xl.head) ~ "PALO", 
                                grepl("RTLO", xl.head) ~ "RTLO", 
                                grepl("PCLO", xl.head) ~ "PCLO")) %>% 
  mutate(alpha.code2 = case_when(alpha.code.pool == "SCAUP" & grepl("SPECIES.1", xl.head) ~ "GRSC",
                                 alpha.code.pool == "SCAUP" & grepl("SPECIES.2", xl.head) ~ "LESC",
                                 alpha.code.pool == "LOON" & grepl("SPECIES.1", xl.head) ~ "COLO",
                                 alpha.code.pool == "LOON" & grepl("SPECIES.2", xl.head)  ~ "PALO",
                                 alpha.code.pool == "LOON" & grepl("SPECIES.3", xl.head)  ~ "RTLO",
                                 alpha.code.pool == "LOON" & grepl("SPECIES.4", xl.head) ~ "PCLO",
                                 alpha.code.pool == "CORM" & grepl("SPECIES.1", xl.head) ~ "DCCO",
                                 alpha.code.pool == "CORM" & grepl("SPECIES.2", xl.head) ~ "BRCO",
                                 alpha.code.pool == "CORM" & grepl("SPECIES.3", xl.head) ~ "PECO",
                                 alpha.code.pool == "HEGR" & grepl("SPECIES.1", xl.head) ~ "HOGR",
                                 alpha.code.pool == "HEGR" & grepl("SPECIES.2", xl.head) ~ "EAGR")) %>% 
  mutate(alpha.code = case_when(!grepl("SPECIES", xl.head) ~ alpha.code.pool,
                                grepl("SPECIES", xl.head) & !is.na(alpha.code1) ~ alpha.code1,
                                grepl("SPECIES", xl.head) & is.na(alpha.code1) ~ alpha.code2)) %>% 
  mutate(section = str_extract(xl.head, "(?<=Section.)[0-9]*")) %>% 
  mutate(neg.machine = "pooled",
         date = ymd(date)) %>% 
  select(names(wbird_from_neg))



# combine and compare ----
all_neg <- bind_rows(wbird_from_neg, wbird_from_neg_pooled) %>%
  rename(transect = Transect, count = tally) %>% 
  mutate(transect = gsub("1=|2=|3=| SUM", "", transect),
         transect = gsub("MIDBAY", "middle", transect),
         transect = tolower(transect),
         transect = gsub("millerton", "millertonbivalve", transect),
         transect = gsub("cgp", "cypressgrove", transect),
         transect = gsub("walker", "walkercreek", transect),
         date = ymd(date),
         alpha.code = update_alpha(alpha.code)) %>%  
  arrange(date, alpha.code, section, transect)

count(all_neg, date, alpha.code, section, transect) %>% view()

all_neg_wide <- all_neg %>% 
  pivot_wider(id_cols = c("date", "alpha.code", "section", "transect"), names_from = neg.machine, values_from = count)


wbirds <- readRDS(here("data_files/wbirds"))


wbirds_all_neg <- right_join(wbirds, all_neg_wide)%>%  
  arrange(date, alpha.code, section, transect) %>% 
  mutate(negs.match = single == pooled,
         wbirds.single.match = count == single)


filter(wbirds_all_neg, negs.match == FALSE | wbirds.single.match == FALSE) %>% write.csv(here("data_files/tracking_finding_problem_records/problem_records.csv"), row.names = FALSE)

problem_records_working <- read.csv(here("data_files/tracking_finding_problem_records/problem_records_working.csv")) 

problem_records_working %>% 
  filter(!is.na(raw)) %>% 
  select(date, section, transect, alpha.code, count, single, pooled, raw, raw.pos, raw.neg, raw.note)  %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  mutate(wbirds.raw = count == raw,
         single.raw = single == raw) %>% 
  arrange(wbirds.raw, date, alpha.code, section) %>% 
  view()



##### do similar with another batch of negative machines that Emi found 1/31/22
# then single species negative magic machines ----


get_neg_magic_machine_tallies <- function(zfile) {

negmachine <- read.xlsx(zfile, sheetIndex = 1, startRow = 13, endRow = 22, header = T) %>% 
  data.frame() 

tallies <- negmachine %>% 
  select(Transect, contains("tall")) %>% 
  pivot_longer(cols = contains("tall"), names_to = "section", values_to = "tally") %>% 
  mutate(section = gsub("Net", "", section),
         section = gsub("Section.", "", section),
         section = gsub(".field.tally", "", section),
         section = gsub("\\.", "", section)) %>% 
  filter(!is.na(Transect)) %>% 
  filter(!(is.na(tally) & Transect %in% c("INVERNESS", "BIVALVE", "MILLERTON", "WALKER", "CGP"))) %>% 
  mutate(date.alpha = zfile,
         date.alpha = gsub("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/data_sheets_negmachine/NEGATIVETALLIES/Neg Tallies to be reviewd by SJ-check for dups/one_sp_date/NegativeWaterbirdMagicMachine", "", date.alpha),
         date.alpha = gsub(".xlsx", "", date.alpha))


}

magic_machine_1sp <- map_df(neg_list_1sp, get_neg_magic_machine_tallies)  %>% 
  separate(date.alpha,  
           into = c("date", "alpha.code"),
           sep = 8) %>% 
  mutate(neg.machine = "magic.1sp",
         date = ymd(date))



# then multi-species negative magic machines ----


get_neg_magic_machine_tallies_multi <- function(zfile) {

wb <- loadWorkbook(zfile)
sheets <- getSheets(wb) %>% 
  names()

read_sheet <- function(zsheet){  
negmachine <- read.xlsx(zfile, sheetName = zsheet, startRow = 3, endRow = 12, header = T) %>% 
  data.frame() 

tallies <- negmachine %>% 
  select(Transect, contains("tall")) %>% 
  pivot_longer(cols = contains("tall"), names_to = "section", values_to = "tally") %>% 
  mutate(section = gsub("Net", "", section),
         section = gsub("Section.", "", section),
         section = gsub(".field.tally", "", section),
         section = gsub("\\.", "", section)) %>% 
  filter(!is.na(Transect)) %>% 
  filter(!(is.na(tally) & Transect %in% c("INVERNESS", "BIVALVE", "MILLERTON", "WALKER", "CGP"))) %>% 
  filter(!grepl("lost.birds", section)) %>% 
  mutate(date.alpha = zsheet)
}
all_sheets <- map_df(sheets, read_sheet)
}

magic_machine_multi <- map_df(neg_list_multisp, get_neg_magic_machine_tallies_multi)  %>% 
  separate(date.alpha,  
           into = c("date", "alpha.code"),
           sep = " ") %>% 
  mutate(neg.machine = "magic.multi",
         date = ymd(date))


# combine and compare magic machines ----
all_neg_magic <- bind_rows(magic_machine_1sp, magic_machine_multi) %>%
  rename(transect = Transect, count = tally) %>% 
  mutate(transect = gsub("1=|2=|3=| SUM", "", transect),
         transect = gsub("MIDBAY", "middle", transect),
         transect = tolower(transect),
         transect = gsub("millerton", "millertonbivalve", transect),
         transect = gsub("cgp", "cypressgrove", transect),
         transect = gsub("walker", "walkercreek", transect),
         date = ymd(date),
         alpha.code = update_alpha(alpha.code)) %>%  
  arrange(date, alpha.code, section, transect)

# check for matches with regular nag machine
check_machine_matches <- all_neg_magic %>% 
  rename(magic.count = count) %>% 
  inner_join(., read.csv(here("data_files/tracking_finding_problem_records/problem_records.csv")) %>% mutate(date = mdy(date),
                                                                           section = as.character(section))) 



wbirds <- readRDS(here("data_files/wbirds"))


wbirds_all_neg_magic <- all_neg_magic %>% 
  rename(single = count) %>% 
  left_join(wbirds)  %>% 
  arrange(date, alpha.code, section, transect) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  mutate(wbirds.match.magic = count == single)


filter(wbirds_all_neg_magic, wbirds.match.magic == FALSE) %>% 
  select(date, transect, section, alpha.code, everything(), -neg.machine, -block) %>% 
  mutate(raw.entered = "") %>% 
  write.csv(here("data_files/problem_records_magic.csv"), row.names = FALSE)


# combine data from all 4 negative machines

all_neg_magic <- bind_rows(wbird_from_neg, wbird_from_neg_pooled, magic_machine_1sp, magic_machine_multi) %>%
  rename(transect = Transect, count = tally) %>% 
  mutate(transect = gsub("1=|2=|3=| SUM", "", transect),
         transect = gsub("MIDBAY", "middle", transect),
         transect = tolower(transect),
         transect = gsub("millerton", "millertonbivalve", transect),
         transect = gsub("cgp", "cypressgrove", transect),
         transect = gsub("walker", "walkercreek", transect),
         date = ymd(date),
         alpha.code = update_alpha(alpha.code)) %>%  
  arrange(date, alpha.code, section, transect)

all_neg_magic_wide <- all_neg_magic %>% 
  pivot_wider(id_cols = c("date", "alpha.code", "section", "transect"), names_from = neg.machine, values_from = count)


wbirds <- readRDS(here("data_files/wbirds"))


wbirds_all_neg <- right_join(wbirds, all_neg_magic_wide)%>% 
  select(-block) %>% 
  arrange(date, alpha.code, section, transect) %>% 
  mutate(no.match.single = count != single,
         no.match.multi = count != magic.multi,
         no.match.1sp = count != magic.1sp) 


wbirds_no_match <- filter(wbirds_all_neg, no.match.single == TRUE | no.match.multi == TRUE | no.match.1sp == TRUE)

working <- full_join(read.csv(here("data_files/problem_records_magic_working.csv")),
                     read.csv(here("data_files/problem_records_working.csv")))

worked <- working %>% 
  filter(raw.entered != "") %>% 
  select(date, transect, section, alpha.code, raw.entered, raw.note) %>% 
  mutate(date = mdy(date),
         section = as.character(section))

neg_worked <- full_join(wbirds_no_match, worked) %>% 
  mutate(transect = factor(transect, levels = c("east", "middle", "west", "bivalve", "inverness", "millertonbivalve", "cypressgrove", "walkercreek")),
         across(contains("raw"), ~replace_na(., ""))) %>% 
  arrange(date, section, transect, alpha.code)
  
  write.csv(neg_worked, here("data_files/all_problems_working.csv"), row.names = FALSE)
## these next functions get the carried and added back negatives ----
# neg machine single species ----

get_machine_negatives <- function(zfile) {

negmachine <- read.xlsx(zfile, sheetIndex = 1, startRow = 13, endRow = 25, header = T) %>% 
  data.frame() %>% 
  select(transect = Transect, added.back = contains("eparted"), final4 = contains("Final.Section.4"))  %>% 
  filter(!is.na(added.back)) %>% 
  mutate(date.alpha = zfile,
         date.alpha = sub(".*Machine", "", date.alpha), 
         date.alpha = gsub(".xlsx", "", date.alpha), 
         date.alpha = gsub("POOLED", "", date.alpha)) %>% 
  mutate(neg.machine = zfile,
         neg.machine = sub(".*NEGATIVETALLIES/", "", neg.machine),
         neg.machine = sub("/Neg.*", "", neg.machine),
         neg.machine = sub("Neg Tallies to be reviewd by SJ-check for dups/", "", neg.machine)) %>% 
  separate(date.alpha,  
           into = c("date", "alpha.code"),
           sep = 8) %>% 
  mutate(date = ymd(date),
         added.back = round(added.back, 0),
         final4 = round(final4, 0))

}

added_back_negmachine <- map_df(neg_list, get_machine_negatives)
  
added_back_pooled_negmachine <- map_df(neg_pooled_list, get_machine_negatives)
  
added_back_1sp_negmachine <- map_df(neg_list_1sp, get_machine_negatives)


get_magic_machine_negatives_multi <- function(zfile) {

wb <- loadWorkbook(zfile)
sheets <- getSheets(wb) %>% 
  names()

read_sheet <- function(zsheet){   
negmachine <- read.xlsx(zfile, sheetIndex = zsheet, startRow = 3, endRow = 7, header = T) %>% 
  data.frame() %>% 
  select(transect = Transect, added.back = contains("eparted"), final4 = contains("Final.Section.4"))  %>% 
  filter(!is.na(added.back))  %>% 
  mutate(date.alpha = zsheet) %>%
  mutate(neg.machine = zfile,
         neg.machine = sub(".*dups/", "", neg.machine),
         neg.machine = sub("/2.*", "", neg.machine)) %>% 
  separate(date.alpha,  
           into = c("date", "alpha.code"),
           sep = " ") %>% 
  mutate(date = ymd(date),
         added.back = round(added.back, 0),
         final4 = round(final4, 0))

}
all_sheets <- map_df(sheets, read_sheet)
}



added_back_magic_multi <- map_df(neg_list_multisp, get_magic_machine_negatives_multi)

all_added_back <- bind_rows(added_back_negmachine, added_back_pooled_negmachine, added_back_1sp_negmachine, added_back_magic_multi) %>% 
  filter(added.back > 0) %>% 
  arrange(date, alpha.code, transect) %>% 
  mutate(original4 = final4 - added.back)

wbirds <- readRDS(here("data_files/wbirds"))


wbirds_v_addedback <- all_added_back %>% 
  mutate(transect = gsub("1=|2=|3=| SUM", "", transect),
         transect = gsub("MIDBAY", "middle", transect),
         transect = tolower(transect),
         alpha.code = update_alpha(alpha.code),
         section = "4") %>% 
  filter(transect != "section sum") %>% 
  left_join(., readRDS(here("data_files/wbirds"))) %>% 
  mutate(count.v.final4 = count - final4,
         count.v.final4.v.addedback = abs(count.v.final4) - added.back) %>% 
  arrange(abs(count.v.final4))


problem_added_back <- wbirds_v_addedback %>%
  filter(count.v.final4.v.addedback != 0, alpha.code != "CORM") %>% 
  left_join(long_tallies_pos_neg %>% mutate(date = mdy(date), section = as.character(section))) %>% 
  select(date, transect, section, alpha.code, neg.machine, everything(), -block) %>% 
  mutate(count.v.tally = count - net.tally) %>% 
  filter(count.v.tally != 0 | is.na(count.v.tally))

write.csv(problem_added_back, here("data_files/tracking_finding_problem_records/tally_addedback_mismatch.csv"), row.names = FALSE)


## combine mismatch files ----
bind_rows(read.csv(here("data_files/tracking_finding_problem_records/tally_addedback_mismatch.csv")),
          read.csv(here("data_files/tracking_finding_problem_records/tally_db_mismatch.csv"))) %>% 
  mutate(net.tally = ifelse(is.na(net.tally), 0, net.tally),
         mismatch.note = ifelse(is.na(mismatch.note), count.v.tally.note, mismatch.note),
         db.v.tally = count - net.tally,
         final4.v.tally = final4 - net.tally,
         final4.v.db = final4 - count) %>% 
  rename(value.in.db = count)  %>% 
  select(date, transect, section, alpha.code, value.in.db, pos.tally, neg.tally, net.tally,
         added.back, final4, original4, contains(".v."), mismatch.note,
         -block, -count.v.tally, -count.v.tally.note, -mismatch, -mismatch.v.addedback, -NA.)%>% 
  arrange(date, alpha.code, transect, section) %>% 
  write.csv(here("data_files/tracking_finding_problem_records/all_mismatch.csv"), row.names = FALSE)

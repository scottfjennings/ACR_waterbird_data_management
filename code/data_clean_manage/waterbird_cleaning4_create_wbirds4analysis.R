
# this is the forth of 4 steps to prep waterbird data for use.

# this step combines cleaned data (grouped birds split and negatives reconciled) with the few remaining records that could not be split

# note that waterbird_cleaning1 is sourced below to read the data from access and do basic house-keeping on variable names



library(lubridate)

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("code/utility/waterbird_utility_functions.r")

# grouped observations that couldn't be allocated
grouped_unsplit4adding <- readRDS("data_files/working_rds/groupies_allocated") %>% 
  ungroup() %>% 
  filter(allocation.scale == "no.allocation") %>% 
  select(date, section, alpha.code = group.alpha.code, section.final = group.sec.final) %>% 
  distinct() %>% 
  filter(section.final > 0) %>% 
  mutate(data.is = "failed.allocation")

# read the birds IDed to groups that we don't allocate to species  
no_try_split <- readRDS("data_files/working_rds/no_try_split") %>% 
  select(date, section, alpha.code = alpha.code, section.final = sec.tally) %>% 
  mutate(data.is = "no.try.allocation")


wbirds4analysis_pre <- readRDS("data_files/working_rds/negatives_carried") %>% 
  select(date, alpha.code, contains("final")) %>% 
  pivot_longer(cols = contains("final"), names_to = "section", values_to = "section.final") %>% 
  mutate(section = gsub("sec.final_", "", section),
         data.is = "cleaned") %>% 
  rbind(., grouped_unsplit4adding, no_try_split) %>% 
  arrange(date, alpha.code, section) 

# double check number of records for each data X species X section; there should be no more than 1
wbird_duplicates <- check_wbird_duplicates(wbirds4analysis_pre) # from waterbird_utility_functions
nrow(wbird_duplicates)

wbird_duplicates2drop <- filter(wbird_duplicates, data.is == "cleaned")

wbirds4analysis <- wbirds4analysis_pre %>% 
  anti_join(., wbird_duplicates2drop)

nrow(wbirds4analysis_pre) - nrow(wbirds4analysis)
check_wbird_duplicates(wbirds4analysis) %>% View()

saveRDS(wbirds4analysis, "data_files/working_rds/wbirds4analysis")

# and a grand summary of allocation and negative reconciliation
tot_wbirds <- wbirds4analysis %>% 
  summarise(tot.wbirds = sum(section.final))

all_no_try_split <- no_try_split %>% 
  summarise(no.try.allocate = sum(section.final)) %>% 
          pivot_longer(cols = contains("no"), names_to = "data.type", values_to = "total.count") 
  


unsplit_neg_lost <- readRDS("data_files/working_rds/groupies_allocated") %>% 
  ungroup() %>% 
  filter(allocation.scale == "no.allocation", group.sec.tally < 0) %>% 
  select(date, section, alpha.code = group.alpha.code, group.sec.tally) %>% 
  distinct() %>% 
  summarise(unallocated.negatives.lost = sum(group.sec.tally)) %>% 
          pivot_longer(cols = contains("lost"), names_to = "data.type", values_to = "total.count")

negatives_added <- readRDS("data_files/working_rds/negatives_carried") %>% 
          summarise(tot.neg.added = sum(birds2add)) %>% 
          pivot_longer(cols = contains("tot."), names_to = "data.type", values_to = "total.count")

wbird_cleaning_summary <- readRDS("data_files/working_rds/un_allocated_summary") %>% 
  summarise(failed.allocation = sum(no.allocation), allocated.bay.ration = sum(bay), allocated.section.ratio = sum(section), total.id.to.group = sum(total.grouped)) %>% 
  pivot_longer(cols = contains("t"), names_to = "data.type", values_to = "total.count") %>%
  rbind(., negatives_added, unsplit_neg_lost, all_no_try_split) %>% 
  mutate(total.wbirds = tot_wbirds$tot.wbirds,
         percent.total = 100 * (total.count/total.wbirds),
         percent.total = round(percent.total, 2))
  




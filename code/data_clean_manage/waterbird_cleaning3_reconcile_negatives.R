

# this is the third of 4 steps to prep waterbird data for use.

# this file carries negative tallies forward through sections, and if there are net negatives left over after section 4, these are added back in to the section 4 count.

# note that waterbird_cleaning1 is sourced below to read the data from access and do basic house-keeping on variable names

# section.num = row 11
# count = yellow boxes
# final.data.record = pink boxes, for section 4 = column I
# section.sum = row 24 under yellow boxes
# section.sum.final = row 24 under pink boxes
# carried.forward = row 25


# source, loads packages, data ----

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/water_birds/code/waterbirds_cleaning1_variable_names.R") # this loads data from access, does basic house keeping on variable names

# read object with grouped counts split by constituent species ratios

wbirds_sppid_allocated <- readRDS("data_files/working_rds/wbirds_sppid_allocated")

no_try_split <- wbirds %>% 
  select(date, block, section, alpha.code, count)


# define functions ----

# reproducing row 24 of the xlsx spreadsheet
# calculate total positive birds ("sec.final") and total positive minus negative ("sec.tally)



widen_section_tallies <- function(zsection_tallies) {
section_tallies_longer <- zsection_tallies %>% 
  pivot_longer(cols = c("sec.tally", "sec.final"), names_to = "sum.type", values_to = "sum") %>% 
  mutate(sec.name = paste(sum.type, section, sep = "_")) 


section_tallies_wider <- section_tallies_longer %>% 
  pivot_wider(id_cols = c(date, alpha.code), names_from = sec.name, values_from = sum) %>% 
  replace(is.na(.), 0) %>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

return(section_tallies_wider)
}



calc_carried_forward <- function(zwide_tallies) {
  
tallies_carried_forward <- zwide_tallies %>% 
  mutate(sec.forward_1 = ifelse(sec.tally_1 < 0, sec.tally_1, 0),
         sec.forward_2 = sec.forward_1 + sec.tally_2,
         sec.forward_3 = sec.forward_2 + sec.tally_3,
         sec.forward_4 = sec.forward_3 + sec.tally_4)%>% 
  select(date, alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) 

}


calc_bay_sum <- function(zcarried_forward) {
  
  zcarried_forward <- zcarried_forward %>% 
    rename(sec.prelim_4 = sec.final_4) %>% 
    mutate(birds2add = ifelse(sec.forward_4 < 0, abs(sec.forward_4), 0),
           sec.final_4 = sec.prelim_4 + birds2add,
           bay.sum = sec.final_1 + sec.final_2 + sec.final_3 + sec.final_4)
  
}


# pipe functions ----




negatives_carried <- readRDS("data_files/working_rds/wbirds_sppid_allocated") %>% 
  widen_section_tallies() %>% 
  calc_carried_forward() %>% 
  calc_bay_sum()


saveRDS(negatives_carried, "data_files/working_rds/negatives_carried")

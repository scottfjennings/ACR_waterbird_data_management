
library(tidyverse)
library(RODBC)
library(lubridate)

useless_groupies <- c("AMCOGRSCLESCBUFF", "COMERBME", "GOOSE", "MERG", "MURRELET", "SWAN", "UNTE", "DUCK")

useful_groupies <- c("LOON", "RTPALO", "CORM", "HEGR", "WCGR", "PCLO")

# basic data reading and prep ----
# read directly from access
wbird_qsel_all_data <- function(){

 db <- "C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/waterbirds_v2.0.accdb"
 con2 <- odbcConnectAccess2007(db)

# get sql code for a query that already exists in access: https://www.techonthenet.com/access/queries/view_sql2007.php
 # right click query name, "design view"
 # Design tab in the toolbar at the top of the screen. Then click on the View button in the Results group. Select SQL View from the popup menu.
 #qsel_all_data <- "SELECT tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_survey.season, tbl_WATERBIRDS_survey.surveynotes, tbl_WATERBIRDS_transect.transect_lookup, tbl_WATERBIRDS_block.surveyblock_lookup, tbl_WATERBIRDS_block.surveytype, tbl_WATERBIRDS_block.BEAUFORT, tbl_WATERBIRDS_block.numberobservers, tbl_WATERBIRDS_block.blocknotes, tbl_WATERBIRDS_block.enteredby, tbl_WATERBIRDS_block.PROOF_LEVELnum, tbl_WATERBIRDS_observation.species_lookup, tbl_WATERBIRDS_observation.Count
#FROM (tbl_WATERBIRDS_survey INNER JOIN tbl_WATERBIRDS_transect ON tbl_WATERBIRDS_survey.SURVEY_ID = tbl_WATERBIRDS_transect.[SURVEY_ID_LOOKUP]) INNER JOIN (tbl_WATERBIRDS_block INNER JOIN tbl_WATERBIRDS_observation ON tbl_WATERBIRDS_block.BLOCK_ID = tbl_WATERBIRDS_observation.[BLOCK_ID_LOOKUP]) ON tbl_WATERBIRDS_transect.TRANSECT_ID = tbl_WATERBIRDS_block.[TRANSECT_ID_LOOKUP]
#ORDER BY tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_transect.transect_lookup, tbl_WATERBIRDS_block.surveyblock_lookup, tbl_WATERBIRDS_observation.species_lookup;"
# qsel_all_data <- "SELECT tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_survey.season AS Season, tbl_WATERBIRDS_survey.surveynotes AS Survey_Notes, tbl_WATERBIRDS_transect.transect_lookup AS Transect, tbl_WATERBIRDS_block.surveyblock_lookup AS Block, tbl_WATERBIRDS_block.surveytype AS [Survey_type], tbl_WATERBIRDS_block.BEAUFORT AS Beaufort, tbl_WATERBIRDS_block.numberobservers AS Num_Observers, tbl_WATERBIRDS_block.AREA_NOT_SURVEYED AS Area_Not_Surveyed, tbl_WATERBIRDS_block.NO_BIRDS_OBS AS No_Birds_Observed, tbl_WATERBIRDS_block.blocknotes AS Block_Notes, tbl_WATERBIRDS_block.enteredby AS Entered_By, tbl_WATERBIRDS_block.PROOF_LEVELnum AS Proof_Level, tbl_WATERBIRDS_observation.species_lookup AS Species, tbl_WATERBIRDS_observation.Count
#FROM (tbl_WATERBIRDS_survey LEFT JOIN tbl_WATERBIRDS_transect ON tbl_WATERBIRDS_survey.SURVEY_ID = tbl_WATERBIRDS_transect.[SURVEY_ID_LOOKUP]) LEFT JOIN (tbl_WATERBIRDS_block LEFT JOIN tbl_WATERBIRDS_observation ON tbl_WATERBIRDS_block.BLOCK_ID = tbl_WATERBIRDS_observation.[BLOCK_ID_LOOKUP]) ON tbl_WATERBIRDS_transect.TRANSECT_ID = tbl_WATERBIRDS_block.[TRANSECT_ID_LOOKUP]
#ORDER BY tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_transect.transect_lookup, tbl_WATERBIRDS_block.surveyblock_lookup, tbl_WATERBIRDS_observation.species_lookup;"
 qsel_all_data <- "SELECT tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_survey.season AS Season, tbl_WATERBIRDS_survey.surveynotes AS Survey_Notes, tbl_WATERBIRDS_transect.transect_lookup AS Transect, tbl_WATERBIRDS_block.surveyblock_lookup AS Block, tbl_WATERBIRDS_block.surveytype AS [Survey_Type], tbl_WATERBIRDS_block.BEAUFORT AS Beaufort, tbl_WATERBIRDS_block.numberobservers AS Num_Observers, tbl_WATERBIRDS_block.AREA_NOT_SURVEYED AS Area_Not_Surveyed, tbl_WATERBIRDS_block.NO_BIRDS_OBS AS No_Birds_Observed, tbl_WATERBIRDS_block.blocknotes AS Block_Notes, tbl_WATERBIRDS_block.enteredby AS Entered_By, tbl_WATERBIRDS_block.PROOF_LEVELnum AS Proof_Level, tbl_WATERBIRDS_observation.species_lookup AS Species, tbl_WATERBIRDS_observation.Count
FROM (tbl_WATERBIRDS_survey LEFT JOIN tbl_WATERBIRDS_transect ON tbl_WATERBIRDS_survey.SURVEY_ID = tbl_WATERBIRDS_transect.[SURVEY_ID_LOOKUP]) LEFT JOIN (tbl_WATERBIRDS_block LEFT JOIN tbl_WATERBIRDS_observation ON tbl_WATERBIRDS_block.BLOCK_ID = tbl_WATERBIRDS_observation.[BLOCK_ID_LOOKUP]) ON tbl_WATERBIRDS_transect.TRANSECT_ID = tbl_WATERBIRDS_block.[TRANSECT_ID_LOOKUP]
ORDER BY tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_transect.transect_lookup, tbl_WATERBIRDS_block.surveyblock_lookup, tbl_WATERBIRDS_observation.species_lookup;"


all_data <- sqlQuery(con2, qsel_all_data, as.is = TRUE, stringsAsFactors = FALSE)
close(con2)
return(all_data)
}

#wbirds <- wbird_qsel_all_data() 
wbird_read_species_table <- function() {
db <- "C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/waterbirds_v2.0.accdb"
con2 <- odbcConnectAccess2007(db)

species <- sqlFetch(con2, "tbl_species_picklist") 

close(con2)
return(species)
}

# read old data stru from access
wbird_read_old_stru <- function() {
  
   db <- "C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/old_stru/WATERBIRDS_pre_schema_change.mdb"
 con2 <- odbcConnectAccess2007(db)
old_wbirds <-  sqlFetch(con2, "WATERBIRD")
  close(con2)
  return(old_wbirds)
}


clean_waterbirds <- function(df) { 
df1 <- df %>%
  rename_all(tolower) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         species = toupper(species),
         count = as.numeric(count)) %>% 
  mutate_if(is.factor, str_trim) %>% 
  mutate_if(is.character, str_trim) 

return(df1)

}

wbird_fix_spp_factors <- function(df) {
  
  spp <- wbird_read_species_table() %>% 
    select(speciescode, species = index) %>% 
    mutate_all(as.character)
  df <- df %>% 
    left_join(., spp) %>% 
    mutate(species = ifelse(species %in% spp$speciescode, species, speciescode)) %>% 
    select(-speciescode)
}

wbird_fix_precount_block_names <- function(df) {
  df <- df %>% 
    mutate(block = gsub("east.cypress_grove", "cypressgrove.sec2", block),
           block = gsub("east.millerton_bivalve", "millertonbivalve.sec1", block),
           block = gsub("east.bivalve", "bivalve.sec1", block),
           block = gsub("east.walker_creek", "walkercreek.sec3", block),
           block = gsub("west.inverness", "inverness.sec1", block))
  
}


######################
# functions below here are to be used on wbirds4analysis, from code/data_clean_manage/waterbird_cleaning4_create_wbirds4analysis.R

# starting point for any waterbird data work should be these 3 functions:
#wbirds <- wbird_qsel_all_data() %>% 
#  clean_waterbirds() %>% 
#  wbird_fix_spp_factors() %>% 
#  wbird_fix_precount_block_names()

# wbirds4analysis <- readRDS("data_files/working_rds/wbirds4analysis")

# add study day as number of days since Nov 1 ----
wbird_add_study_day <- function(df) {
  
  nov_1 <- data.frame(nov1 = as.Date(paste(seq(1989, 2020), "-11-01", sep = "")),
                      dec31 = as.Date(paste(seq(1989, 2020), "-12-31", sep = ""))) %>% 
  mutate(nov1.yday = yday(nov1),
         dec31.yday = yday(dec31),
         year = year(nov1))
  
  df <- df %>% 
    mutate(year = year(date),
           year.day = yday(date),
           study.year = ifelse(month(date) < 6, year(date) - 1, year(date))) %>% 
    left_join(., dplyr::select(nov_1, -nov1, -dec31), by = c("year")) %>% 
    mutate(study.day = ifelse(year.day > nov1.yday, year.day - nov1.yday, year.day + (dec31.yday - nov1.yday))) %>% 
    dplyr::select(-nov1.yday, -dec31.yday, -year)
  
}


wbird_add_season <- function(wbird_df) {
  wbird_df <- wbird_df %>% 
    mutate(season = ifelse(month(date) < 6, year(date) - 1, year(date)))
}


wbird_combine_scaup <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>% 
    filter(alpha.code %in% c("GRSC", "LESC", "SCAUP")) %>% 
    group_by(season, date, survey.num, section) %>% 
    summarise(section.final = sum(section.final)) %>% 
    mutate(alpha.code = "SCAUP",
           data.is = "lumped.scaup") %>% 
    ungroup() %>% 
    rbind(., filter(zwbirds4analysis, !alpha.code %in% c("GRSC", "LESC", "SCAUP"))) %>% 
    arrange(date, section, alpha.code)
}


wbird_combine_wegr_clgr <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>% 
    filter(alpha.code %in% c("WEGR", "CLGR", "WCGR")) %>% 
    group_by(season, date, survey.num, section) %>% 
    summarise(section.final = sum(section.final)) %>% 
    mutate(alpha.code = "WCGR",
           data.is = "lumped.wcgr") %>% 
    ungroup() %>% 
    rbind(., filter(zwbirds4analysis, !alpha.code %in% c("WEGR", "CLGR", "WCGR"))) %>% 
    arrange(date, section, alpha.code)
}



# check fro duplicate date X section X alpha.code records; there should be no more than one of each of these

check_wbird_duplicates <- function(zwbirds4analysis) {
  dup_recs <- zwbirds4analysis %>% 
    group_by(date, alpha.code, section) %>% 
    mutate(num.rec = n()) %>% 
    filter(num.rec > 1)
  
}
  
  
  


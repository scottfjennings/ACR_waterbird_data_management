


#' Query Access database to load data into R
#'
#' @param db file path for the location of the access database
#'
#' @return data frame
#' @export
#'
#'
#' @details
#' To get sql code for a query that already exists in access: (following https://www.techonthenet.com/access/queries/view_sql2007.php). 
#' Right click query name, "design view" in the design tab in the toolbar at the top of the screen. 
#' Then click on the View button in the Results group. Select SQL View from the popup menu.
#' The resulting query text was saved as qsel_all_data in this function 
#' 
#' @examples
#' # no run
#' # wbirds <- query_waterbirds(here("data_files/waterbirds_v2.0.accdb"))
#' # wbirds <- query_waterbirds("V:/Waterbirds_data/WATERBIRDS_new_dbase_structure/waterbirds_v2.0.accdb") # Azure path via remote desktop
query_waterbirds <- function(db){

 con2 <- odbcConnectAccess2007(db)


 qsel_all_data <- "SELECT tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_survey.season AS Season, tbl_WATERBIRDS_survey.surveynotes AS Survey_Notes, tbl_WATERBIRDS_transect.transect_lookup AS Transect, tbl_WATERBIRDS_block.surveyblock_lookup AS Block, tbl_WATERBIRDS_block.surveytype AS [Survey_Type], tbl_WATERBIRDS_block.BEAUFORT AS Beaufort, tbl_WATERBIRDS_block.numberobservers AS Num_Observers, tbl_WATERBIRDS_block.AREA_NOT_SURVEYED AS Area_Not_Surveyed, tbl_WATERBIRDS_block.NO_BIRDS_OBS AS No_Birds_Observed, tbl_WATERBIRDS_block.blocknotes AS Block_Notes, tbl_WATERBIRDS_block.enteredby AS Entered_By, tbl_WATERBIRDS_block.PROOF_LEVELnum AS Proof_Level, tbl_WATERBIRDS_observation.species_lookup AS Species, tbl_WATERBIRDS_observation.Count
FROM (tbl_WATERBIRDS_survey LEFT JOIN tbl_WATERBIRDS_transect ON tbl_WATERBIRDS_survey.SURVEY_ID = tbl_WATERBIRDS_transect.[SURVEY_ID_LOOKUP]) LEFT JOIN (tbl_WATERBIRDS_block LEFT JOIN tbl_WATERBIRDS_observation ON tbl_WATERBIRDS_block.BLOCK_ID = tbl_WATERBIRDS_observation.[BLOCK_ID_LOOKUP]) ON tbl_WATERBIRDS_transect.TRANSECT_ID = tbl_WATERBIRDS_block.[TRANSECT_ID_LOOKUP]
ORDER BY tbl_WATERBIRDS_survey.Year, tbl_WATERBIRDS_survey.Month, tbl_WATERBIRDS_survey.Day, tbl_WATERBIRDS_transect.transect_lookup, tbl_WATERBIRDS_block.surveyblock_lookup, tbl_WATERBIRDS_observation.species_lookup;"


all_data <- sqlQuery(con2, qsel_all_data, as.is = TRUE, stringsAsFactors = FALSE)
close(con2)
return(all_data)
}

#' Read species table
#'
#' Reading in species table allows linking of database species index numbers with the appropriate alpha code 
#'
#' @param db file path for the location of the access database
#' 
#' @return data frame
#' @export
#' 
#' @seealso [wbird_sppindex_to_alpha()] 
#'
#' @examples
#' wbird_read_species_table(db)
read_species_table <- function(db) {
con2 <- odbcConnectAccess2007(db)

species <- sqlFetch(con2, "tbl_species_picklist") 

close(con2)
return(species)
}


#' Read all waterbird data
#'
#' Reading in all 4 data tables from access and join. This duplicates the query used in query_waterbirds(). This function probably not needed.
#'
#' @param db file path for the location of the access database
#' 
#' @return data frame
#' @export
#' 
#' @seealso [wbird_sppindex_to_alpha()] 
#'
#' @examples
#' read_wbird_all(db)
read_wbird_table <- function(db, ztable) {
con2 <- odbcConnectAccess2007(db)

obs <- sqlFetch(con2, "tbl_WATERBIRDS_observation") %>% 
  select(-Field1)
block <- sqlFetch(con2, "tbl_WATERBIRDS_block")
transect <- sqlFetch(con2, "tbl_WATERBIRDS_transect") 
survey <- sqlFetch(con2, "tbl_WATERBIRDS_survey") 
species <- sqlFetch(con2, "tbl_species_picklist") 

close(con2)

wbird_table <- obs %>% 
  rename(BLOCK_ID = BLOCK_ID_LOOKUP) %>% 
  full_join(block, by = c("BLOCK_ID")) %>% 
  rename(TRANSECT_ID = TRANSECT_ID_LOOKUP) %>% 
  full_join(transect, by = c("TRANSECT_ID")) %>% 
  rename(SURVEY_ID = SURVEY_ID_LOOKUP) %>% 
  full_join(survey, by = c("SURVEY_ID")) %>% 
  full_join(species %>% rename(species_lookup = index) %>% mutate(species_lookup = as.character(species_lookup))) %>% 
  mutate(alpha.code = ifelse(is.na(speciescode), species_lookup, speciescode))

return(species)
}


#' Read old waterbird structure
#'
#' Read data with old structure from original access database.
#'
#' @return db file path for the location of the old version access database
#' @export
#'
#' @examples
#' # NO RUN
#' # wbird_read_old_stru()
read_old_stru <- function(db) {
  
 con2 <- odbcConnectAccess2007(db)
old_wbirds <-  sqlFetch(con2, "WATERBIRD")
  close(con2)
  return(old_wbirds)
}


#' Clean waterbirds database
#' 
#' Basic field name and format cleaning
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
#' df <- clean_waterbirds(df)
clean_waterbirds <- function(df) { 
df1 <- df %>%
  rename_all(tolower) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         species = toupper(species),
         count = as.numeric(count)) %>% 
  mutate_if(is.factor, str_trim) %>% 
  mutate_if(is.character, str_trim) %>% 
  rename(alpha.code = species)

return(df1)

}

#' Replace species index number (from access db) with alpha code
#' 
#' Some records have the species index number entered in species instead of alpha code. this replaces those index nums  
#' 
#' @param df data frame
#'
#' @return data frame
#' @export
#'
#' @examples
#' df <- sppindex_to_alpha(df, spp_table)
sppindex_to_alpha <- function(df, spp_table) {

  spp <- spp_table %>% 
    mutate(alpha.code = index) %>% 
    select(speciescode, alpha.code) %>% 
    mutate_all(as.character)
  df <- df %>% 
    left_join(., spp) %>% 
    mutate(alpha.code = ifelse(grepl("^[[:digit:]]", alpha.code), speciescode, alpha.code)) %>% 
    select(-speciescode)
}

#' Make precount block names match format of main count blocks
#'
#' @param df data frame with at least a field named "block" 
#'
#' @return data frame with all fields in df
#' @export
#'
#' @examples
#' wbird_fix_precount_block_names()
fix_precount_block_names <- function(df) {
  df <- df %>% 
    mutate(block = gsub("east.cypress_grove", "cypressgrove.sec2", block),
           block = gsub("east.millerton_bivalve", "millertonbivalve.sec1", block),
           block = gsub("east.bivalve", "bivalve.sec1", block),
           block = gsub("east.walker_creek", "walkercreek.sec3", block),
           block = gsub("west.inverness", "inverness.sec1", block))
  
}





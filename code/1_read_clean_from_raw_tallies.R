
# FUnctions to read data from multiple raw tally .xlsx files. These files have data entered as each individual tally of number of birds seen (e.g. each value recored on the data sheets), eliminating the need for humans to add these values together by hand.

# extract date from file name, requires stripping parts of file name that indicate proof status and extension
#' get_survey_date_file
#'
#' @param ztally_file full file path for raw tally data file
#'
#' @return
#' @export
#'
#' @examples
get_survey_date_file <- function(ztally_file) {
  
  if(!exists("raw_tally_location", where = .GlobalEnv)) {
    stop("Please specify the location where raw tally location data are stored, e.g. V:/Waterbirds_data/waterbirds_raw_data_entry/")
  }
survey_date_file <- gsub(paste(raw_tally_location, "entered_raw_data/|\\_p.xlsx|\\_p2.xlsx", sep = "|"), "", ztally_file) %>% 
  ymd()
}


#
# read_raw_tallies ----

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
read_raw_tallies <- function(ztally_file) {

  survey_date_file <- get_survey_date_file(ztally_file)
  
  
raw_tallies <- read_excel(ztally_file, sheet = "data", skip = 1, .name_repair = "universal") %>% 
  rename(tally.index = 1) %>% 
  filter(tally.index != "index")

# fix names coming from .xlsx to ones R can work with easier
znames <- names(raw_tallies) %>% 
  data.frame() %>% 
  rename(zname = 1) %>% 
  mutate(zname2 = case_when(grepl("p\\.|proof|\\.\\.\\.", zname) ~ paste(lag(zname), "tally", sep = "_"),
                            !grepl("p\\.", zname) & zname != "tally.index" ~ paste(zname, "species", sep = "_"),
                            TRUE ~ as.character(zname))) %>% 
  mutate(zname2 = gsub("bay.east", "bay.e", zname2),
         zname2 = gsub("bay.west", "bay.w", zname2),
         zname2 = gsub("ton.bivalve", "ton.biv", zname2),
         zname2 = gsub("\\.\\.", "\\.", zname2))

# and replace in the current data
names(raw_tallies) <- znames$zname2

# need a list of block names present in the current data
blocks <- znames %>% 
  filter(!grepl("p\\.|proof|\\.\\.\\.", zname) & zname != "tally.index") %>% 
  select(zname2) %>% 
  mutate(zname2 = gsub("_species", "", zname2))

# this sub-function maps across each block in `blocks` and extracts the species and tally column.
make_tallies_longer <- function(zblock) {
zz <- raw_tallies[grepl(zblock, names(raw_tallies))] %>% 
  mutate(block = zblock) %>% 
  rename("species" = 1,
         "tally" =  2) %>% 
   filter(!is.na(tally))
}

# converting to long format with this map_df call on each block name works better than a pivot_longer call
long_tallies <- map_df(blocks$zname2, make_tallies_longer) %>% 
  mutate(date = survey_date_file,
         species = toupper(species),
         tally = as.numeric(tally))

# 1995-12-16 has funky data with middle west 1 and 2 recorded on the same sheet and no clear way to separate. this results in some extra columns in that xlsx file, this reduces to just the columns we want
long_tallies <- long_tallies %>% 
  select(date, block, species, tally)

return(long_tallies)
}



# some helper data checks ----
# NO RUN check_survey_dates ----
#' Check survey dates
#' 
#' Check to ensure that the date used for the file name matches the date entered in the data sheet.
#'
#' @param ztally_file path to a tally data sheet xlsx file
#'
#' @return
#' @export
#' 
#' @details runs pretty slow so shouldn't be needed for regular work flow.
#'
#' @examples
#' date_check <- map_df(tally_files, check_survey_dates)
#' 
#' date_check2 <- date_check %>%
#' full_join(read_excel("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/waterbird_raw_entry_tracking.xlsx", sheet = "sheets with data entered") %>% mutate(date_entered = ymd(date))) %>%
#' arrange(date_entered)
check_survey_dates <- function(ztally_file) {

  survey_date_file <- get_survey_date_file(ztally_file)
  
survey_date_check <- read_excel(ztally_file, sheet = "data", n_max = 1, col_names = FALSE) %>% 
  rename("date_entered" = 1) %>% 
  select(date_entered) %>% 
  mutate(date_entered = ymd(date_entered),
         date_file = survey_date_file,
         date_check = date_entered == date_file)
}





# check for any rows with date filled but date_entered, date_file == NA


# NO RUN check_header_names ----
#' Check header names
#' 
#' Check to ensure that no values in the 2 header rows got changed.
#'
#' @param ztally_file path to a tally data sheet xlsx file
#'
#' @return
#' @export
#' 
#' @details runs pretty slow so shouldn't be needed for regular work flow.
#'
#' @examples
#' header_check <- map_df(tally_files, check_header_names)
check_header_names <- function(ztally_file) {

expected_headers <- c("east", "proofed", "p", "west", "mid-bay east", "mid-bay west", "mid-bay port", "mid-bay starboard", "millerton.bivalve", "inverness", "cypress.grove", "walker.creek", "bivalve", "species", "tally", "index")
  
raw_tallies <- read_excel(ztally_file, sheet = "data", col_names = FALSE, n_max = 3, .name_repair = "universal")
date <- raw_tallies[1,1] %>% 
  rename("date" = 1)
block <- raw_tallies[2:3,] %>% 
  pivot_longer(everything(), values_to = "header") %>% 
  mutate(header = gsub("[[:digit:]]+", "", header),
         header = trimws(header)) %>% 
  distinct(header) %>% 
  mutate(date = date$date) %>% 
  filter(!is.na(header), !header %in% expected_headers)

}







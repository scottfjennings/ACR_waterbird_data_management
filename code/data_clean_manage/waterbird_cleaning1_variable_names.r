

# first of 4 steps to prep waterbird data for use.
# the files waterbird_cleaning1... thru 4... should be run in order to yield a cleaned data file that has lumped birds split into constituent species where this can be done reasonably, and has any negative tallies reconciled 

# this file reads waterbird data from access and does basic house-keeping on variable names

# !! but note, this file is sourced by waterbird_cleaning2 and waterbird_cleaning3, so it does not need to be manually run
# ---
# these code files use functions which are defined in waterbird_utility_funtions.R and bird_utility_functions.r
# ---
# as of 2019 data are in a new relational Access db
# query from db now yields data in longer format with species and count fields, rather than wide with a field for each species






# 1 packages, settings -------------
library(tidyverse)
library(lubridate)
options("scipen"=999)
source("code/utility/waterbird_utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.r")

wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")
#wbird_keep_taxa_gulls <- c("Anseriformes", "Alcidae", "Laridae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Suliformes")


wbirds <- wbird_qsel_all_data() %>% 
  clean_waterbirds() %>% 
  wbird_fix_spp_factors() %>% 
  fix_4letter_codes() %>% # from bird_utility_functions.r
  wbird_fix_precount_block_names() %>% 
  bird_taxa_filter(join_taxa = c("alpha.code", "species"), # from bird_utility_functions.r
                          keep_taxa = wbird_keep_taxa) %>% 
  separate(block, into = c("transect", "section"), remove = F) %>% 
  mutate(section = gsub("sec", "", section))





   




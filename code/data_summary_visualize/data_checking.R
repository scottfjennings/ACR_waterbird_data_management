



library(tidyverse)
library(RODBC)

source("C:/Users/scott.jennings/Documents/Projects/water_birds/code/waterbird_utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/water_birds/code/waterbirds_start.r")


# uses wbirds from waterbirds_start.r, which should be created by second source call above

exclude_dates <- c("1999-01-30")

# checking species detected

spp_detect <- wbirds %>% 
  distinct(alpha.code, common.name, species)



# anything interesting in the notes fields? ----
# block level notes
block_notes <- wbirds %>% 
  select(date, block, block_notes) %>% 
  distinct() %>% 
  filter(!is.na(block_notes)) %>% 
  mutate(block_notes = tolower(block_notes))

write.csv(block_notes, "temp/block_notes.csv")

block_notes_terms <- c("not", "hunt", "keep data", "jk", "review", "proportion", "poor", "coverage", "verified", "hard", "missing", "data", "fog", "incomplete", "count", "duck", "waterfowl", "flew", "goose", "added", "shot")

block_terms_coll <- paste(block_notes_terms, collapse = " | ")

important_block_notes <- block_notes %>% 
  filter(grepl(block_terms_coll, block_notes)) %>%  
  mutate(block.w.notes = paste(block, block_notes, sep = ": "))


# survey level notes
survey_notes <- wbirds %>% 
  select(date, survey_notes) %>% 
  distinct() %>% 
  filter(!is.na(survey_notes)) %>% 
  mutate(survey_notes = tolower(survey_notes),
         survey_notes = paste("all survey: ", survey_notes))


# combine survey and block notes
important_block_notes_date <- important_block_notes %>% 
  select(date, notes = block.w.notes) %>% 
  rbind(., select(survey_notes, date, notes = survey_notes)) %>% 
  group_by(date) %>% 
  summarize(all.date.notes = paste(notes, collapse = " | "))

# get all block notes that have numbers

num_notes <- wbirds %>% 
  filter(grepl("\\d", blocknotes)) %>% 
  distinct(date, surveyblock, blocknotes) %>% 
  filter(!is.na(blocknotes))
# mostly gulls, but a few waterbirds
# can I extract spp and numbers?

num_notes_extract <- num_notes %>% 
  separate(col = blocknotes, remove = F, 
           into = paste("splitfield", seq(1:10), sep = "_"), sep = ",|\\.")

num_notes_extract_longer <- num_notes_extract %>% 
  pivot_longer(cols = contains("splitfield")) %>% 
  select(date, surveyblock, blocknotes, extractnotes = value) %>% 
  filter(!is.na(extractnotes)) %>% 
  filter(extractnotes != "")

# is there a hunter effect on bird numbers? ----
# identify survey X block instances where hunters were present
hunt_blocks <- block_notes %>% 
  filter(grepl("hunt", block_notes)) %>% 
  mutate(hunters = T) %>% 
  select(-block_notes)

hunt_effect <- wbirds %>% 
  select(date, species, count, block) %>% 
  full_join(., hunt_blocks) %>% 
  filter(block %in% distinct(hunt_blocks, block)$block) %>% 
  mutate(hunters = ifelse(is.na(hunters), "hunters not present", "hunters present"),
         game.spp = ifelse(species %in% c("AMCO", "AMWI", "BAGO", "BRAN", "BUFF", "CACG", "CAGO", "CANV", "CITE", "COGO", "COMO", "DUCK", "EUWI", "GADW", "GRSC", "GWFG", "GWTE", "HOME", "LESC", "MALL", "NOPI", "NOSH", "REHE", "RNDU", "RUDU", "SCAUP", "WODU"), "game species", "non-game species"))

hunt_effect %>% 
  group_by(date, hunters, game.spp, block) %>% 
  summarise(sum.count = sum(count)) %>% 
  filter(!is.na(sum.count)) %>% 
  ungroup() %>% 
  group_by(hunters, game.spp) %>% 
  summarise(mean.count = mean(sum.count),
            sd.count = sd(sum.count),
            num.rec = n(),
            st.err = sd.count/sqrt(num.rec)) %>% 
  ggplot() +
  geom_point(aes(x = hunters, y = mean.count, color = game.spp)) +
  geom_errorbar(aes(x = hunters, ymin = mean.count - st.err, ymax = mean.count + st.err, color = game.spp), width = 0.2) +
  theme_classic() +
  ylab("Mean number of birds counted") +
  theme(legend.title = element_blank())
  
ggsave("figures_output/waterbirds_hunt_effect.png", width = 6, height = 6)
# checking if any surveyblocks were missed on any dates ----
# make table of dates and surveyblocks, 1 = data exist, 0 = no data
#dates_blocks_longer <- table(wbirds$date, wbirds$block) %>% 
#  data.frame() %>% 
#  mutate(Freq = ifelse(Freq > 0, 1, 0)) %>% 
#  rename(date = Var1, block = Var2, counted = Freq) %>% 
#  mutate(date = as.Date(date))

dates_blocks_longer <- wbirds %>% 
  distinct(date, block, area_not_surveyed, no_birds_observed) %>% 
  data.frame() %>% 
  mutate(surveyed = ifelse(area_not_surveyed == 0, "surveyed", "not surveyed"),
         surveyed = ifelse(no_birds_observed == 1, "surveyed, no birds obs", surveyed))


# make wide table of dates and surveyblocks
dates_blocks <- dates_blocks_longer %>% 
  pivot_wider(id_cols = date, names_from = block, values_from = surveyed) %>% 
  select(date, contains("east.sec"), contains("middle.sec"), contains("west.sec"), everything())  
  dates_blocks[is.na(dates_blocks)] <- "no record"

  
# compare to previous version of this table, before we added 
old_dates_blocks <- read.csv("data_files/data_summaries/missed_surveys_notes_20200702.csv") %>% 
  select(date, east.sec1, east.sec2, east.sec3, east.sec4, middle.sec1, middle.sec2, middle.sec3, middle.sec4, west.sec1, west.sec2, west.sec3, west.sec4, east.bivalve, east.cypress_grove, east.millerton_bivalve, east.walker_creek, west.inverness, num.blocks.counted, -all.date.notes) %>% 
  mutate(date = as.Date(as.character(date))) %>% 
  arrange(date)

comp_dates_blocks <- dates_blocks - old_dates_blocks %>% 
  data.frame()

# extract blocks that were missed
missed_surveys <- dates_blocks_longer %>% 
  filter(counted == 0) %>% 
  arrange(date)

# summarize by block
missed_surveys_by_block <- missed_surveys%>% 
  group_by(block) %>% 
  summarise(n())

# combine wide date X surveyblock with the filtered notes
missed_surveys_notes <- full_join(dates_blocks, important_block_notes_date, by = c("date")) %>% 
  select(date, everything())

write.csv(missed_surveys_notes, "figures_output/data_summaries/missed_surveys_notes_v2.csv", row.names = F)


# read block table from db to check for indication that a surveyblock was surveyed even if count data do not exist

# read directly from access, basic data cleaning
wbird_block_tbl <- function(){

 db <- "C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/waterbirds_v2.0.accdb"
 con2 <- odbcConnectAccess2007(db)

all_data <- sqlFetch(con2, "tbl_WATERBIRDS_block")
 
close(con2)
return(all_data)
}


# check for block mismatches between surveyblock_lookup and datetransectblockkey
blocktbl_field_mismatch <- wbird_block_tbl() %>% 
  mutate_if(is.factor, as.character)  %>% 
  mutate_if(is.character, str_trim) %>% 
  rowwise() %>%
  mutate(match = grepl(surveyblock_lookup, datetransectblockkey)) %>% 
  filter(match == F & !is.na(datetransectblockkey))
# none




wbird_block <- wbird_block_tbl() %>% 
  filter(!is.na(datetransectblockkey)) %>% 
  #select(datetransectblockkey) %>% 
  mutate(datetransectblockkey = as.character(datetransectblockkey))%>% 
  separate(datetransectblockkey, into = c("znum", "date", "block", "transect"), sep = "\\.") %>% 
  mutate(surveyblock = paste(block, transect, sep = "."),
         surveyblock2 = ifelse(grepl("sec", transect), paste(block, transect, sep = "."), transect),
         date = as.Date(date, format = "%m-%d-%Y"),
         block_tbl = 1) 


# combine records from qsel_all_data quesry and tbl_WATERBIRDS_block to see if there are any missmatches
block_obs_test <- wbird_block %>% 
  select(date, block = surveyblock, block_tbl) %>% 
  full_join(., dates_blocks_longer, by = c("date", "block")) %>% 
  rename(in.tbl_block = block_tbl, in.query = counted) %>% 
  arrange(date, block) %>% 
  mutate(in.tbl_block = ifelse(is.na(in.tbl_block), 0, in.tbl_block),
         in.query = ifelse(is.na(in.query), 0, in.query))


block_obs_test_bad <- block_obs_test %>% 
  filter(in.tbl_block != in.query)
# nope


# check count values for each species ----


spp_obs <- distinct(wbirds, species)


spp_summary <- wbirds %>% 
  group_by(species) %>% 
  summarize(max.count = max(count),
            min.count = min(count),
            mean.count = round(mean(count), 1),
            num.records = n())

# everything looks ok

# high level, total number of birds counted per year, by boat or land survey ----
total_count_year <- wbirds %>% 
  group_by(year(date), survey_type) %>% 
  summarise(total.sbirds = sum(count)) %>% 
  ungroup() %>% 
  rename(year = 1) %>% 
  pivot_wider(id_cols = year, names_from = survey_type, values_from = total.sbirds)


# proof level?? ----
# values = 2 and 3 are good.
table(wbirds$proof_level)
filter(wbirds,proof_level < 2)


# check out negative tallies ----

wbird_negs <- filter(wbirds, count < 0)

table(wbird_negs$alpha.code)

# which are lumped?
pool_negs <- wbird_negs %>% 
  check_pooled_spp() %>% 
  select(date, block, alpha.code, count, pooled.spp) %>% 
  filter(pooled.spp == 1)

table(pool_negs$alpha.code, pool_negs$block)

# check some days with negatives

filter(wbirds, date == "2000-01-08", alpha.code %in% c("GRSC", "LESC", "SCAUP")) %>% 
  select(date, alpha.code, count, block)


### checking data in old db structure

old_wbirds <- wbird_read_old_stru() %>% 
  filter(MONTH == 12, DAY == 15, YEAR == 2007) %>% 
  select(MONTH, DAY, YEAR, SECTION, BOAT, BRCO) %>% 
  mutate(spp.total = sum(BRCO))


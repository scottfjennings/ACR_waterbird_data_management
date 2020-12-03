## code to reshape original waterbird data table to multiple relational, long format tables

## throughout, field names in ALLCAPS are fields from the original waterbirds db, fields with lowercase names are newly-generated fields. Field name capitalization will be standardized for the final new db structure


library(tidyverse)
library(lubridate)
wbirds <- read.csv("WATERBIRD_20181011.csv")

wbirds.spp.list <- c("COLO", "PALO", "PACOLO", "RTLO", "RTPALO", "LOON", "HOGR", "EAGR", "EAHOGR", "WEGR", "CLGR", "WECLGR", "RNGR", "PBGR", "AWPE", "BRPE", "DCCO", "BRCO", "PECO", "CORM", "GBHE", "GREG", "SNEG", "GRHE", "BCNH", "CAGO", "ROGO", "BRAN", "MALL", "GADW", "NOPI", "GWTE", "CITE", "AMWI", "EUWI", "NOSH", "REHE", "CANV", "RNDU", "GRSC", "LESC", "SCAUP", "COGO", "BAGO", "BUFF", "HADU", "WWSC", "SUSC", "SCOTER", "BLSC", "OLDS", "RUDU", "HOME", "RBME", "COME", "OSPR", "PEFA", "NOHA", "COMO", "AMCO", "REPH", "BOGU", "FOTE", "COMU", "RHAU", "PIGU", "CAAU", "BEKI", "KAYAK", "WODU", "KIEI", "GWFG", "AMCOGRSCLESCBUFF", "COMERBME", "MUSW")


##-------------------------------------------------------
survey_blocker <- function(){
## create a supporting data frame that helps with changing BOAT to transect and names the blocks (which is just a concatenation of transect and section name)
## wrapping this in a simple function to reduce workspace clutter
zboats <- data.frame(BOAT = seq(1:3),
                     transect = c("east", "middle", "west"))


## SECTION has a different meaning for ground and boat based surveys.
## generating a new field "survey.block" to identify each of the 17 unique areas with distinct bird data
# build up the ground count survey.blocks
#sections 5-8 (Walker Creek, Cypress Grove, Millerton-to-Bivlave, Bivalve) would be “Boat 1” (east-shore boat)  
# Section 9 (Inverness) would be “Boat 3” (west-shore boat).
ground.sections <- data.frame(SECTION = seq(5, 9),
                              survey.block = c("walker_creek", "cypress_grove", "millerton_bivalve", "bivalve", "inverness"),
                              transect = c(rep("east", 4), "west"),
                              BOAT = c(rep(1, 4), 3)) %>% 
  mutate(survey.type = "land") %>% 
  select(transect, SECTION, BOAT, survey.block, survey.type)
# now the boat based survey.blocks
zboats.sections <- cbind(rbind(zboats, zboats, zboats, zboats),
                         SECTION = rep(1:4, each = 3)) %>% 
  mutate(survey.block = paste("sec", SECTION, sep = ""),
         survey.type = "boat") %>% 
  select(transect, BOAT, SECTION, survey.block, survey.type)

survey.blocks <- rbind(zboats.sections, ground.sections) %>% 
  mutate(survey.block = paste(transect, survey.block, sep = "."))
return(survey.blocks)
}

survey.blocks <- survey_blocker()
##-------------------------------------------------------

## build the keys to link the forthcoming 4 relational tables
wbirds.keyed <- wbirds %>% 
  full_join(., survey.blocks) %>% 
  mutate(date.key = mdy(paste(MONTH, DAY, YEAR, sep = "-"))) %>% # key linking survey and transect tables
  arrange(date.key, SECTION, BOAT) %>% 
  mutate(date.transect.block.key = paste(paste(MONTH, DAY, YEAR, sep = "-"), survey.block, sep = "."), # key linking block and observation tables
         date.transect.block.key = paste(seq(1:length(date.key)), date.transect.block.key, sep = "."), # making it start with sequential number for chronological sorting
         date.transect.key = paste(paste(MONTH, DAY, YEAR, sep = "-"), transect, sep = ".")) %>%  # key lining transect and block tables
  select(date.transect.block.key, date.transect.key, date.key, survey.block, survey.type, BOAT, SECTION, everything())

## fill 0's for 0 counts - requires wbirds.spp.list, generated above
wbirds.keyed[wbirds.spp.list][is.na(wbirds.keyed[wbirds.spp.list])] <- 0





##-------------------------------------------------------
## make the final surveys table
## survey.date.key field is a unique identifier for each survey (=day); goes sequentially from the very first survey

wbirds.survey <- wbirds.keyed %>% 
  select(date.key, MONTH, DAY, YEAR) %>% 
  distinct() %>% 
  mutate(season = ifelse(MONTH > 6, YEAR, YEAR - 1),
         survey.notes = as.character(""))

write.csv(wbirds.survey, "WATERBIRDS_survey.csv", row.names = F)

##-------------------------------------------------------
## make the final transect table (trading boat number to transect name for clarity). 
## add blank number of observers field
## date.transect.key is the unique identifier to specify the unique survey X transect combos
## if every transect was covered on every survey (true), then the length of wbirds.transect should be 3 times the length of wbirds.survey
wbirds.transect <- wbirds.keyed %>% 
  select(date.transect.key, transect, date.key) %>% 
  distinct()

write.csv(wbirds.transect, "WATERBIRDS_transect.csv", row.names = F)

##-------------------------------------------------------
## make final block (section) table
## if all blocks were surveyed on each survey date, then nrow(wbirds.block) = nrow(survey.blocks) * nrow(wbird.survey)
## however, not all blocks were surveyed on each survey date (see helper code below to ID which).
## the 
wbirds.block <- wbirds.keyed %>% 
  select(date.transect.block.key, date.transect.key, BOAT, SECTION, survey.block, survey.type, BEAUFORT, NOTES, WATERBIRDLOG, PROOF_LEVEL, entered.by = Initials) %>% 
  mutate(NOTES = as.character(NOTES),
         WATERBIRDLOG = as.character(WATERBIRDLOG),
         block.notes = ifelse(WATERBIRDLOG == "", NOTES, paste(NOTES, WATERBIRDLOG, sep = ". ")),
         PROOF_LEVEL = tolower(PROOF_LEVEL),
         PROOF_LEVELnum = ifelse(PROOF_LEVEL == "", 0,
                                 ifelse(PROOF_LEVEL == "ep", 1, 2)),
         number.observers = as.numeric("")) %>% 
  select(date.transect.block.key, date.transect.key, survey.block, survey.type, BEAUFORT, number.observers, block.notes, entered.by, PROOF_LEVELnum)

write.csv(wbirds.block, "WATERBIRDS_block.csv", row.names = F)

##-------------------------------------------------------

## make a long version of the full waterbirds data table
## nrow(wbirds.observation) should be = nrow(survey.blocks) * nrow(distinct(wbirds.observation, species))

wbirds.observation <- wbirds.keyed %>% 
  select(date.transect.block.key, everything(), -MONTH, -DAY, -YEAR, -date.transect.key, -date.key, -survey.block, -survey.type, -BOAT, -SECTION, 
         -ID, -BEAUFORT, -NOTES, -WATERBIRDLOG, -PROOF_LEVEL, -Initials, -transect) %>% 
  gather(species, count, -date.transect.block.key) %>%
  arrange(date.transect.block.key, species) %>% 
  mutate(observation.ID = seq(1:length(date.transect.block.key)))


# can then write it back to the same place the wide version csv is stored
write.csv(wbirds.observation, "WATERBIRDS_observation.csv", row.names = F)


##-------------------------------------------------------
## now to test the keys, can rebuild the full dataset by joining on the created keys...
wbirds.full.long <- wbirds.survey %>%
  full_join(., wbirds.transect, by = c("date.key")) %>% 
  full_join(., wbirds.block, by = c("date.transect.key")) %>% 
  inner_join(., wbirds.observation, by = c("date.transect.block.key")) %>%
  arrange(date.transect.block.key, species)

write.csv(wbirds.full.long, "WATERBIRDS_full_long.csv", row.names = F)


## ... and create the same full dataset created by gathering (making long) the original keyed wide dataset...
wbirds.test2 <- wbirds.keyed %>% 
  gather(species, count, -date.transect.block.key, -date.transect.key, -date.key, -survey.block, -survey.type, -BOAT, -SECTION, -ID, -MONTH, -DAY, -YEAR, -BEAUFORT, -NOTES, -WATERBIRDLOG, -PROOF_LEVEL, -Initials, -transect) %>% 
  select(date.transect.block.key, date.transect.key, date.key, survey.block, survey.type, species, count) %>% 
  arrange(date.transect.block.key, species)

## ... then compare the 2

foo2 <- wbirds.full.long %>% 
  select(date.transect.block.key, date.transect.key, date.key, survey.block, survey.type, species, count) %>% 
  arrange(date.transect.block.key, species)

identical(foo2, wbirds.test2) ## hopefully the result of this is TRUE

##-------------------------------------------------
## some additional helper/testing code


## double check that wbirds.block is correct
nrow(wbirds.block %>% distinct(date.transect.key)) #should be the same as nrow(wbirds.transect) 

## the number of survey dates * the number of survey.blocks is greater than the above nrow(), because some surveys didn't include all blocks
## simple table showing which sections were surveyed on which date
which.section.date <- wbirds %>% 
  mutate(survey.date = mdy(paste(MONTH, DAY, YEAR, sep = "-")),
         temp.block = paste(BOAT, SECTION, sep = ".")) %>%
  select(survey.date, temp.block) %>% 
  distinct()%>% 
  mutate(done = 1) %>% 
  spread(temp.block, done) 

which.section.date[is.na(which.section.date)]<-0

## tally the number of days each block was surveyed
which.section.date.colsums <- data.frame(colSums(which.section.date[,2:ncol(which.section.date)]))

## can make long to count the numer of missed blocks
which.section.date.long <- which.section.date %>% 
  gather(section, done, -survey.date) %>% 
  mutate(section = as.numeric(section))


## the following should be true:
## number of survey dates times the number of survey.blocks (71 * 17) = nrow(filter(which.section.date.long, done == "N")) + nrow(wbirds.block)


## there are 4 occurences of different beaufort scale was given to the same section by different boats. here they are
beauf.double.test <- data.frame(table(wbirds.test$survey.date, wbirds.test$SECTION)) %>% 
  rename(survey.date = Var1, SECTION = Var2, num.beaufs = Freq) %>% 
  filter(num.beaufs > 1)


library(xlsx)
library(tidyverse)
library(stringr)


## notes/code duplicating the pooled species negative tally worksheet: master_NegWBMachinePOOLED.xlsx

## pooled species are the spp pairs or triples that are sometimes undiferentiated in the field

## worksheet has cells to enter the pooled tally and the tallies for each of the undifferentiated species, for each of the following transects:

trans.sects.land <- data.frame(Transect = c("INVERNESS",
                                   "BIVALVE",
                                   "MILLERTON",
                                   "WALKER",
                                   "CGP"),
                     Section = c(1, 1, 1, 3, 2))

transects.boat <- c("1_EAST",
                    "2_MIDBAY_SUM",
                    "3_WEST")


# and following sections:
sections <- seq(1:4)

trans.secs.boat <- expand.grid(Transect = transects.boat,
                               Section = sections)

trans.sects <- rbind(trans.sects.land, trans.secs.boat)

#sample data

zdata.birds <- data.frame(EagrHogr = rnorm(17, mean = 1),
                             Eagr = rnorm(17, mean = 1),
                             Hogr = rnorm(17, mean = 1))%>% 
  mutate(EagrHogr = round(EagrHogr*100),
         Eagr = round(Eagr*100),
         Hogr = round(Hogr*100))


zdata <- cbind(trans.sects, zdata.birds)


zdata.fixed <- zdata %>% 
  mutate(EagrHogr = ifelse(Section == 1, abs(EagrHogr), EagrHogr),
         Eagr = ifelse(Section == 1, abs(Eagr), Eagr),
         Hogr = ifelse(Section == 1, abs(Hogr), Hogr),
         PooledLost = ifelse(EagrHogr < 0, EagrHogr, 0),
         FinalRecord = ifelse(EagrHogr < 0, 0, EagrHogr))



section.sums <- zdata.fixed %>% 
  group_by(Section) %>% 
  summarise(EagrHogrSum = sum(EagrHogr),
            EagrSum = sum(Eagr),
            HogrSum = sum(Hogr),
            PooledLostSum = sum(PooledLost),
            FinalRecordSum = sum(FinalRecord)) %>% 
  gather(varb, value, -Section) %>% 
  mutate(varbSect = paste(varb, Section, sep = "_")) %>% 
  select(varbSect, value) %>% 
  spread(varbSect, value) %>% 
  select(EagrHogrSum_1, PooledLostSum_1, EagrSum_1, HogrSum_1, FinalRecordSum_1, 
         EagrHogrSum_2, PooledLostSum_2, EagrSum_2, HogrSum_2, FinalRecordSum_2,
         EagrHogrSum_3, PooledLostSum_3, EagrSum_3, HogrSum_3, FinalRecordSum_3,
         EagrHogrSum_4, PooledLostSum_4, EagrSum_4, HogrSum_4, FinalRecordSum_4)


###############################################3

# read in existing data from Neg Machine xlsx files

pooled_list <- list.files("NEGATIVETALLIES_copy042418/pooled")


read_pooled_neg_machine <- function(zfile) {

pooled <- read.xlsx(paste("NEGATIVETALLIES_copy042418/pooled/", zfile, sep = ""), sheetIndex = 1, startRow = 13, endRow = 25, header = T) %>% 
  data.frame() %>% 
  select(Transect, contains("Final"))


pooled2 <- pooled %>% 
  mutate(zfile = zfile,
         which.sum = ifelse(Transect == "Cumulative Net Field Tally \"lost\" pooled birds minus differentiated birds", "net", as.character(Transect))) %>% 
  select(which.sum, contains("Final"), zfile)

return(pooled2)
}

wbird_pooled <- map_df(pooled_list, read_pooled_neg_machine)
zcols <- c(3, 6, 9)
corm2 <- corm[,zcols]



foo <- corm %>% 
  select(Transect, everything(), -contains("lost"), -contains("Final"), -contains("proportion"), -contains("allocated")) 
%>% 
  filter(!is.na(Transect)) %>% 
  gather(field, tally, -Transect) %>% 
  filter(!is.na(tally)) %>% 
  mutate(section = ifelse(str_detect(field, "Section.1") == T, 1, ""),
         section = ifelse(str_detect(field, "Section.2") == T, 2, section),
         section = ifelse(str_detect(field, "Section.3") == T, 3, section),
         section = ifelse(str_detect(field, "Section.4") == T, 4, section))



##################################################
## read waterbird database

waterbirds <- read.xlsx("WATERBIRD_copy042418.xlsx", sheetIndex = 1)

wbirds_filter <- function(month, day, year, spp) {
  new <- waterbirds %>% 
    filter(MONTH == month, DAY == day, YEAR == year) %>% 
    select(ID, MONTH, DAY, YEAR, SECTION, BOAT, spp)
  return(new)
}

scaup20040110 <- wbirds_filter(01, 10, 2004, "SCAUP")
brco20080112 <- wbirds_filter(01, 12, 2008, "BRCO")

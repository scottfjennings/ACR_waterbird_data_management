

# NO RUN for tallies entered as character string in single cell ----
tallies <- read.csv(here("data_files/entered_raw/tallies.csv")) %>% 
  mutate(alpha.code = toupper(alpha.code)) %>% 
  filter(tally != "") %>% 
  mutate(row.index = row_number())


split_tallies <- function(zrow.index) {
tally1 <- filter(tallies,
                 row.index == zrow.index)

tally_ho <- str_split(tally1$tally, pattern = ",") %>% 
  unlist() %>% 
  data.frame() %>% 
  rename(tally = 1) %>% 
  mutate(row.index = tally1$row.index,
         tally = trimws(tally),
         tally = as.numeric(tally))
}


split_tallies <- map_df(tallies$row.index, split_tallies) %>% 
  full_join(tallies %>% select(-tally))

split_tallies_pos_neg <- split_tallies %>% 
  mutate(pos.neg = ifelse(tally < 0, "neg.tally", "pos.tally")) %>%
  group_by(date, transect, section, alpha.code, pos.neg) %>% 
  summarise(tally.sum = sum(tally)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(date, transect, section, alpha.code), names_from = pos.neg, values_from = tally.sum) %>% 
  mutate(net.tally = pos.tally + neg.tally)


write.csv(split_tallies_pos_neg, here("data_files/tracking_finding_problem_records/summed_tallies.csv"), row.names = FALSE)





block_sums %>%
filter(date > "2019-06-01") %>% 
mutate(net = positive + negative) %>%
group_by(date) %>%
summarize(tot.birds = sum(net)) %>%
view()

 


# NO RUN check which middle transect generally has more birds ----
long_tallies2 %>% 
  filter(species == "RTLO") %>% 
  group_by(date, transect) %>% 
  summarise(tot.trans = sum(tally)) %>% 
  ungroup() %>%  
  group_by(transect) %>% 
  summarise(mean.tot.trans = mean(tot.trans)) %>% view()






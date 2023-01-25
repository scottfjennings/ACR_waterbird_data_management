

# we divy pooled birds based on the ratio of positively IDed constituent species  

# the following series of functions from waterbird_cleaning2_split_groups.R will determine the ratio of constituent species
  make_pooled_date_constituents() %>%  
  calc_section_constituent_sum_ratio() %>% 
  calc_bay_constituent_sum_ratio()

# we can test that these functions yield the intended result by creating test data with known ratios  
  
# first set up some hypothetical true data for a 2-species pooled group (e.g. CORM = DCCO & BRAC)
# these are the only 3 lines you should need to change  
constituent.ratios = c(0.6, 0.94, 0.15, 1) # true ratio of DCCO:BRAC in each section
tot.constituent = c(3000, 2500, 2006, 150) # true total number of combined DCCO and BRAC in each section
proportion.unid = c(.1, .1, .1, 0) # proportion of tot.constituent that that are not identified to species (tallied as CORM) 


# next generate test data from that truth
test_bay_constituent_values = data.frame(alpha.code = c("DCCO", "BRAC"),
                                      test.bay.constituent.sum = c((sum(constituent.ratios * ((1 - proportion.unid) * tot.constituent))), 
                                                                   (sum((1 - constituent.ratios) * ((1 - proportion.unid) * tot.constituent))))) %>% 
  mutate(test.bay.constituent.sum = round(test.bay.constituent.sum, 0),
         test.bay.all.constituent.sum = sum(test.bay.constituent.sum),
         test.abs.tally.ratio.bay = test.bay.constituent.sum/test.bay.all.constituent.sum)

test_ratios <- data.frame(section = as.character(rep(1:4, each = 2)),
                          alpha.code = rep(c("DCCO", "BRAC"), 4),
                          ratios = c(constituent.ratios[1], 1 - constituent.ratios[1], 
                                     constituent.ratios[2], 1 - constituent.ratios[2],
                                     constituent.ratios[3], 1 - constituent.ratios[3],
                                     constituent.ratios[4], 1 - constituent.ratios[4]))

test_pooled <- data.frame(date = "2020-01-15",
                          section = as.character(seq(1, 4)),
                          pooled.alpha.code = "CORM",
                          test.pooled.section.sum.tally = tot.constituent * proportion.unid) %>% 
  mutate(test.pooled.section.sum.final = test.pooled.section.sum.tally)

test_df_split_pooled <- data.frame(date = "2020-01-15",
                                   section = as.character(seq(1, 4)),
                                   tot.constituent = tot.constituent,
                                   proportion.unid = proportion.unid) %>% 
  full_join(test_ratios) %>% 
  mutate(section.sum.tally = ((1 - proportion.unid) * tot.constituent) * ratios,
         section.sum.tally = round(section.sum.tally, 0),
         section.sum.final = section.sum.tally) %>% 
  select(date, section, alpha.code, section.sum.tally, section.sum.final) %>% 
  bind_rows(data.frame(date = "2020-01-15",
                   section = as.character(seq(1, 4)),
                   alpha.code = "CORM",
                   section.sum.tally = proportion.unid * tot.constituent,
                   section.sum.final = proportion.unid * tot.constituent,
                   group.spp = "DCCO, BRAC")) %>% 
  filter(section.sum.final > 0) %>% 
  mutate(section = as.character(section)) %>% 
  arrange(date, section, alpha.code)



# then run the series of functions and set up a simple comparison between expected (true) and derived ratios

test_df_split_pooled %>% 
  make_pooled_date_constituents() %>%  
  calc_section_constituent_sum_ratio() %>% 
  calc_bay_constituent_sum_ratio() %>% 
  full_join(test_ratios) %>%  
  full_join(test_bay_constituent_values) %>% 
  mutate(correct.section.ratio = round(section.each.constituent.ratio, 2) == round(ratios, 2),
         correct.bay.constituent.sum = test.bay.constituent.sum == bay.constituent.sum,
         correct.bay.all.constituent.sum = test.bay.all.constituent.sum == bay.all.constituent.sum,
         correct.abs.tally.ratio.bay = test.abs.tally.ratio.bay == abs.tally.ratio.bay) %>% 
  view()
# all correct... should be TRUE

test_df_split_pooled %>% 
  get_pooled() %>% 
  full_join(test_pooled) %>% 
  mutate(across(c(pooled.section.sum.tally, pooled.section.sum.final), ~replace_na(., 0)),
         correct.section.tally = test.pooled.section.sum.tally == pooled.section.sum.tally,
         correct.section.final = test.pooled.section.sum.final == pooled.section.sum.final)




# testing negative reconciliation ----

neg_rec_test_df <- data.frame(date = rep(c("2020-01-15", "2020-01-20", "2020-01-25"), each = 4),
                              alpha.code = "BRAC",
                              section = rep(seq(1, 4), times = 3),
                              section.tally = c(1000, -1000, 0, 0,
                                                30, -25, 0, 0,
                                                -9000, 2000, 0, 0),
                              section.final = c(1000, 0, 0, 0,
                                                30, 0, 0, 0,
                                                0, 2000, 0, 0))



 neg_rec_test_df %>% 
  widen_section_tallies() %>% 
  calc_carried_forward()  %>% 
  calc_bay_sum() %>% view()

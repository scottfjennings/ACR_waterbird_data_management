Waterbird data cleaning and prep for analysis
================

This serves as a worked example showing how to process ACR waterbird
data from the raw database to a form usable for analysis. There are
three major steps in this workflow:  
1\. read the data, standardize field names and get fields in the correct
format. Functions for these tasks are defined in
waterbird\_cleaning1\_data\_read\_format.R  
2\. separate birds field-identified to pooled species groups into
constituent species. Functions for these tasks are defined in
waterbird\_cleaning2\_split\_groups.R  
3\. handle any negative counts (birds that flew north past the boats);
assign to the appropriate section and avoid double counting. Functions
for these tasks are defined in
waterbird\_cleaning3\_reconcile\_negatives.R

First load packages and setup

``` r
library(tidyverse)
library(lubridate)
library(here)
library(RODBC)

library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/birdnames_support/data/custom_bird_list")


options("scipen"=999)
source(here("code/waterbird_cleaning1_data_read_format.r"))
source(here("code/waterbird_cleaning2_split_groups.r"))
source(here("code/waterbird_cleaning3_reconcile_negatives.r"))
```

These are the core waterbird taxa that are represented in this database.
Other taxa exist in the db, but the following data management does not
apply to them.

``` r
wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")
#wbird_keep_taxa_gulls <- c("Anseriformes", "Alcidae", "Laridae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Suliformes")
```

read\_species\_table() and query\_waterbirds() read data directly from
the access database; the path where the database is stored must be
specified for both functions, so we define it as db here for simpler
code later.

``` r
db <- "C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/waterbirds_v2.0.accdb"
```

# Step 1. Read data and basic cleaning

Read the waterbirds data and do some basic data management in prep for
splitting POOLED and reconciling negatives. Unless otherwise indicated,
the functions are defined in waterbird\_cleaning1\_data\_read\_format.R.

``` r
# read in just the species lookup table from the access db
spp_table <- read_species_table(db) 

# query 
wbirds <- query_waterbirds(db) %>% 
  clean_waterbirds() %>% # basic field names and formatting
  sppindex_to_alpha(spp_table = spp_table) %>% # the query sets some alpha.codes to the lookup number in the species table instead of the 4-letter code
  mutate(alpha.code = update_alpha(alpha.code)) %>% # from birdnames
  fix_precount_block_names() %>% # assign precounts to the correct section: bivalve, millertonbivalve and inverness into section 1; cypressgrove into section 2: walkercreek into section 3
  bird_taxa_filter(keep_taxa = wbird_keep_taxa) %>% # from birdnames; filter to just waterbird species
  separate(block, into = c("transect", "section"), remove = F) %>% 
  mutate(section = gsub("sec", "", section))  %>% 
  select(date, block, section, transect, alpha.code, count) # we only need a few columns
```

From here, you may wish to save wbirds if you want to use unprocessed
data or want to do something with the data at the block level.

# Step 2. handle pooled birds.

The logic of splitting is as follows:

  - There are several species of waterbirds that may not be identified
    to species but that can be assigned to a POOLED group (e.g. LOON,
    CORM).

  - POOLED birds are allocated to the constituent species of that POOLED
    group based on the ratios of positively identified birds in each
    constituent species. Allocation is only done if the number of
    positively identified birds in the group is greater than the number
    of undifferentiated birds.

  - The bay is divided into 4 sections, numbered from south to north

  - If there are enough identified birds at the section scale, then the
    ratio of IDed birds in each species counted in that section is used
    to divvy the lumped birds. The group cutoff value is 50 for grebes,
    100 for all other groups.

  - If there are not enough IDed birds in the Section, then the ratio
    for IDed birds across the entire Bay is used. NOTE: IT APPEARS KELLY
    AND TAPPAN 1998 DID NOT DO THIS EXTRA STEP. DOING THIS EXTRA STEP
    INCLUDES MORE INDIVIDUAL BIRDS, BUT AT THE COST OF MORE LIBERAL
    ASSUMPTIONS ABOUT RATIOS OF IDENTIFIED AND UNIDENTIFIED BIRDS BEING
    CONSTANT THROUGHOUT THE ENTIRE BAY. THERE ARE AT LEAST SOME
    OCCASIONS WHERE THE CONSTITUENT SPP RATIO IN THE SECTION WITH POOLED
    BIRDS IS QUITE DIFFERENT THAN THE RATIO IN THE WHOLE BAY (E.G., )
    THE FOLLOWING CODE INDICATES WHETHER AN ALLOCATION WAS MADE BASED ON
    SECTION OR BAY RATIOS, SO THAT THE USER CAN DECIDE WHICH DATA TO
    INCLUDE IN ANALYSIS

Unless otherwise indicated, the functions are defined in
waterbird\_cleaning2\_split\_groups.R.

For each pooled group on each day, we want to calculate the ratios of
constituent species in that group (e.g. ratio of DCCO:BRAC:PECO on a
given day that some birds were assigned to CORM). We need to do this at
the section and bay scales.

The first part of step 2 is to sum the number of birds in each of the
four sections of the bay. In the final mutate() line here we add a
column “group.spp” for constituent species in each POOLED group, if
applicable.  
sum\_section\_tallies\_finals() incorporates birds counted in the
precount zones into the appropriate sections.

``` r
section_tallies_finals <- wbirds %>%  
  sum_section_tallies_finals() %>%
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) # from birdnames
```

Next we calculate section- and bay-scale ratios of constituent species.
\* make\_pooled\_date\_constituents() - for each date and section when
birds are IDed to a POOLED group, this function extracts the number of
birds IDed to the constituent species in that POOLED group on that day.
\* calc\_section\_constituent\_sum\_proportion() - calculate the ratio
of constituent species at the section level \*
calc\_bay\_constituent\_sum\_proportion() - calculate the ratio of
constituent species at the bay level

``` r
constituent_ratios <- section_tallies_finals %>% 
  make_pooled_date_constituents() %>% 
  calc_section_constituent_sum_proportion() %>% 
  calc_bay_constituent_sum_proportion()
# warning about missing pieces is expected
```

And then we peel off just the POOLED bird data from wbirds, join with
the ratio table, and allocate POOLED birds to constituent species based
on the appropriate ratio.

``` r
pooled_block <- get_pooled_block(wbirds)

allocated_pooled_block <- allocate_pooled_block(constituent_ratios, pooled_block) %>% 
  arrange(date, block, pooled.alpha.code, alpha.code)
```

A quick summary to view how many of each POOLED group were allocated by
section of bay ratio or remain unallocated.

``` r
allocated_pooled_block %>% 
    filter(allocation.scale != "no.allocation") %>%  
    group_by(pooled.alpha.code, allocation.scale)%>% 
    replace(., is.na(.), 0) %>%
    summarise(total = sum(abs(allocated.count))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = pooled.alpha.code, values_from = total, names_from = allocation.scale) %>% 
  full_join(., allocated_pooled_block %>%
              filter(allocation.scale == "no.allocation") %>%
              distinct(date, section, pooled.alpha.code, pooled.section.tally) %>%
              group_by(pooled.alpha.code) %>%
              summarise(no.allocation = sum(abs(pooled.section.tally))) %>%
              ungroup()) %>% 
  select(pooled.alpha.code, section, bay, no.allocation) %>% 
  view()
```

The final step in the splitting sub-workflow is to combine the split
pooled, unsplit pooled, and unpooled birds back together.

``` r
#split_unsplit_unpooled <- combine_split_unsplit_unpooled(section_tallies_finals, allocated_pooled)
split_unsplit_unpooled_block <- combine_split_unsplit_unpooled_block(wbirds, allocated_pooled_block)
```

# Step 3. Handle negatives

## The assumptions and logic for handling negatives:

  - We assume all birds flying past the boats originated on the bay (did
    not arrive to the bay behind the boats from the south during the
    count). Thus, all birds flying past the boats have either already
    been counted or they were missed in an area that was counted.

  - At the end of each section, there will be known birds behind the
    boats (positive counts) and known birds ahead of the boats (negative
    counts).
    
      - All of these birds have been counted at least 1 time.
      - Some of them have been counted 2 or more times. For these bird
        we want to either avoid counting them again (if they could have
        come from the pool of known birds behind the boats), or add them
        back to section 1 assuming that we missed them between the
        precount and boat count.

  - All negatives should be assigned to a section before the one they
    were detected in (and they should never be added to section 4), and
    because of that they should be subtracted from future section
    counts.
    
      - If the negatives can be explained by positives in each section
        they should be assigned to the section they were observed in.
      - If they cannot be explained by previous positives they should be
        added back to section 1 (most parsimonious assumption is that
        they were missed in precount or moved south between end of
        precount and start of boat count).

## The mechanics of handling negatives:

At the end of each section, we tally up the known birds behind and ahead
of the boats in that section. We compare those tallies to the cumulative
birds ahead of and behind the boats at the start of that section to
determine how many new birds we actually observed in that section.

  - Any new observed negatives in the current section are added to the
    cumulative negatives.

  - Any new observed positives are added to the cumulative positives.

  - If we started the section with any cumulative negatives, then we
    assume those birds were re-encountered in this section and we
    subtract those cumulative negatives from the current section
    positive tally.
    
      - If there are more current section positives than cumulative
        negatives, then the number of new positives in the current
        section is the number of observed positives minus the cumulative
        negatives.
    
      - If there are more cumulative negatives than current section
        positives, then we add no new positives for the current section
        and the balance of cumulative negatives (once current section
        positives have been subtracted) is carried forward to the next
        section.

  - If there are more cumulative negatives than cumulative positives,
    then that difference represents birds that we missed somewhere along
    the way; the most logical assumption is that we missed them between
    the end of the precount and the start of the boat count, so we add
    them back to section 1.

Once we do this for all 4 sections, we have:

  - a value for known new birds added to each section

  - a value for number of missed birds to add back to section 1

  - a value for the number of known birds ahead of the boat

  - a value for the number of known birds behind the boat

As a check on the process, the sum of the known birds ahead of and
behind the boats at the end of section 4 should equal the sum of the
number of birds assigned to each section plus those to be added back to
section 1. This test is formalized below with
check\_subtracted\_forward().

A quick look at how many dates we have negatives for each species in
each block:

``` r
filter(wbirds, count < -100) %>% 
  count(alpha.code, block) %>% 
  pivot_wider(id_cols = alpha.code, values_from = n, names_from = block) %>% 
  select(alpha.code, contains("1"), contains("2"), contains("3"), contains("4")) %>% 
  view()
```

And now the series of functions to do the negative handling.

``` r
subtracted_forward <- split_unsplit_unpooled_block %>% 
  get_section_pos_neg() %>% 
  widen_block_pos_neg() %>% 
  subtract_forward_add_back()
```

subtracted\_added\_viewer() is a function that reshapes a single species
X date record for easier viewing (and hopefully understanding) of how
negatives were handled. See the included
new\_negative\_logic\_examples.pdf for some worked examples of how
negatives are handled. The examples are for the following species dates:
BRAC 2004-12-18, 2009-02-08, 2013-02-09, 2015-02-14; DCCO 2019-01-12

``` r
subtracted_added_viewer(zdate = "2004-12-18", zalpha.code = "BRAC")

subtracted_added_viewer(zdate = "2009-02-08", zalpha.code == "BRAC")

subtracted_added_viewer(zdate = "2019-01-12", zalpha.code == "DCCO")

subtracted_added_viewer(zdate = "2013-02-09", zalpha.code == "BRAC")

subtracted_added_viewer(zdate = "2015-02-14", zalpha.code == "BRAC")
```

Check for any records where the sum of the known birds ahead of and
behind the boats at the end of section 4 does NOT equal the sum of the
number of birds assigned to each section plus those to be added back to
section 1.

``` r
check_subtracted_forward(subtracted_forward) %>% 
  arrange(-diff.total) %>% # all diff.total should be 0, this puts any >0 at the top; 
  view()
```

Finally, summarize the negative handled data (mostly just adding the net
negatives back to section 1). waterbirds4\_analysis is the data ready
for any analysis, data visualization, etc.

``` r
waterbirds4_analysis <- make_wbirds4_analysis(subtracted_forward)

saveRDS(waterbirds4_analysis, "data_files/working_rds/waterbirds4_analysis")
```
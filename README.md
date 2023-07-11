---
output:
  pdf_document: default
  html_document: default
---
# ACR_waterbird_data_management  
Code to compile and clean waterbird abundance data collected by Audubon Canyon Ranch as part of a long term monitoring project on Tomales Bay, CA  

This README provides some background information on this monitoring project and the prior method of data management, and brief description of the repository contents. The code file `code/waterbird_data_management_workflow.R` (https://github.com/scottfjennings/ACR_waterbird_data_management/blob/main/code/waterbird_data_management_workflow.R) contains a worked example of how to call the series of functions in this repository, and provides more detail on the rationale for each step of the process.  



For full description of this long term project see:  
  + Kelly, J. P., & Tappen, S. L. (1998). Distribution, abundance, and implications for conservation of winter waterbirds on Tomales Bay, California. Western Birds, 29, 103–120.  
  + Kelly, J. P., Rothenbach, C. A., & Weathers, W. W. (2018). Echoes of numerical dependence: Responses of wintering waterbirds to Pacific herring spawns. Marine Ecology Progress Series, 597, 243–257. https://doi.org/10.3354/meps12594  
  

In brief, this monitoring project began in 1989 and involves up to 4 full censuses each winter of all waterbirds on Tomales Bay, CA. Surveys count all individuals of species in the following orders and families (Anseriformes, Alcidae, Laridae, Gaviidae, Pelecanidae, Podicipediformes, Suliformes). The field methods for these surveys result in 2 large data issues that need to be corrected prior to analysis:  
  1. Individual birds that cannot be identified to species are tallied as part of a higher taxonomic group  
  2. Birds fly past the survey boats in the direction that the boats are traveling, in some cases yielding a net negative tally for a section of the bay  

The main task of the code in this repository is to remedy these 2 issues to the greatest extent possible. These tasks were originally accomplished using xlsx worksheets. These worksheets had multiple limitations:
* The logic behind the workflow was poorly documented, and figuring out the mechanics of each operation was very difficult
* There was a separate file for each POOLED taxa group on each day; as of 2019 there were almost 190 of these files. Because of this:
** There was an extra step of data transfer to these files then back to the database, which required an extra step of proofing and an extra chance for transcription errors to survive to the final database
** There was no practical way to make a slight change to the method and have that change affect pre-2019 data. Any such change would need to be made to all ~190 files. 

And, most critically, these worksheets did not properly handle negatives. There are 2 main problems:
* When large flocks were counted as positives early in the survey then they flew passed the boats to apparently leave the bay, they were counted a second time when the net negatives were added back to the section 4 tally.
* When large flocks were missed early in the survey then they flew passed the boats to apparently leave the bay, they were added to the section 4 tally when in reality they were most logically using the bay farther south, and should be added to a farther south section.

This repository holds code to accomplish the task of dealing with negatives and pooled birds while avoiding the problems of the previous negative method.

## The workflow    

# there are 3 main steps to preparing the waterbird data for use. In this workflow, these steps are accomplished using functions which are defined in separate files.  

# 1. Read in and do basic cleaning - as of Jan 2023 the data exist in 3 different locations. 
    # The raw tally files contain a separate file for each survey date. The data are entered in a wide format with "species" and "tally" column pairs for each survey block, and the tally columns have every individual tally as they appear on the data sheets. This format makes for efficient data entry and proofing but needs additional wrangling before it can be efficiently cleaned and managed. These data were entered and proofed in 2022-23 and should be considered the cleanest version of the data. These data are as free as possible from any arithmetic errors or inconsistent handling of negatives. The data in this format allow the same handling of pooled and negatives across all years. To read these data use 1_read_clean_from_raw_tallies.R
    # The access database has the historically cleaned data. The data cleaning process to create this database appears to have changed through the history of the project, and these changes were apparently never fully documented. If you want to read data from this database, use functions in 1_read_clean_from_access.R
    # The Negative Machine files are .xlsx files used to calculate net negatives that remain at the end of section 4. The raw data in these files is likely unreliable, so it is unlikely that you will want to read it in, but if you do, use 1_read_clean_from_NegMachine.R

# 2. Split pooled birds (birds IDed to higher taxonomic groups than species)
    # This step uses functions defined in 2_split_pooled.R

# 3. Reconcile negatives (birds that flew forward of the boats) - there are 2 possible methods to handle negatives, depending on whether you assume birds flying forward of the boats land in the current section or fly past the current section.
    # The Negative Machine method, which was apparently used from about 2000 onward, assumes that negatives land in the current section and thus should be subtracted from the current section count. The data can be processed with this method of handing negatives using the functions defined in 3_negative_machine.R
    # If you assume negatives fly beyond the current section then you would subtract them from future sections and NOT from the current section. To process the data with thie method use functions defined in 3_subtract_forward_negatives.R. NOTE: as of 1/31/2023 this may not be fully functional.
    

There are also a few helper files with additional code

* utils.R contains some utility functions that are used in mutliple places throughout the workflow

* extras/total_kayaks.R calculates and plots the total number of kayaks seen per survey. Plot is saved to figures_output/total_kayaks.png

* extras/function_tests.R - Creates some basic toy data frames and uses them to test certain functions. This mostly works for the split_groups functions.

* extras/old_negative_machine.R - Contains functions that replicate the calculations in the old WBNegMachine xlsx files. These calculations are incorrect so these functions are saved mostly for project record keeping.

* extras/view_negative_examples.RMD and associated rendered files contains a description of data from some surveys where the assumptions of out field protocol and data management might be problematic. These are some example days where we get substantially different baywide totals if assuming negatives land in the current section (where they were encountered as negatives) vs if we assume they land in farther north sections. See next bullet for note on data used in this.

* extras/wrangle_negatives.R prior to the current (as of winter 2022) raw data entry system, SJ experimented with only re-entering the negatives on each data sheet. These data are saved at data_files/entered_raw/negatives.csv. This file contains code to wrangle those data and combine with the existing (Access) data to try and reconstruct separate positive and negative values. This shouldn't be needed for the current workflow, but these wrangled negatives are used in the current version of view_negative_examples.

### contemporary data management notes
1/31/22 - SJ. Last week in testing my negative calculations, I discovered that in the new database, some records have count set to the value derived from the old NegWBMachine spreadsheets, rather than the value recorded on the data sheet. I wrote extract_NegMachine.R to scrape the raw values from there to compare to the database, with the logic that mismatches would indicate where we need to go back and check the raw data sheets.

Also this date Emi sent me some more xlsx files she found. These have a slightly different file name "NegativeWaterbirdMagicMachine" vs "NegWBMachine" and calculate the carried forward tally in a slightly different way (but with same result)

### old data management notes

JK, Rothenback analysis notes, from: S:\Databases\Waterbirds_data\Dropbox files from Christine 20141111\Data Manipulation.doc
Counts started in December 1989. Missing data are: Jan+Feb 1991, Feb 1996, Jan 1998, Feb 2000, Feb 2001 *2 in Jan*, Dec 2002, Feb 2005 *2 in Jan*, (Feb 2010 *2 in Jan*, Jan + Dec 2012). () = outside herring survey dates. 1996 is missing February's data, and also has the earliest survey date in January (6th).
Sections 6, 7, 8 were combined with the rest of the data until December of 1990. Section 5 was combined with the rest of the data until December 1991. Section 9 was combined until January 1992. 
Section 5: missing 01/93, 12/96, 02/06, 
Section 7: missing 02/04
Section 8: missing 12/91, 12/11
Section 9: missing 02/04

included PECO in CORM allocation, included all 3 scoters in SCOTER allocation
For bayabun and baydens – excluded all raptors, terns, gulls, and kingfisher.

### Data quirks
2013/12/21 the survey was run from north to south so normal negative reconciliation must be reversed.
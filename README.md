---
output:
  pdf_document: default
  html_document: default
---
# ACR_waterbird_data_management  
Code to compile and clean waterbird abundance data collected by Audubon Canyon Ranch as part of a long term monitoring project on Tomales Bay, CA  

This README provides some background information on this monitoring project and the prior method of data management, and brief description of the repository contents. The [vignette](https://github.com/scottfjennings/ACR_waterbird_data_management/blob/main/vignette.md) contains a worked example of how to call the series of functions in this repository, and provides more detail on the rationale for each step of the process.  



For full description of this long term project see:  
  + Kelly, J. P., & Tappen, S. L. (1998). Distribution, abundance, and implications for conservation of winter waterbirds on Tomales Bay, California. Western Birds, 29, 103–120.  
  + Kelly, J. P., Rothenbach, C. A., & Weathers, W. W. (2018). Echoes of numerical dependence: Responses of wintering waterbirds to Pacific herring spawns. Marine Ecology Progress Series, 597, 243–257. https://doi.org/10.3354/meps12594  
  

In brief, this monitoring project began in 1989 and involves up to 4 full censuses of all waterbirds on Tomales Bay, CA. Surveys count all individuals of species in the following orders and families (Anseriformes, Alcidae, Laridae, Gaviidae, Pelecanidae, Podicipediformes, Suliformes). These surveys result in 2 large data issues that need to be corrected prior to analysis:  
  1. Individual birds that cannot be identified to species are tallied as part of a higher taxonomic group  
  2. Birds fly past the survey boats in the direction that the boats are traveling, in some cases yielding a net negative tally for a section of the bay  

The main task of this code is to remedy these 2 issues to the greatest extent possible. These tasks were originally accomplished using xlsx worksheets. These worksheets had multiple limitations:
* The logic behind the workflow was poorly documented, and figuring out the mechanics of each operation was very difficult
* There was a separate file for each POOLED taxa group on each day; as of 2019 there were almost 190 of these files. Because of this:
** There was an extra step of data transfer to these files then back to the database, which required an extra step of proofing and an extra chance for transcription errors to survive to the final database
** There was no practical way to make a slight change to the method and have that change affect pre-2019 data. Any such change would need to be made to all ~190 files. 

And, most critically, these worksheets did not properly handle negatives. There are 2 main problems:
* When large flocks were counted as positives early in the survey then they flew passed the boats to apparently leave the bay, they were counted a second time when the net negatives were added back to the section 4 tally.
* When large flocks were missed early in the survey then they flew passed the boats to apparently leave the bay, they were added to the section 4 tally when in reality they were most logically using the bay farther south, and should be added to a farther south section.

This repository holds code to accomplish the task of dealing with negatives and pooled birds while avoiding the problems of the previous negative method.

## The workflow    

There are three major steps in this data management workflow; each step is accomplished with a series of functions, and the functions for each step are defined together in the three waterbird_cleaning... files listed below.   

* waterbird_cleaning1_utils.R - Functions for preliminary data cleaning prior to the main tasks (standardize field names, change data types), and also some summary functions that are used multiple places. 

* waterbird_cleaning2_split_groups.R - Functions to assign birds that were identified to higher taxonomic levels are assigned to species level where possible.

* waterbird_cleaning3_reconcile_negatives.R - Functions to reconcile any negative counts.  

There are also a few helper files with additional code

* function_tests.R - Creates some basic toy data frames and uses them to test certain functions. This mostly works for the split_groups functions.

* old_negative_machine.R - Contains functions that replicate the calculations in the old WBNegMachine xlsx files. These calculations are incorrect so these functions are saved mostly for project record keeping.

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
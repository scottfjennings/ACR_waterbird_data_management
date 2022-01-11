# ACR_waterbird_data_management  
Code to compile and clean waterbird abundance data collected by Audubon Canyon Ranch as part of a long term monitoring project on Tomales Bay, CA  


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

In 2019 work was begun to transfer these tasks to R code. 

## Code    

* waterbird_cleaning1_variable_names.R - preliminary data cleaning prior to the main tasks. Mainly pipes the raw data through a series of functions that are defined in waterbird_utility_functions.R (see below) and elsewhere.  

* waterbird_cleaning2_split_groups.R - this is where birds that were identified to higher taxonomic levels are assigned to species level where possible.  

* waterbird_cleaning3_reconcile_negatives.R - this is where any net negative tallies are reconciled.  

* waterbird_cleaning4_create_wbirds4analysis.R - the code in the previous files results in a few different objects. Here they are combined into a single object which can be saved to disk and used for any subsequent analysis.   

* waterbird_utility_functions.R - defines several functions and creates some small objects that are used in multiple places in the data management workflow. This file is called with source() where needed in other files, so generally users should not need to manually run any code here.  

* make_new_wbirds_dbase.R - this code reshapes data from the original wide version to the new, longer and relational version. This code is not part of any current data management work flow. It is saved here for reference.  

* waterbirds_pooled_spp_neg_tallies.R - this code replicates as closely as possible the old xlsx files that originally accomplished what waterbird_cleaning3_reconcile_negatives.R now does. . This code is not part of any current data management work flow. It is saved here for reference.  


### old data management notes/

JK, Rothenback analysis notes, from: S:\Databases\Waterbirds_data\Dropbox files from Christine 20141111\Data Manipulation.doc
Counts started in December 1989. Missing data are: Jan+Feb 1991, Feb 1996, Jan 1998, Feb 2000, Feb 2001 *2 in Jan*, Dec 2002, Feb 2005 *2 in Jan*, (Feb 2010 *2 in Jan*, Jan + Dec 2012). () = outside herring survey dates. 1996 is missing February's data, and also has the earliest survey date in January (6th).
# Sections 6, 7, 8 were combined with the rest of the data until December of 1990. Section 5 was combined with the rest of the data until December 1991. Section 9 was combined until January 1992. 
Section 5: missing 01/93, 12/96, 02/06, 
Section 7: missing 02/04
Section 8: missing 12/91, 12/11
Section 9: missing 02/04

included PECO in CORM allocation, included all 3 scoters in SCOTER allocation
For bayabun and baydens – excluded all raptors, terns, gulls, and kingfisher.


# ACR_waterbird_data_management  
Code to compile and clean waterbird abundance data collected by Audubon Canyon Ranch as part of a long term monitoring project on Tomales Bay, CA  


For full description of this long term project see:  
  + Kelly, J. P., & Tappen, S. L. (1998). Distribution, abundance, and implications for conservation of winter waterbirds on Tomales Bay, California. Western Birds, 29, 103–120.  
  + Kelly, J. P., Rothenbach, C. A., & Weathers, W. W. (2018). Echoes of numerical dependence: Responses of wintering waterbirds to Pacific herring spawns. Marine Ecology Progress Series, 597, 243–257. https://doi.org/10.3354/meps12594  
  

In brief, this monitoring project began in 1989 and involves up to 4 full censuses of all waterbirds on Tomales Bay, CA. Surveys count all individuals of species in the following orders and families (Anseriformes, Alcidae, Laridae, Gaviidae, Pelecanidae, Podicipediformes, Suliformes). These surveys result in 2 large data issues that need to be corrected prior to analysis:  
  1. Individual birds that cannot be identified to species are tallied as part of a higher taxonomic group  
  2. Birds fly past the survey boats in the direction that the boats are traveling, in some cases yielding a net negative tally for a section of the bay  


The main task of this code is to remedy these 2 issues to the greatest extent possible. 

## Code  
*code is organized into 3 subfolders*   

### data_clean_manage/
* waterbird_cleaning1_variable_names.R - preliminary data cleaning prior to the main tasks. Mainly pipes the raw data through a series of functions that are defined in waterbird_utility_functions.R (see below) and elsewhere.  

* waterbird_cleaning2_split_groups.R - this is where species that were identified to higher taxonomic levels are assigned to species level where possible.  

* waterbird_cleaning3_reconcile_negatives.R - this is where any net negative tallies are reconciled.  

* waterbird_cleaning4_create_wbirds4analysis.R - the code in the previous files results in a few different objects. Here they are combined into a single object which can be saved to disk and used for any subsequent analysis.   

### utility/
* waterbird_utility_functions.R - defines several functions and creates some small objects that are used in multiple places in the data management workflow. This file is called with source() where needed in other files, so generally users should not need to manually run any code here.  

* make_new_wbirds_dbase.R - this code reshapes data from the original wide version to the new, longer and relational version. This code is not part of any current data management work flow. It is saved here for reference.  

* waterbirds_pooled_spp_neg_tallies.R - this code replicates as closely as possible the old xlsx files that originally accomplished what waterbird_cleaning3_reconcile_negatives.R now does. . This code is not part of any current data management work flow. It is saved here for reference.  

### data_summary_visualize/

* basic_plots_summaries.R - self explanatory file name  

* check_spp_by_section.Rmd - check which section of the bay each species is generally most commonly encountered. This code was developed as part of efforts to identify possible opportunities to reduce survey effort.  

* data_checking.R - self explanatory file name.  

* visualize_species_detections_proportions.Rmd - How many species are detected during ACR Tomales Bay Waterbird surveys, how frequently is each species detected, and what proportion of the total waterbirds does each species represent?  

* waterbirds_basic_summaries.R - also self explanatory.  

* waterbird_species_list.Rmd - output a list of all species in the database

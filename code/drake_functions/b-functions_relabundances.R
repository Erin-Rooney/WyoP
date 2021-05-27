# UWYO cover crop and compost results in R
# ERIN ROONEY
# MAY 26 2021

################################################## #

# `b-functions_relabundances.R`

################################################## #

## this script will load functions for:
## (a) processing P pools, enzymes, som, biomass data
## -- (a1) standardizing  to grams of cover crop biomass
## -- (a2) 
## -- (a3) cleaning the data file and creating a longform version 
## -- (a4) 
## (b) computing relative abundance by p type, for each fertility, cover crop, time  

# INSTRUCTIONS:
## source this file in the `5_drakeplan.R` file, do not run the script here.
## This script can (generally) be used as is for most data that follow this format. No modifications needed 

## edit: all files are unique to this experiment because this is a unique experiment.


################################################## #
################################################## #

# 1. PROCESSING FUNCTIONS -------------------------------------------------
## LEVEL I FUNCTIONS -------------------------------------------------------


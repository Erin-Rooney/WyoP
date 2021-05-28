# UWYO cover crop and compost results in R
# ERIN ROONEY
# MAY 26 2021

################################################## #

# `d-functions_stats.R`

################################################## #

## this script will load functions for:
## (a) aov stats
## (b) hsd stats
## (c) permanovas

# INSTRUCTIONS:
## source this file in the `5_drakeplan.R` file, do not run the script here.
## This script can (generally) be used as is for most data that follow this format. No modifications needed 

## edit: all files are unique to this experiment because this is a unique experiment.


################################################## #
################################################## #

# 1. PROCESSING FUNCTIONS -------------------------------------------------
## LEVEL I FUNCTIONS -------------------------------------------------------

permanova = function(p_relabund_by_sample){
relabund_wide_sample =
  p_relabund_by_sample %>%
  ungroup %>% 
  select(X, time, ctrt, ftrt, phosphorus_pool, relabund) %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "pbic_percbio" = "Available P",
                                  'amac_percbio' = "Reserve P",
                                  'unavp_percbio' = "Fixed P",
                                  'porg_percbio' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P"))) %>%
  pivot_wider(names_from = "phosphorus_pool", values_from = "relabund") %>%
  filter(ctrt != "Control" & time != "Baseline") %>% 
  replace(is.na(.),0) 


library(vegan)
permanova_ftrt_ctrt_all = 
  adonis(relabund_wide_sample %>% select(c(`Available P`, `Reserve P`, `Organic P`, `Fixed P`)) ~ 
           (ctrt*ftrt*time), 
         data = relabund_wide_sample) 



permatable = permanova_ftrt_ctrt_all$aov.tab

write.csv(permatable, "permatable.csv", row.names = TRUE)

}
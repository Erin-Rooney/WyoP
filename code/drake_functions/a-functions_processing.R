# UWYO cover crop and compost results in R
# ERIN ROONEY
# MAY 26 2021

################################################## #

# `a-functions_processing.R`

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

standardized_incub_dat = function(incub_dat){
  incub_dat %>% 
  dplyr::mutate(unavp = as.numeric(unavp),
                amac = as.numeric(amac)) %>% 
  dplyr::mutate(phos_percbio  = (phos/cbio),
                bg_percbio = (bg/cbio),
                nag_percbio = (nag/cbio),
                ag_percbio = (ag/cbio),
                lap_percbio = (lap/cbio),
                abts_percbio = (abts/cbio),
                pbic_percbio = (pbic/cbio),
                amac_percbio = (amac/cbio),
                edta_percbio = (edta/cbio),
                unavp_percbio = (unavp/cbio),
                porg_percbio = (porg/cbio),
                ptot_percbio = (ptot/cbio))  
  
}

  
incub_dat_enzymes_longer = function(standardized_incub_dat){
  standardized_incub_dat %>% 
  select(time, ctrt, ftrt, phos_percbio, bg_percbio, nag_percbio, ag_percbio, lap_percbio, abts_percbio) %>% 
  pivot_longer(-c(time, ctrt, ftrt), names_to = 'enzymes_type', values_to = 'enzymes_percbio') %>% 
  filter(ctrt != "Control")
  
}

## LEVEL II FUNCTIONS -------------------------------------------------------


p_relabund_by_sample = function(standardized_incub_dat){
  standardized_incub_dat %>%
    na.omit() %>% 
    select(X, time, ctrt, ftrt, pbic_percbio, amac_percbio, unavp_percbio, porg_percbio) %>% 
    #is this where I need to summarize? pivot longer then pivot wider again?
    pivot_longer(-c(X, time, ctrt, ftrt), 
                 names_to = 'phosphorus_pool', values_to = 'abund') %>% 
    mutate(abund = if_else(abund < 0, 0, abund)) %>% 
    group_by(X, ctrt, ftrt, time) %>%
    dplyr::mutate(total = sum(abund)) %>% 
    ungroup() %>% 
    mutate(relabund = (abund/total)*100) %>% 
    mutate(phosphorus_pool = recode(phosphorus_pool, "pbic_percbio" = "Available P",
                                    'amac_percbio' = "Reserve P",
                                    'unavp_percbio' = "Fixed P",
                                    'porg_percbio' = "Organic P")) %>% 
    mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P")))
  
}

p_relabund_summary = function(p_relabund_by_sample){ 
  p_relabund_by_sample %>% 
  group_by(ctrt, ftrt, time, phosphorus_pool) %>% 
  dplyr::summarise(relabund_mean = mean(relabund)) %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "pbic_percbio" = "Available P",
                                  'amac_percbio' = "Reserve P",
                                  'unavp_percbio' = "Fixed P",
                                  'porg_percbio' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P")))

}

p_relabund_summary_se = function(p_relabund_by_sample){ 
  p_relabund_by_sample %>% 
  group_by(ctrt, ftrt, time, phosphorus_pool) %>% 
  dplyr::summarise(relabund_mean = round(mean(relabund), 2),
                   se = round(sd(relabund)/sqrt(n()),2)) %>%
  mutate(summary = paste(relabund_mean, "\u00b1", se)) %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "pbic_percbio" = "Available P",
                                  'amac_percbio' = "Reserve P",
                                  'unavp_percbio' = "Fixed P",
                                  'porg_percbio' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P"))) %>% 
  select(c(-relabund_mean, -se))
  
}
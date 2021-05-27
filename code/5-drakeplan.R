
## DRAKE PLAN
## USE THIS SCRIPT/PLAN TO PROCESS, ANALYZE, AND GRAPH UWYO BIOMASS, PHOS, AND SOM DATA
## ERIN C ROONEY
## MAY-26-2021

##############################
##############################

## SOURCE THE FUNCTION FILES FIRST, AND THEN RUN THE DRAKE PLAN
## DON'T RUN THE PROCESSING PLAN MULTIPLE TIMES. ONCE IS ENOUGH.

##############################
##############################


# 0. load packages --------------------------------------------------------
library(drake)
library(tidyverse)
library(PNWColors)
library(soilpalettes)
library(wesanderson)

# 1. SET input file paths -------------------------------
INCUB = "raw/Inc_finaldata_July2.csv"
BIOMASS = "raw/biocorr_1.csv" 
GH = "raw/gh_pfrac.csv"
PALL = "raw/allfert_allcrop_P.csv"

## SET the treatment variables
TREATMENTS = quos(ftrt, ctrt, time)

# 2. source the functions --------------------------------------------------------
source("code/drake_functions/a-functions_processing.R")
source("code/drake_functions/b-functions_relabundances.R")
source("code/drake_functions/c-functions_ggplots.R")
source("code/drake_functions/d-functions_stats.R")

# 3. load drake plans -----------------------------------------------------
uwyo_drake_plan = drake_plan(
  # a. PROCESSING ---- 
  
  incub_dat = read.csv(file_in(INCUB)),
  bio_dat = read.csv(file_in(BIOMASS)) %>%  rename('ctrt' = 'ï..ctrt'),
  gh_dat = read.csv(file_in(GH)) %>%  rename('ctrt' = 'ï..ctrt'),
  pall = read.csv(file_in(PALL)),
  
standardized = standardized_incub_dat(incub_dat),
standardized_p = standardized_pall (pall),
enzymes_longer = incub_dat_enzymes_longer(standardized),
relabund_sample = p_relabund_by_sample (standardized),
relabund_summary = p_relabund_summary(relabund_sample),
relabund_se = p_relabund_summary_se(relabund_sample),
  
  
  # b. RELABUND ----

  # c. PLOTS
phos_relabund = relabund_plot(relabund_summary),
phos_relabund_boxplot = relabund_sample_plot(relabund_sample),
available_p_point = pbic_abs_plot(standardized_p),
t1_available_p_point = t1_pbic_abs_plot(standardized_p),
t2_available_p_point = t2_pbic_abs_plot(standardized_p),
t1_reserve_p_point = t1_amac_abs_plot(standardized_p),
t2_reserve_p_point = t2_amac_abs_plot(standardized_p),
organic_p_point = porg_abs_plot(standardized_p),
reserve_p_point = amac_abs_plot(standardized_p),
unavailable_p_point = unavp_abs_plot(standardized_p),



  
  # # d. STATISTICS ---- 
  # ## PERMANOVA
  # fticr_permanova = compute_permanova(relabund_cores),
  # fticr_permanova_tzero = compute_permanova_tzero(relabund_cores),
  # 
  # ## PCA
  # gg_pca = compute_fticr_pca(relabund_cores), 
  # gg_pca2 = compute_fticr_pca2(relabund_cores), 
  # gg_pca_tzero = compute_fticr_pca_tzero(relabund_cores),

# # e. OUTPUT FILES ----
# fticr_meta %>% write.csv("data/processed/fticr/fticr_meta.csv", row.names = FALSE),
# fticr_data_trt %>% write.csv("data/processed/fticr/fticr_data_by_treatment.csv", row.names = FALSE),
# #  fticr_data_longform %>% write.csv() 

# REPORT
outputreport = rmarkdown::render(
  knitr_in("reports/exploratory_report.Rmd"),
  output_format = rmarkdown::github_document())
)


# 4. make plans -------------------------------------------------------------------------
make(uwyo_drake_plan, lock_cache = F)
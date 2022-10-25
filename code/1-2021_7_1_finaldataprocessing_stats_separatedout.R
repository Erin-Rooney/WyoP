#Greenhouse incubation
#E Rooney
#7 19 2021

#Stats


#load packages----------------
source("code/0-packages.R")

#load raw data--------------
final_dat = read.csv("raw/2021_7_1_finaldat.csv")


#data processing-----------


bio_dat2 = 
  final_dat %>% 
  mutate(ctrt = recode(ctrt, "ALL " = 'All Mixture',
                       "B" = 'Buckwheat',
                       "BO" = 'Buckwheat Oat',
                       "F" = "Faba Bean",
                       "FLW" = "Fallow",
                       "FO" = "Faba Bean Oat",
                       "O" = 'Oat',
                       "R" = 'Radish',
                       "RO" = 'Radish Oat')) %>% 
  # dplyr::mutate(species_group = case_when(grepl("All", ctrt)~"All",
  #                                grepl("Buckwheat", ctrt)~"B",
  #                                grepl("Faba", ctrt)~"F",
  #                                grepl("Radish", ctrt)~"R",
  #                                grepl("Fallow", ctrt)~"Fal")) 
  mutate(ctrt = factor(ctrt, levels = c('All Mixture', 'Buckwheat Oat', 'Buckwheat', 
                                        'Faba Bean Oat', "Faba Bean", 'Radish Oat', 
                                        "Radish", 'Oat', 'Fallow'))) %>% 
  dplyr::mutate(wbio = as.numeric(wbio),
                cbio = as.numeric(cbio)) %>% 
  dplyr::mutate(wbio_per_cbio = (wbio/cbio),
                wbiokg_ha = (wbio * 7.29),
                cbiokg_ha = (cbio * 7.29)) %>% 
  
  dplyr::mutate(ftrt = as.factor(ftrt),
                ctrt = as.factor(ctrt),
                time = as.factor(time),
                pbic = as.numeric(pbic),
                amac = as.numeric(amac),
                edta = as.numeric(edta),
                unavp = as.numeric(unavp),
                porg = as.numeric(porg),
                ptot = as.numeric(ptot),
                cbio = as.numeric(cbio),
                wbio = as.numeric(wbio),
                caco3 = as.numeric(caco3),
                inorgcarbon = as.numeric(inorgcarbon),
                amm = as.numeric(amm),
                nit = as.numeric(nit),
                pmn = as.numeric(pmn),
                pmc = as.numeric(pmc)
  ) %>% 
  dplyr::mutate(pbic_percbio  = (pbic/cbio),
                amac_percbio = (amac/cbio),
                unavp_percbio = (unavp/cbio),
                porg_percbio = (porg/cbio),
                inorgcarbon_percbio = (inorgcarbon/cbio),
                amm_percbio = (amm/cbio),
                nit_percbio = (nit/cbio),
                pmn_percbio = (pmn/cbio),
                pmc_percbio = (pmc/cbio)) %>% 
  select(-caco3, -inorgcarbon, -inorgcarbon_percbio, -sph2, -rate2, -seed2) %>% 
  na.omit()

ph_dat =
  final_dat %>% 
  mutate(ctrt = recode(ctrt, "ALL " = 'All Mixture',
                       "B" = 'Buckwheat',
                       "BO" = 'Buckwheat Oat',
                       "F" = "Faba Bean",
                       "FLW" = "Fallow",
                       "FO" = "Faba Bean Oat",
                       "O" = 'Oat',
                       "R" = 'Radish',
                       "RO" = 'Radish Oat')) %>% 
  # dplyr::mutate(species_group = case_when(grepl("All", ctrt)~"All",
  #                                grepl("Buckwheat", ctrt)~"B",
  #                                grepl("Faba", ctrt)~"F",
  #                                grepl("Radish", ctrt)~"R",
  #                                grepl("Fallow", ctrt)~"Fal")) 
  mutate(ctrt = factor(ctrt, levels = c('All Mixture', 'Buckwheat Oat', 'Buckwheat', 
                                        'Faba Bean Oat', "Faba Bean", 'Radish Oat', 
                                        "Radish", 'Oat', 'Fallow'))) %>% 
  dplyr::mutate(wbio = as.numeric(wbio),
                cbio = as.numeric(cbio)) %>% 
  dplyr::mutate(wbio_per_cbio = (wbio/cbio),
                wbiokg_ha = (wbio * 7.29),
                cbiokg_ha = (cbio * 7.29)) %>% 
  
  dplyr::mutate(ftrt = as.factor(ftrt),
                ctrt = as.factor(ctrt),
                time = as.factor(time),
                pbic = as.numeric(pbic),
                amac = as.numeric(amac),
                edta = as.numeric(edta),
                unavp = as.numeric(unavp),
                porg = as.numeric(porg),
                ptot = as.numeric(ptot),
                cbio = as.numeric(cbio),
                wbio = as.numeric(wbio),
                caco3 = as.numeric(caco3),
                inorgcarbon = as.numeric(inorgcarbon),
                amm = as.numeric(amm),
                nit = as.numeric(nit),
                pmn = as.numeric(pmn),
                pmc = as.numeric(pmc)
  ) %>% 
  dplyr::mutate(pbic_percbio  = (pbic/cbio),
                amac_percbio = (amac/cbio),
                unavp_percbio = (unavp/cbio),
                porg_percbio = (porg/cbio),
                inorgcarbon_percbio = (inorgcarbon/cbio),
                amm_percbio = (amm/cbio),
                nit_percbio = (nit/cbio),
                pmn_percbio = (pmn/cbio),
                pmc_percbio = (pmc/cbio)) %>% 
  na.omit()

ph_dat_grouped =
  ph_dat %>% 
  dplyr::mutate(grouping = if_else(ctrt == "Fallow", "Fallow", "Cover Crop")) %>% 
  group_by(ftrt, ctrt) %>% 
  dplyr::summarise(ph_mean = round(mean(sph2), 2),
                   ph_se = round(sd(sph2)/sqrt(n()),2)) 




bio_dat2_grouped =
  bio_dat2 %>%
  group_by(ctrt) %>% 
  dplyr::summarise(wbio = round(mean(wbiokg_ha), 2),
                   cbio = round(mean(cbiokg_ha), 2),
                   cbio_se = round(sd(cbiokg_ha)/sqrt(n()),2),
                   amm = round(mean(amm), 2),
                   nit = round(mean(nit), 2),
                   pmn = round(mean(pmn), 2),
                   pmc = round(mean(pmc), 2),
                   pbic = round(mean(pbic), 2),
                   amac = round(mean(amac), 2),
                   porg = round(mean(porg), 2),
                   unavp = round(mean(unavp), 2),
  ) %>% 
  dplyr::mutate(grouping = "yes")
#dplyr::mutate(grouping = if_else(ctrt == "Fallow", "Fallow", "Cover Crop")) 



###

print(ph_dat_grouped)

ph_dat_grouped %>% knitr::kable()

write.csv(ph_dat_grouped, "output/ph_dat_grouped.csv")

#

#stats---------------

# biomass = 
#   bio_dat2 %>% 
#   select(ftrt, ctrt, time, wbio, cbio) %>% 
#   group_by(ftrt, ctrt) %>% 
#   dplyr::summarise(cbio_mean = round(mean(cbio, na.rm= TRUE)),
#                    cbio_se = round(sd(cbio, na.rm= TRUE)/sqrt(n())),
#                    wbio_mean = round(mean(wbio, na.rm= TRUE)),
#                    wbio_se = round(sd(wbio, na.rm= TRUE)/sqrt(n())),
#                    
#   ) %>% 
#   mutate(covercrop = paste(cbio_mean, "\u00b1", cbio_se),
#          wheat = paste(wbio_mean, "\u00b1", wbio_se)
#          ) %>% 
#   select(ftrt, ctrt, wheat, covercrop) %>% 
#   mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
#                        "CNTL " = "Control",
#                        'IFERT ' = "Inorganic Fertilizer"))

#group by fertility treatment summary table for biomass

biomass1 = 
  bio_dat2 %>% 
  select(ftrt, ctrt, time, wbiokg_ha, cbiokg_ha) %>% 
  group_by(ftrt) %>% 
  dplyr::summarise(cbio_mean = round(mean(cbiokg_ha, na.rm= TRUE),3),
                   cbio_sd = round(sd(cbiokg_ha, na.rm= TRUE),3),
                   wbio_mean = round(mean(wbiokg_ha, na.rm= TRUE),3),
                   wbio_sd = round(sd(wbiokg_ha, na.rm= TRUE),3)
                   
  ) %>% 
  mutate(covercrop = paste(cbio_mean, "\u00b1", cbio_sd),
         wheat = paste(wbio_mean, "\u00b1", wbio_sd)
  ) %>% 
  select(ftrt, wheat, covercrop) %>% 
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer"))

biomass1 %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(biomass1, "biomassftrt.csv", row.names = FALSE)


#group by ctrt summary table for biomass

biomass2 = 
  bio_dat2 %>% 
  select(ftrt, ctrt, time, wbiokg_ha, cbiokg_ha) %>% 
  group_by(ctrt) %>% 
  dplyr::summarise(cbio_mean = round(mean(cbiokg_ha, na.rm= TRUE),3),
                   cbio_sd = round(sd(cbiokg_ha, na.rm= TRUE)),3,
                   wbio_mean = round(mean(wbiokg_ha, na.rm= TRUE),3),
                   wbio_sd = round(sd(wbiokg_ha, na.rm= TRUE),3)
                   
  ) %>% 
  mutate(covercrop = paste(cbio_mean, "\u00b1", cbio_sd),
         wheat = paste(wbio_mean, "\u00b1", wbio_sd)
  ) %>% 
  select(ctrt, wheat, covercrop)



biomass2 %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(biomass2, "biomassctrt.csv", row.names = FALSE)

#stats


#wheat biomass 

#effect of cover crop presence
# wbio_aov <- aov(wbio ~ grouping, data = bio_dat2_grouped)
# summary.aov(wbio_aov)

#effect of cover crop biomass
wbio_cbio <- lme(wbio ~ cbio, random = ~1|ctrt, data = bio_dat2_grouped)
anova(wbio_cbio)

#effect of fertility treatments
# wbio_aov <- aov(wbio ~ ftrt, data = bio_dat2_grouped)
# summary.aov(wbio_aov)

#CN metrics

#effect of fertility (bio_dat2_grouped not formatted for below stats)
# ammftrt_aov <- aov(amm ~ ftrt*grouping, data = bio_dat2_grouped)
# summary.aov(ammftrt_aov)
# 
# nitftrt_aov <- aov(nit ~ ftrt*grouping, data = bio_dat2_grouped)
# summary.aov(nitftrt_aov)
# 
# pmnftrt_aov <- aov(pmn ~ ftrt*grouping, data = bio_dat2_grouped)
# summary.aov(pmnftrt_aov)
# 
# pmcftrt_aov <- aov(pmc ~ ftrt*grouping, data = bio_dat2_grouped)
# summary.aov(pmcftrt_aov)

##############################

#standardized

bio_dat3 =
  bio_dat2 %>%
  filter_all(all_vars(!is.infinite(.)))

wbio2_aov <- aov(wbio_per_cbio ~ ctrt, data = bio_dat3)
summary.aov(wbio2_aov)

pbic2_aov <- aov(pbic_percbio ~ ctrt, data = bio_dat3)
summary.aov(pbic2_aov)

amac2_aov <- aov(amac_percbio ~ ctrt, data = bio_dat3)
summary.aov(amac2_aov)

unavp2_aov <- aov(unavp_percbio ~ ctrt, data = bio_dat3)
summary.aov(unavp2_aov)

porg2_aov <- aov(porg_percbio ~ ctrt, data = bio_dat3)
summary.aov(porg2_aov)

amm2_aov <- aov(amm_percbio ~ ctrt, data = bio_dat3)
summary.aov(amm2_aov)

nit2_aov <- aov(nit_percbio ~ ctrt, data = bio_dat3)
summary.aov(nit2_aov)

nit2_hsd <- HSD.test(nit2_aov, "ctrt")
print(nit2_hsd)


pmc2_aov <- aov(pmc_percbio ~ ctrt, data = bio_dat3)
summary.aov(pmc2_aov)

pmn2_aov <- aov(pmn_percbio ~ ctrt, data = bio_dat3)
summary.aov(pmn2_aov)


###################################


wbio_cbio <- lme(wbio ~ cbio, random = ~1|ctrt, data = bio_dat2_grouped)
anova(wbio_cbio)


# wbio_ctrt <- lme(wbio ~ ctrt, random = ~1|ftrt, data = bio_dat2_grouped)
# anova(wbio_ctrt)
# 
# ctrt_aov <- aov(wbio ~ ctrt*ftrt, data = bio_dat2_grouped)
# summary.aov(ctrt_aov)

# wbioctrt_hsd <- HSD.test(ctrt_aov, "ctrt")
# print(wbioctrt_hsd)
# 
# wbioftrt_hsd <- HSD.test(ctrt_aov, "ftrt")
# print(wbioftrt_hsd)


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(pbic != '.')

pbic_aov <- aov(pbic ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(pbic_aov)

pbicftrt_aov <- aov(pbic ~ ftrt, data = bio_dat2_grouped_filtered)
summary.aov(pbicftrt_aov)


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(amac != '.')

amac_aov <- aov(amac ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(amac_aov)


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(porg != '.')

porg_aov <- aov(porg ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(porg_aov)

bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(unavp != '.')

unavp_aov <- aov(unavp ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(unavp_aov)

######
#CN metrics

##individual cc effect on PMN


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(pmn != '.')

pmn_aov <- aov(pmn ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(pmn_aov)

pmn_hsd <- HSD.test(pmn_aov, "ctrt")
print(pmn_hsd)

### check cc vs fallow

pmng_aov <- aov(pmn ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(pmng_aov)

pmng_hsd <- HSD.test(pmng_aov, "grouping")
print(pmng_hsd)

###

#cc individual effect on PMC

bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(pmc != '.')

pmc_aov <- aov(pmc ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(pmc_aov)

pmc_hsd <- HSD.test(pmc_aov, "ctrt")
print(pmc_hsd)


####

#overall cc effect vs fallow

pmcg_aov <- aov(pmc ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(pmcg_aov)

pmcg_hsd <- HSD.test(pmcg_aov, "grouping")
print(pmcg_hsd)



####


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(nit != '.')

nit_aov <- aov(nit ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(nit_aov)

nit_hsd <- HSD.test(nit_aov, "ctrt")
print(nit_hsd)

bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(amm != '.')

amm_aov <- aov(amm ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(amm_aov)

amm_hsd <- HSD.test(amm_aov, "ctrt")
print(amm_hsd)

bio_dat2_grouped_filtered_cntl =
  bio_dat2_grouped %>% 
  filter(pmn != '.' & ftrt == "CNTL ")

pmn_aov <- aov(pmn ~ ctrt, data = bio_dat2_grouped_filtered_cntl)
summary.aov(pmn_aov)

pmn_hsd <- HSD.test(pmn_aov, "ctrt")
print(pmn_hsd)


bio_dat2_grouped_filtered_ifert =
  bio_dat2_grouped %>% 
  filter(pmn != '.' & ftrt == "IFERT ")

pmn_aov <- aov(pmn ~ ctrt, data = bio_dat2_grouped_filtered_ifert)
summary.aov(pmn_aov)

pmn_hsd <- HSD.test(pmn_aov, "ctrt")
print(pmn_hsd)




#SOM, wheat biomass, and cover crop biomass summary tables.

bio_dat2_grouped = 
  bio_dat2_grouped %>% 
  dplyr::mutate(amm = as.numeric(amm),
                nit = as.numeric(nit),
                pmn = as.numeric(pmn),
                pmc = as.numeric(pmc),
                cbio = as.numeric(cbio),
                wbio = as.numeric(wbio),
  )

SOM_sd = 
  bio_dat2_grouped %>% 
  select(ctrt, nit, amm, pmn, pmc) %>% 
  group_by(ctrt) %>% 
  dplyr::summarise(amm_mean = round(mean(amm, na.rm= TRUE)),
                   amm_se = round(sd(amm, na.rm= TRUE)/sqrt(n()),3),
                   nit_mean = round(mean(nit, na.rm= TRUE)),
                   nit_se = round(sd(nit, na.rm= TRUE)/sqrt(n())),
                   pmc_mean = round(mean(pmc, na.rm= TRUE)),
                   pmc_se = round(sd(pmc, na.rm= TRUE)/sqrt(n())),
                   pmn_mean = round(mean(pmn, na.rm= TRUE)),
                   pmn_se = round(sd(pmn, na.rm= TRUE)/sqrt(n())),
  ) 


SOM_sd_standardized = 
  bio_dat2_grouped %>%
  filter_all(all_vars(!is.infinite(.)))%>% 
  select(ctrt, time, nit_percbio, amm_percbio, pmn_percbio, pmc_percbio) %>% 
  group_by(ctrt) %>% 
  dplyr::summarise(nit_mean = round(mean(nit_percbio, na.rm= TRUE)),
                   nit_se = round(sd(nit_percbio, na.rm= TRUE)/sqrt(n())),
                   amm_mean = round(mean(amm_percbio, na.rm= TRUE)),
                   amm_se = round(sd(amm_percbio, na.rm= TRUE)/sqrt(n())),
                   pmc_mean = round(mean(pmc_percbio, na.rm= TRUE)),
                   pmc_se = round(sd(pmc_percbio, na.rm= TRUE)/sqrt(n())),
                   pmn_mean = round(mean(pmn_percbio, na.rm= TRUE)),
                   pmn_se = round(sd(pmn_percbio, na.rm= TRUE)/sqrt(n())),
  ) 



biomass = 
  bio_dat2_grouped %>% 
  select(ctrt, time, wbio, cbio) %>% 
  group_by(ctrt) %>% 
  dplyr::summarise(wbio_mean = round(mean(wbio, na.rm= TRUE)),
                   wbio_se = round(sd(wbio, na.rm= TRUE)/sqrt(n())),
                   cbio_mean = round(mean(cbio, na.rm= TRUE)),
                   cbio_se = round(sd(cbio, na.rm= TRUE)/sqrt(n()))
  ) %>% 
  mutate(wheat = paste(wbio_mean, "\u00b1", wbio_se),
         covercrop = paste(cbio_mean, "\u00b1", cbio_se)) 
# select(ctrt, wheat, covercrop) %>% 
# mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
#                      "CNTL " = "Control",
#                      'IFERT ' = "Inorganic Fertilizer"))

# this will also add " NA" for the blank cells
# use str_remove to remove the string

biomass %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(biomass, "biomass.csv", row.names = FALSE)

#############


biomass = 
  bio_dat2_grouped %>% 
  select(ctrt, time, wbio, cbio) %>% 
  group_by(ctrt) %>% 
  dplyr::summarise(wbio_mean = round(mean(wbio, na.rm= TRUE)),
                   wbio_se = round(sd(wbio, na.rm= TRUE)/sqrt(n())),
                   cbio_mean = round(mean(cbio, na.rm= TRUE)),
                   cbio_se = round(sd(cbio, na.rm= TRUE)/sqrt(n()))
  ) %>% 
  mutate(wheat = paste(wbio_mean, "\u00b1", wbio_se),
         covercrop = paste(cbio_mean, "\u00b1", cbio_se)) 
# select(ctrt, wheat, covercrop) %>% 
# mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
#                      "CNTL " = "Control",
#                      'IFERT ' = "Inorganic Fertilizer"))

# this will also add " NA" for the blank cells
# use str_remove to remove the string

biomass %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(biomass, "biomass.csv", row.names = FALSE)

#pH table

ph_dat_groupedforstats =
  ph_dat %>% 
  dplyr::mutate(grouping = if_else(ctrt == "Fallow", "Fallow", "Cover Crop")) %>% 
  group_by(ctrt) %>% 
  dplyr::summarise(ph_mean = round(mean(sph2), 2),
                   ph_se = round(sd(sph2)/sqrt(n()),2),
                   inorgcarbon_mean = round(mean(inorgcarbon), 2),
                   inorgcarbon_se = round(sd(inorgcarbon)/sqrt(n()),2)) %>% 
  mutate(pH = paste(ph_mean, "\u00b1", ph_se),
         inorganiccarbon = paste(inorgcarbon_mean, "\u00b1", inorgcarbon_se)) 

ph_dat_groupedforstats %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(ph_dat_groupedforstats, "output/ph.csv", row.names = FALSE)

###
#pH stats

ph_aov <- aov(sph2 ~ ftrt*ctrt, data = ph_dat)
summary.aov(ph_aov)

ph_hsd <- HSD.test(ph_aov, "ctrt")
print(ph_hsd)

inorgc_aov <- aov(inorgcarbon ~ ftrt*ctrt, data = ph_dat)
summary.aov(inorgc_aov)

inorgc_hsd <- HSD.test(inorgc_aov, "ctrt")
print(inorgc_hsd)

inorgcf_hsd <- HSD.test(inorgc_aov, "ftrt")
print(inorgcf_hsd)



############ 
#Available P pbic 

pbic =
  bio_dat2_grouped %>% 
  group_by(ftrt, time) %>% 
  dplyr::summarise(pbic_mean = round(mean(pbic), 2),
                   pbic_se = round(sd(pbic)/sqrt(n()),2)) %>% 
  mutate(pbic = paste(pbic_mean, "\u00b1", pbic_se)) 

pbic %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(pbic, "output/pbic_ftrt_table.csv", row.names = FALSE)

pbicstat_aov <- aov(pbic ~ ctrt*ftrt, data = bio_dat2_grouped)
summary.aov(pbicstat_aov)

pbicstat_hsd <- HSD.test(pbicstat_aov, "ftrt")
print(pbicstat_hsd)


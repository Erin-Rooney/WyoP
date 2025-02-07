#Greenhouse incubation
#E Rooney
#7 19 2021

#contains SOM fig 
#pH
#early cc vs wheat biomass figure (not included in manuscript)

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


#ggplots-------

#not used cc vs wheat biomass

bio_dat2 %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot(aes(x = as.numeric(cbiokg_ha), y = wbiokg_ha))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
  stat_regline_equation(label.y = 6,label.x = 60, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 5.5, label.x = 60, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "cover crop biomass, grams/pot")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
  facet_grid(.~ftrt)+
  theme_er()

###

#pH fig

ph_dat_grouped %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot(aes(x = ctrt, y = ph_mean, fill = ftrt))+
  geom_bar(stat= "identity", position = "dodge")+
    labs(y = "pH")+
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+

  #facet_wrap(.~ctrt)+
  theme_er()

print(ph_dat_grouped)

ph_dat_grouped %>% knitr::kable()

write.csv(ph_dat_grouped, "output/ph_dat_grouped.csv")

#

#all together cc vs wheat biomass, not included in manuscript

bio_dat2 %>%
  filter(ctrt != 'Fallow') %>% 
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot(aes(x = as.numeric(cbiokg_ha), y = wbiokg_ha))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
  stat_regline_equation(label.y = 6,label.x = 60, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 5.5, label.x = 60, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, kg/ha",
       x = "cover crop biomass, kg/ha")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
  #facet_grid(.~ctrt)+
  theme_er()
#
# 
# bio_dat2_grouped %>%
#   filter(ctrt != 'Fallow') %>% 
#   mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
#                        'CNTL ' = "Control",
#                        'IFERT ' = "Inorganic Fertilizer")) %>% 
#   ggplot(aes(x = as.numeric(cbiokg_ha), y = wbiokg_ha))+
#   geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 5, alpha = 0.7)+
#   geom_smooth(method = "lm", se = FALSE, group = 'sample')+
#   stat_regline_equation(label.y = 50,label.x = 500, aes(label = ..eq.label..)) +
#   stat_regline_equation(label.y = 45, label.x = 500, aes(label = ..rr.label..)) +
#   stat_fit_glance(method = 'lm',
#                   method.args = list(formula = formula),
#                   geom = 'text',
#                   aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
#                   label.x = 15, label.y = 3.5, size = 4)+
#   labs(y = "wheat biomass, kg/ha",
#        x = "cover crop biomass, kg/ha")+
#   #scale_color_manual(values = mycolors)+
#   #scale_color_manual(values = pnw_palette('Shuksan',9))+
#   scale_fill_manual(values = pnw_palette('Shuksan2',9))+
#   #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
#   scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
#   facet_grid(.~ftrt)+
#   theme_er()+
#   theme(legend.position = "NONE")

#

####

#wheat bio vs cc bio August 25 2022
#kg per hectare
  
library(NatParksPalettes)

allcc_bio = 
  bio_dat2_grouped %>%
  dplyr::mutate(cbio = as.numeric(cbio)) %>% 
   ggplot(aes(x = cbio, y = wbio))+
  geom_errorbar(aes(xmin=cbio-cbio_se, xmax=cbio+cbio_se), color = "grey40")+
  geom_point(aes(fill = ctrt, color = ctrt), size = 4.5)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
  stat_regline_equation(label.y = 21,label.x = 25, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 20, label.x = 25, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 4)+
  labs(y = "wheat biomass, kg/ha",
       x = "cover crop biomass, kg/ha",
       fill = "", shape = "", color = "")+
  ylim(0,25)+
  scale_color_manual(values = natparks.pals(name = "Olympic", 9))+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+
  #scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
  theme_er()+
  theme(legend.position = "right")

ggsave("output/allccbio.tiff", plot = allcc_bio, height = 4.5, width = 6)

#
biomass_fig_individual_nolines =
  bio_dat2 %>%
  #filter(ftrt == "CNTL " & ctrt %in% c("Oat", "All Mixture", "Faba Bean Oat", "Faba Bean")) %>% 
  filter(ftrt %in% "CNTL " & ctrt != "Faba Bean") %>% 
  ggplot(aes(x = cbiokg_ha, y = wbiokg_ha))+
  geom_point(aes(fill = ctrt), color = "black", size = 4, alpha = 0.5, shape = c(21))+
  # geom_smooth(method = "lm", se = FALSE)+
  # stat_regline_equation(label.y = 7,label.x = 125, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 2, label.x = 125, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 2.5)+
  labs(y = "wheat biomass, kg per hectare",
       x = "cover crop biomass, kg per hectare")+
  scale_color_manual(values = natparks.pals(name = "Olympic", 9))+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+
  facet_wrap(.~ctrt)+
  ylim(0,40)+
  theme_er()+
  theme(legend.position = "None", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/biomass_individual_nolines.tiff", plot = biomass_fig_individual_nolines, height = 6.75, width = 6.25)

biomass_fig_individual_fababean =
  bio_dat2 %>%
  #filter(ftrt == "CNTL " & ctrt %in% c("Oat", "All Mixture", "Faba Bean Oat", "Faba Bean")) %>% 
  filter(ftrt %in% "CNTL " & ctrt %in% "Faba Bean") %>% 
  ggplot(aes(x = cbiokg_ha, y = wbiokg_ha))+
  geom_point(aes(fill = ctrt), color = "black", size = 4, alpha = 0.5, shape = c(21))+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 7,label.x = 125, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 2, label.x = 125, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 2.5)+
  labs(y = "wheat biomass, kg per hectare",
       x = "cover crop biomass, kg per hectare")+
  scale_color_manual(values = natparks.pals(name = "Olympic", 9))+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+
  facet_wrap(.~ctrt)+
  ylim(0,40)+
  theme_er()+
  theme(legend.position = "None", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/biomass_individual_fababean.tiff", plot = biomass_fig_individual_fababean, height = 4, width = 4)



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





bio_dat3 =
  bio_dat2 %>%
  filter_all(all_vars(!is.infinite(.)))

######
#CN metrics


#SOM, wheat biomass, and cover crop biomass summary tables.

bio_dat2_grouped_numeric = 
  bio_dat2_grouped %>% 
  dplyr::mutate(amm = as.numeric(amm),
                nit = as.numeric(nit),
                pmn = as.numeric(pmn),
                pmc = as.numeric(pmc),
                cbio = as.numeric(cbio),
                wbio = as.numeric(wbio),
  )


bio_dat2_numeric = 
  bio_dat2 %>% 
  dplyr::mutate(amm = as.numeric(amm),
                nit = as.numeric(nit),
                pmn = as.numeric(pmn),
                pmc = as.numeric(pmc),
                cbio = as.numeric(cbio),
                wbio = as.numeric(wbio),
  )


SOM_sd_grouped = 
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

SOM_sd = 
  bio_dat2 %>% 
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




a = 
  SOM_sd %>%
  ggplot()+
  geom_bar(aes(x = ctrt, y = nit_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.4, color = "black")+
  geom_errorbar(aes(x = ctrt, ymin = nit_mean - nit_se, ymax = nit_mean + nit_se), width = .2,
                position = position_dodge(.9), color = 'black')+
  labs(y = "Nitrate, mg/kg",
       x = " ")+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+ 
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90),
        legend.position = "none")+
  annotate("text", x = 1, y = 32, label = "bc")+
  annotate("text", x = 2, y = 29, label = "bc")+
  annotate("text", x = 3, y = 24, label = "c")+
  annotate("text", x = 4, y = 49, label = "a")+
  annotate("text", x = 5, y = 42, label = "ab")+
  annotate("text", x = 6, y = 25, label = "bc")+
  annotate("text", x = 7, y = 28, label = "bc")+
  annotate("text", x = 8, y = 19, label = "c")+
  annotate("text", x = 9, y = 30, label = "bc")


b = SOM_sd %>%
  #filter(ctrt != "Fallow") %>% 
  # mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
  #                      "CNTL " = "Control",
  #                      'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = pmn_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.4, color = "black")+
  geom_errorbar(aes(x = ctrt, ymin = pmn_mean - pmn_se, ymax = pmn_mean + pmn_se), width = .2,
                position = position_dodge(.9), color = 'black')+
  labs(y = "PMN, mg/kg",
       x = " ")+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+ 
  #facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90),
        legend.position = "none")+
  annotate("text", x = 1, y = 20, label = "ab")+
  annotate("text", x = 2, y = 15, label = "ab")+
  annotate("text", x = 3, y = 17, label = "ab")+
  annotate("text", x = 4, y = 30, label = "a")+
  annotate("text", x = 5, y = 22, label = "ab")+
  annotate("text", x = 6, y = 23, label = "ab")+
  annotate("text", x = 7, y = 15, label = "b")+
  annotate("text", x = 8, y = 19, label = "ab")+
  annotate("text", x = 9, y = 12, label = "b")+
  NULL

c = SOM_sd %>%
  #filter(ctrt != "Fallow") %>% 
  # mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
  #                      "CNTL " = "Control",
  #                      'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = pmc_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.4, color = "black")+
  geom_errorbar(aes(x = ctrt, ymin = pmc_mean - pmc_se, ymax = pmc_mean + pmc_se), width = .2,
                position = position_dodge(.9), color = 'black')+
  labs(y = "PMC, mg/kg",
       x = " ")+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+ 
  #facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90),
        legend.position = "none")+
  annotate("text", x = 1, y = 175, label = "a")+
  annotate("text", x = 2, y = 160, label = "ab")+
  annotate("text", x = 3, y = 140, label = "ab")+
  annotate("text", x = 4, y = 144, label = "ab")+
  annotate("text", x = 5, y = 140, label = "ab")+
  annotate("text", x = 6, y = 130, label = "ab")+
  annotate("text", x = 7, y = 110, label = "b")+
  annotate("text", x = 8, y = 125, label = "ab")+
  annotate("text", x = 9, y = 100, label = "b")+
  NULL


d = SOM_sd %>%
  #filter(ctrt != "Fallow") %>% 
  # mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
  #                      "CNTL " = "Control",
  #                      'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = amm_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.4, color = "black")+
  geom_errorbar(aes(x = ctrt, ymin = amm_mean - amm_se, ymax = amm_mean + amm_se), width = .2,
                position = position_dodge(.9), color = 'black')+
  labs(y = "Ammonium, mg/kg",
       x = " ")+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+ 
  #facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90),
        legend.position = "none")+
  # annotate("text", x = 1, y = 4, label = "a")+
  # annotate("text", x = 2, y = 5, label = "a")+
  # annotate("text", x = 3, y = 4, label = "a")+
  # annotate("text", x = 4, y = 5, label = "a")+
  # annotate("text", x = 5, y = 5, label = "a")+
  # annotate("text", x = 6, y = 4, label = "a")+
  # annotate("text", x = 7, y = 4, label = "a")+
  # annotate("text", x = 8, y = 4, label = "a")+
  # annotate("text", x = 9, y = 4, label = "a")+
  NULL


library(patchwork)
library(cowplot)

SOM_sd_fig = a+b+c+d #combines the two plots
#plot_layout(guides = "collect") # sets a common legend

ggsave("output/SOM_sd_fig.tiff", plot = SOM_sd_fig, height = 6, width = 5)



#########standardized

#SOM, wheat biomass, and cover crop biomass summary tables.





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

###figure time

ph_dat_groupedforstats %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = ph_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.4, color = "gray20")+
  geom_errorbar(aes(x = ctrt, ymin = ph_mean - ph_se, ymax = ph_mean + ph_se), width = .2,
                position = position_dodge(.9), color = 'gray50')+
  annotate("text", x = 1, y = 9, label = "c")+
  annotate("text", x = 2, y = 9, label = "b")+
  annotate("text", x = 3, y = 9, label = "ab")+
  annotate("text", x = 4, y = 9, label = "b")+
  annotate("text", x = 5, y = 9, label = "b")+
  annotate("text", x = 6, y = 9, label = "b")+
  annotate("text", x = 7, y = 9, label = "ab")+
  annotate("text", x = 8, y = 9, label = "a")+
  annotate("text", x = 9, y = 9, label = "ab")+
  labs(y = "pH",
       x = " ")+
  ylim(0, 10)+
  scale_fill_manual(values = pnw_palette('Lake',9))+
  theme_er()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90), legend.position = "NONE")


###
SOM_sd_standardized %>%
  #filter(ftrt == "CNTL ") %>% 
  # mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
  #                      "CNTL " = "Control",
  #                      'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = nit_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.7, color = "gray50")+
  geom_errorbar(aes(x = ctrt, ymin = nit_mean - nit_se, ymax = nit_mean + nit_se), width = .2,
                position = position_dodge(.9), color = 'gray50')+
  labs(y = "Nitrate standardized, mg/kg",
       x = " ")+
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  #facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

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


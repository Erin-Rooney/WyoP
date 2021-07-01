#Greenhouse incubation
#E Rooney
#5 19 2021

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
  dplyr::mutate(wbio_per_cbio = (wbio/cbio)) %>% 
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



bio_dat2_grouped =
  bio_dat2 %>%
  dplyr::mutate(grouping = if_else(ctrt == "Fallow", "Fallow", "Cover Crop")) 


#ggplots-------

bio_dat2 %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
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

#

bio_dat2 %>%
  filter(ctrt %in% c("Oat")) %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 4.75,label.x = 5, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 4.25, label.x = 5, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "cover crop biomass, grams/pot")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  #scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  scale_fill_manual(values = c("#d7b1c5"))+
  scale_shape_manual(values = c(22))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "None")

#

bio_dat2 %>%
  filter(ftrt == "CNTL " & ctrt %in% c("Oat", "All Mixture", "Faba Bean Oat", "Faba Bean")) %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 5.75,label.x = 1, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 5.25, label.x = 1, aes(label = ..rr.label..)) +
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
  facet_wrap(.~ctrt)+
  ylim(0,6)+
  theme_er()+
  theme(legend.position = "None")

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

biomass = 
  bio_dat2 %>% 
  select(ftrt, ctrt, time, wbio, cbio) %>% 
  group_by(ftrt, ctrt) %>% 
  dplyr::summarise(cbio_mean = round(mean(cbio, na.rm= TRUE)),
                   cbio_sd = round(sd(cbio, na.rm= TRUE)),
                   wbio_mean = round(mean(wbio, na.rm= TRUE)),
                   wbio_sd = round(sd(wbio, na.rm= TRUE))
                   
  ) %>% 
  mutate(covercrop = paste(cbio_mean, "\u00b1", cbio_sd),
         wheat = paste(wbio_mean, "\u00b1", wbio_sd)
  ) %>% 
  select(ftrt, ctrt, wheat, covercrop) %>% 
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer"))


biomass %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(biomass, "biomass2.csv", row.names = FALSE)

#stats



wbio_aov <- aov(wbio ~ grouping, data = bio_dat2_grouped)
summary.aov(wbio_aov)


wbio_cbio <- lme(wbio ~ cbio, random = ~1|ctrt, data = bio_dat2_grouped)
anova(wbio_cbio)

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


pmc2_aov <- aov(pmc_percbio ~ ctrt, data = bio_dat3)
summary.aov(pmc2_aov)

pmn2_aov <- aov(pmn_percbio ~ ctrt, data = bio_dat3)
summary.aov(pmn2_aov)

###################################


wbio_cbio <- lme(wbio ~ cbio, random = ~1|ctrt, data = bio_dat2_grouped)
anova(wbio_cbio)


wbio_ctrt <- lme(wbio ~ ctrt, random = ~1|ftrt, data = bio_dat2_grouped)
anova(wbio_ctrt)

ctrt_aov <- aov(wbio ~ ctrt*ftrt, data = bio_dat2_grouped)
summary.aov(ctrt_aov)

wbioctrt_hsd <- HSD.test(ctrt_aov, "ctrt")
print(wbioctrt_hsd)

wbioftrt_hsd <- HSD.test(ctrt_aov, "ftrt")
print(wbioftrt_hsd)


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(pbic != '.')

pbic_aov <- aov(pbic ~ grouping, data = bio_dat2_grouped_filtered)
summary.aov(pbic_aov)


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


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(pmn != '.')

pmn_aov <- aov(pmn ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(pmn_aov)

pmn_hsd <- HSD.test(pmn_aov, "ctrt")
print(pmn_hsd)


bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(pmc != '.')

pmc_aov <- aov(pmc ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(pmc_aov)



bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(nit != '.')

nit_aov <- aov(nit ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(nit_aov)

bio_dat2_grouped_filtered =
  bio_dat2_grouped %>% 
  filter(amm != '.')

amm_aov <- aov(amm ~ ctrt, data = bio_dat2_grouped_filtered)
summary.aov(amm_aov)


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
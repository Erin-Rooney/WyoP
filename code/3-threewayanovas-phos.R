### 3 Factor ANOVAs for all SOM parameters

#phos_pools_fig

#load packages----------------
source("code/0-packages.R")

#data

pall = read.csv("raw/allfert_allcrop_P.csv")

pall2 = 
  pall %>% 
  mutate(ctrt = recode(ctrt, "ALL " = 'All Mixture',
                       "B" = 'Buckwheat',
                       "BO" = 'Buckwheat Oat',
                       "F" = "Faba Bean",
                       "FLW" = "Fallow",
                       "FO" = "Faba Bean Oat",
                       "O" = 'Oat',
                       "R" = 'Radish',
                       "RO" = 'Radish Oat'),
         ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
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
                pmc_percbio = (pmc/cbio),
                wbio_percbio = (wbio/cbio)) 


pall3_t1 = 
  pall2 %>% 
  na.omit()

#standardized 3 way anovas-----------

pall3 =
  pall2 %>%
  select(ftrt, ctrt, time, wbio, cbio, pbic_percbio, amac_percbio, unavp_percbio,
         porg_percbio, amm_percbio, nit_percbio, pmn_percbio, pmc_percbio, wbio_percbio) %>% 
  NaRV.omit() %>% 
  filter(ctrt != "Fallow")

amm_aov <- aov(amm_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(amm_aov)

nit_aov <- aov(nit_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(nit_aov)

pmn_aov <- aov(pmn_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(pmn_aov)

pmc_aov <- aov(pmc_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(pmc_aov)


wbio_aov <- aov(wbio_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(wbio_aov)

pbic_aov <- aov(pbic_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(pbic_aov)

amac_aov <- aov(amac_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(amac_aov)

unavp_aov <- aov(unavp_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(unavp_aov)

porg_aov <- aov(porg_percbio ~ ftrt*ctrt*time, data = pall3)
summary.aov(porg_aov)


#not standardized + fallow----------

amm2_aov <- aov(amm ~ ftrt*ctrt*time, data = pall2)
summary.aov(amm2_aov)

nit2_aov <- aov(nit ~ ftrt*ctrt*time, data = pall2)
summary.aov(nit2_aov)

pmn2_aov <- aov(pmn ~ ftrt*ctrt*time, data = pall2)
summary.aov(pmn2_aov)

pmc2_aov <- aov(pmc ~ ftrt*ctrt*time, data = pall2)
summary.aov(pmc2_aov)


wbio2_aov <- aov(wbio ~ ftrt*ctrt*time, data = pall2)
summary.aov(wbio2_aov)

pbic2_aov <- aov(pbic ~ ftrt*ctrt*time, data = pall2)
summary.aov(pbic2_aov)

amac2_aov <- aov(amac ~ ftrt*ctrt*time, data = pall2)
summary.aov(amac2_aov)

amac2_hsd = HSD.test(amac2_aov, "ftrt")
print(amac2_hsd)


#hsds separated by time--------------

unavp2_aov <- aov(unavp ~ ftrt*ctrt*time, data = pall2)
summary.aov(unavp2_aov)

unavp2_hsd = HSD.test(unavp2_aov, "ftrt")
print(unavp2_hsd)

unavp2_hsd = HSD.test(unavp2_aov, "ctrt")
print(unavp2_hsd)


porg2_aov <- aov(porg ~ ftrt*ctrt*time, data = pall2)
summary.aov(porg2_aov)

porg2_hsd = HSD.test(porg2_aov, "ftrt")
print(porg2_hsd)

porg2_hsd = HSD.test(porg2_aov, "ctrt")
print(porg2_hsd)

pall2_time1 = pall2 %>% filter(time == 1)
pall2_time2 = pall2 %>% filter(time == 2)
 
#Time 1

porg_time1_aov <- aov(porg ~ ftrt*ctrt, data = pall2_time1)
summary.aov(porg_time1_aov)

porg_time1_hsd_ftrt = HSD.test(porg_time1_aov, "ftrt")
print(porg_time1_hsd_ftrt)

porg_time1_hsd_ctrt = HSD.test(porg_time1_aov, "ctrt")
print(porg_time1_hsd_ctrt)

pall2_time1_compost = pall2 %>% filter(time == 1 & ftrt == "Compost") %>% select(ctrt, ftrt, unavp) %>% na.omit()

unavp_time1_aov <- aov(unavp ~ ctrt, data = pall2_time1_compost)
summary.aov(unavp_time1_aov)

unavp_time1_hsd = HSD.test(unavp_time1_aov, "ctrt")
print(unavp_time1_hsd)


pall2_time1_control = pall2 %>% filter(time == 1 & ftrt == "Control") %>% select(ctrt, ftrt, unavp) %>% na.omit()

unavp_time1_aov <- aov(unavp ~ ctrt, data = pall2_time1_control)
summary.aov(unavp_time1_aov)

unavp_time1_hsd = HSD.test(unavp_time1_aov, "ctrt")
print(unavp_time1_hsd)


#Time 2

porg_time2_aov <- aov(porg ~ ftrt*ctrt, data = pall2_time2)
summary.aov(porg_time2_aov)

porg_time2_hsd_ftrt = HSD.test(porg_time2_aov, "ftrt")
print(porg_time2_hsd_ftrt)

porg_time2_hsd_ctrt = HSD.test(porg_time2_aov, "ctrt")
print(porg_time2_hsd_ctrt)






#######individual hsd within each ftrt to test differences in ctrt for figures-----------

unavpcompost_aov <- aov(unavp ~ ctrt, data = pall2 %>% filter(ftrt == "Compost"))
summary.aov(unavpcompost_aov)

unavpcompost_hsd = HSD.test(unavpcompost_aov, "ctrt")
print(unavpcompost_hsd)


unavpcontrol_aov <- aov(unavp ~ ctrt, data = pall2 %>% filter(ftrt == "Control"))
summary.aov(unavpcontrol_aov)

unavpcontrol_hsd = HSD.test(unavpcontrol_aov, "ctrt")
print(unavpcontrol_hsd)



unavpifert_aov <- aov(unavp ~ ctrt, data = pall2 %>% filter(ftrt == "Inorganic Fertilizer"))
summary.aov(unavpifert_aov)

unavpifert_hsd = HSD.test(unavpifert_aov, "ctrt")
print(unavpifert_hsd)



###------


porgcompost_aov <- aov(porg ~ ctrt, data = pall2 %>% filter(ftrt == "Compost"))
summary.aov(porgcompost_aov)

porgcompost_hsd = HSD.test(porgcompost_aov, "ctrt")
print(porgcompost_hsd)


porgcontrol_aov <- aov(porg ~ ctrt, data = pall2 %>% filter(ftrt == "Control"))
summary.aov(porgcontrol_aov)

porgcontrol_hsd = HSD.test(porgcontrol_aov, "ctrt")
print(porgcontrol_hsd)



porgifert_aov <- aov(porg ~ ctrt, data = pall2 %>% filter(ftrt == "Inorganic Fertilizer"))
summary.aov(porgifert_aov)

porgifert_hsd = HSD.test(porgifert_aov, "ctrt")

print(porgifert_hsd)




#not standardized minus fallow------------

pall2b =
  pall2 %>%
  filter(ctrt != "Fallow")

amm3_aov <- aov(amm ~ ftrt*ctrt*time, data = pall2b)
summary.aov(amm3_aov)

nit3_aov <- aov(nit ~ ftrt*ctrt*time, data = pall2b)
summary.aov(nit3_aov)

pmn3_aov <- aov(pmn ~ ftrt*ctrt*time, data = pall2b)
summary.aov(pmn3_aov)

pmc3_aov <- aov(pmc ~ ftrt*ctrt*time, data = pall2b)
summary.aov(pmc3_aov)


wbio3_aov <- aov(wbio ~ ftrt*ctrt*time, data = pall2b)
summary.aov(wbio3_aov)

pbic3_aov <- aov(pbic ~ ftrt*ctrt*time, data = pall2b)
summary.aov(pbic3_aov)

amac3_aov <- aov(amac ~ ftrt*ctrt*time, data = pall2b)
summary.aov(amac3_aov)

unavp3_aov <- aov(unavp ~ ftrt*ctrt*time, data = pall2b)
summary.aov(unavp3_aov)

porg3_aov <- aov(porg ~ ftrt*ctrt*time, data = pall2b)
summary.aov(porg3_aov)


#######################

pall2 %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ctrt, shape = ftrt), size = 3, alpha = 0.8)+
  labs(title = "By Covercrop")+
  #scale_color_manual(values = wes_palette("Darjeeling2", 9))+
  theme_er()+
  NULL


pall3 = 
  pall2 %>% 
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
)

#not used in manuscript
  
pall3 %>%
  filter(ctrt != "Fallow") %>% 
  ggplot(aes(x = cbio, y = pbic, group = ctrt))+
  geom_point(aes(color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  #geom_smooth(method = "lm", se = FALSE, group = 'ctrt')+
  #stat_regline_equation(label.y = 47,label.x = 50, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 45, label.x = 50, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL


#not used in manuscript--------------

pall2 %>%
  filter(ctrt != "Fallow" & time == "1") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL


pall2 %>%
  filter(ctrt != "Fallow" & time == "2") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

#not used in manuscript

pall2 %>%
  filter(ctrt != "Fallow" & time == "1") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = amac, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "reserve P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

#not used in manuscript

pall2 %>%
  filter(ctrt != "Fallow" & time == "2") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = amac, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "reserve P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL


#not used in manuscript

pall2 %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = porg, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "organic P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

#not used in manuscript

pall2 %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = amac, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "reserve P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

#not used in manuscript

pall2 %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = unavp, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "unavailable P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

######
#new ggplot figs Aug 25 2022---------------

pall_longer =
  pall2 %>% 
  select(time, ctrt, ftrt, amac, porg, unavp) %>% 
  na.omit() %>% 
  #is this where I need to summarize? pivot longer then pivot wider again?
  pivot_longer(-c(time, ctrt, ftrt), 
               names_to = 'phosphorus_pool', values_to = 'concentration') %>% 
  group_by(ftrt, ctrt, time, phosphorus_pool) %>%
  dplyr::summarise(p_concentration = round(mean(concentration), 2),
                   se = round(sd(concentration)/sqrt(n()),2)) %>% 
  mutate(ctrt = factor(ctrt, levels = c('All Mixture', 'Buckwheat Oat', 'Buckwheat', 
                                        'Faba Bean Oat', "Faba Bean", 'Radish Oat', 
                                        "Radish", 'Oat', 'Fallow'))) %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "amac" = 'reserve P',
                                  "porg" = "organic P",
                                  "unavp" = "fixed P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('organic P', 'reserve P', 'fixed P')))

#p concentrations are absolute 
#this is literally the only figure from this script so far that is actually included in the manuscript

library(tibble)

gglabel = tribble(
  ~ctrt, ~x, ~y, ~label,
  "Faba Bean Oat", 'Compost', 79, "A",
  "Faba Bean Oat", 'Control', 58, "B",
  "Faba Bean Oat", 'Inorganic Fertilizer', 58, "B")

P_reserve_fig =
  pall_longer %>%
  filter(phosphorus_pool == "reserve P" & ctrt == "Faba Bean Oat") %>% 
  mutate(ftrt = factor(ftrt, levels = c('Compost', 'Inorganic Fertilizer', 'Control'))) %>% 
  ggplot()+
  geom_bar(aes(x = ftrt, y = p_concentration, fill = ftrt), alpha = 0.9, stat = 'identity', position = "dodge", color = "black")+
  geom_errorbar(aes(x = ftrt, ymin = p_concentration - se, ymax = p_concentration + se), 
                width = .2, position = position_dodge(.9), color = 'black')+
  geom_text(data = gglabel, aes(x = x, y = y, label = label), color = 'black', size = 5)+
  labs(y = "Reserve P, mg/kg", x = "")+
  scale_fill_manual(values = pnw_palette('Starfish',3))+
  # geom_text(data = gglabel, aes(x = x, y = y, label = label), color = "black", size = 3, position_dodge(width = 0.2), 
  #            stat = 'identity')+
  ylim(0,100)+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "NONE")+
  #theme(legend.position = "NONE", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  NULL
  
ggsave("output/phos_reserve_fig.tiff", plot = P_reserve_fig, height = 5, width = 4.5)

library(NatParksPalettes)

P_pools_fig_rotation1 =
  pall_longer %>%
  filter(phosphorus_pool != "reserve P" & time == "1") %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "fixed P" = "fixed P (mg/kg)",
                                  "organic P" = "organic P (mg/kg)")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = p_concentration, fill = ctrt), alpha = 0.5, stat = 'identity', position = "dodge", color = "black")+
  geom_errorbar(aes(x = ctrt, ymin = p_concentration - se, ymax = p_concentration + se), 
                width = .2, position = position_dodge(.9), color = 'black')+
  labs(y = " ", x = "")+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+ 
  facet_grid(phosphorus_pool~ftrt, switch = "y")+
  theme_er()+
  theme(legend.position = "NONE", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside")+
  NULL

P_pools_fig_rotation2 =
  pall_longer %>%
  filter(phosphorus_pool != "reserve P" & time == "2") %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "fixed P" = "fixed P (mg/kg)",
                                  "organic P" = "organic P (mg/kg)")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = p_concentration, fill = ctrt), alpha = 0.5, stat = 'identity', position = "dodge", color = "black")+
  geom_errorbar(aes(x = ctrt, ymin = p_concentration - se, ymax = p_concentration + se), 
                width = .2, position = position_dodge(.9), color = 'black')+
  labs(y = " ", x = "")+
  ylim(0, 500)+
  scale_fill_manual(values = natparks.pals(name = "Olympic", 9))+ 
  facet_grid(phosphorus_pool~ftrt, switch = "y")+
  theme_er()+
  theme(legend.position = "NONE", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside")+
  NULL

ggsave("output/phos_pools_fig1.tiff", plot = P_pools_fig_rotation1, height = 6, width = 7)
ggsave("output/phos_pools_fig2.tiff", plot = P_pools_fig_rotation2, height = 6, width = 7)



#############

#hsd summary table for available P (absolute) group by cover crop

## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

pall_ftrt_p_summarized =
  pall2 %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  na.omit() %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to = 'phos_pool', values_to = "abund") %>% 
  group_by(ctrt, ftrt, time, phos_pool) %>%
  dplyr::summarise(abundance = round(mean(abund), 2),
                   se = round(sd(abund)/sqrt(n()),2))

pall_ftrt_p_sample =
  pall2 %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  na.omit() %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to = 'phos_pool', values_to = "abund")


pall_ctrt_p_sample =
  pall2 %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  na.omit() %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to = 'phos_pool', values_to = "abund")

table_ftrt = 
  pall_ftrt_p_summarized %>% 
  mutate(summary = paste(abundance, "\u00b1", se)) %>% 
  dplyr::select(-abundance, -se)

######

#same as above but this time, time is combined for manuscript results section

pall_ftrt_p_summarized_amac =
  pall2 %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  na.omit() %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to = 'phos_pool', values_to = "abund") %>% 
  group_by(ctrt, ftrt, phos_pool) %>%
  dplyr::summarise(abundance = round(mean(abund), 2),
                   se = round(sd(abund)/sqrt(n()),2))


table_ftrt_amac = 
  pall_ftrt_p_summarized_amac %>% 
  mutate(summary = paste(abundance, "\u00b1", se)) %>% 
  dplyr::select(-abundance, -se)

amac_only =
  table_ftrt_amac %>% 
  filter(phos_pool == "amac")

#######

#same as above but this time, time is combined for manuscript results section, for PORG

pall_ftrt_p_summarized_porg =
  pall2 %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  na.omit() %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to = 'phos_pool', values_to = "abund") %>% 
  group_by(ctrt, ftrt, phos_pool) %>%
  dplyr::summarise(abundance = round(mean(abund), 2),
                   se = round(sd(abund)/sqrt(n()),2))


table_ftrt_porg = 
  pall_ftrt_p_summarized_porg %>% 
  mutate(summary = paste(abundance, "\u00b1", se)) %>% 
  dplyr::select(-abundance, -se)

porg_only =
  table_ftrt_porg %>% 
  filter(phos_pool == "porg")

#now unavp

pall_ftrt_p_summarized_unavp =
  pall2 %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  na.omit() %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to = 'phos_pool', values_to = "abund") %>% 
  group_by(ctrt, ftrt, phos_pool) %>%
  dplyr::summarise(abundance = round(mean(abund), 2),
                   se = round(sd(abund)/sqrt(n()),2))


table_ftrt_unavp = 
  pall_ftrt_p_summarized_unavp %>% 
  mutate(summary = paste(abundance, "\u00b1", se)) %>% 
  dplyr::select(-abundance, -se)

unavp_only =
  table_ftrt_unavp %>% 
  filter(phos_pool == "unavp")



#fit hsd function

fit_hsd = function(dat){
  a = aov(abund ~ ctrt, data = dat)
  h = HSD.test(a, "ctrt")
  h$groups %>% mutate(ctrt = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(ctrt, label)
}


#run fit_hsd function

abund_hsd_ctrt = 
  pall_ftrt_p_sample %>%
  group_by(ftrt, time, phos_pool) %>% 
  do(fit_hsd(.))

#no time, amac only

abund_hsd_ctrt_notime = 
  pall_ctrt_p_sample %>%
  group_by(ftrt, phos_pool) %>% 
  do(fit_hsd(.)) %>% 
  filter(phos_pool == "amac")

#time, porg only

abund_hsd_ftrt_time1_porg = 
  pall_ctrt_p_sample %>%
  filter(time == "1") %>% 
  group_by(ftrt, phos_pool) %>% 
  do(fit_hsd(.)) %>% 
  filter(phos_pool == "porg")


abund_hsd_ftrt_time2_porg = 
  pall_ctrt_p_sample %>%
  filter(time == "2") %>% 
  group_by(ftrt, phos_pool) %>% 
  do(fit_hsd(.)) %>% 
  filter(phos_pool == "porg")


#time, unavp only

abund_hsd_ftrt_time1_uavp = 
  pall_ctrt_p_sample %>%
  filter(time == "1") %>% 
  group_by(ftrt, phos_pool) %>% 
  do(fit_hsd(.)) %>% 
  filter(phos_pool == "unavp")

abund_hsd_ftrt_time2_uavp = 
  pall_ctrt_p_sample %>%
  filter(time == "2") %>% 
  group_by(ftrt, phos_pool) %>% 
  do(fit_hsd(.)) %>% 
  filter(phos_pool == "unavp")

write.csv(abund_hsd_ftrt_time1_porg, "output/hsd_GHporg1.csv")
write.csv(abund_hsd_ftrt_time2_porg, "output/hsd_GHporg2.csv")
write.csv(abund_hsd_ftrt_time2_uavp, "output/hsd_GHunavp2.csv")
write.csv(abund_hsd_ftrt_time1_uavp, "output/hsd_GHunavp1.csv")


# combine with summarized table 
#something is going wrong and I'm winding up with phos pools all being the same

abund_table_with_hsd_ftrt = 
  table_ftrt %>% 
  left_join(abund_hsd_ftrt) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, label),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -label) %>% 
  pivot_wider(names_from = "phos_pool", values_from = "value") %>% 
  force()

abund_table_with_hsd_ftrt %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(abund_table_with_hsd_ftrt, "output/ftrtphos_hsdstats.csv", row.names = FALSE)

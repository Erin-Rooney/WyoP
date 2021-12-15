#Greenhouse incubation
#E Rooney
#5 19 2021

#load packages----------------
source("code/0-packages.R")

#load raw data--------------
incub_dat = read.csv("raw/Inc_finaldata_July2.csv")
bio_dat = read.csv("raw/biocorr_1.csv") %>%  rename('ctrt' = 'ï..ctrt')

# normalize by ccbio------------------

incub_dat2 =
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
  

incub_dat_enzymeslonger =
  incub_dat2 %>% 
  select(time, ctrt, ftrt, phos_percbio, bg_percbio, nag_percbio, ag_percbio, lap_percbio, abts_percbio) %>% 
  pivot_longer(-c(time, ctrt, ftrt), names_to = 'enzymes_type', values_to = 'enzymes_percbio') %>% 
  filter(ctrt != "Control")

#ggplot-----------------

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "phos_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "Phosphatase concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")


phos = incub_dat2 %>% 
  #filter(time != "Baseline" & ctrt == "Control") %>%
  filter(time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ftrt, y = phos), alpha = 0.7) +
  labs(x = "", y = "Phosphatase concentrations in Fallow")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  #facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")

bg = incub_dat2 %>% 
  #filter(time != "Baseline" & ctrt == "Control") %>% 
  filter(time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ftrt, y = bg), alpha = 0.7) +
  labs(x = "", y = "ß-glucosidase concentrations in Fallow")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  #facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")

lap = incub_dat2 %>% 
  #filter(time != "Baseline" & ctrt == "Control") %>% 
  filter(time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ftrt, y = lap), alpha = 0.7) +
  labs(x = "", y = "leucine aminopeptidase concentrations in Fallow")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  #facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")



incub_dat_enzymeslonger %>% 
  # group_by(ctrt, ftrt, time, enzymes_type) %>%
  # dplyr::summarise(abundance = round(mean(enzymes_percbio), 2),
  #                  se = round(sd(enzymes_percbio)/sqrt(n()),2)) %>% 
  mutate(enzymes_type = recode(enzymes_type, "phos_percbio" = "phos",
                               'bg_percbio' = "bg",
                               'nag_percbio' = "nag",
                               'ag_percbio' = "ag",
                               'lap_percbio' = "lap",
                               'abts_percbio' = "abts")) %>% 
  #filter(time != "Baseline" & ctrt == "Control") %>%
  filter(time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = enzymes_type, y = enzymes_percbio, fill = ftrt), alpha = 0.7) +
  labs(x = "", y = "Enzyme concentrations per gram of cover crop biomass")+
  scale_fill_manual(values = wes_palette('Royal1'))+
  #facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")

library(cowplot)
library(patchwork)

phos + bg + lap + plot_layout(guides = "collect")


incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "bg_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "BG Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "nag_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "NAG Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()


incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "lap_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "LAP Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "abts_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "ABTS Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")





#stats-------------------


enzymes_nocon =
  incub_dat_enzymeslonger %>% 
  group_by(ctrt, ftrt, time, enzymes_type) %>%
  dplyr::summarise(abundance = round(mean(enzymes_percbio), 2),
                   se = round(sd(enzymes_percbio)/sqrt(n()),2)) %>% 
  mutate(enzymes_type = recode(enzymes_type, "phos_percbio" = "phos",
                                'bg_percbio' = "bg",
                                'nag_percbio' = "nag",
                                'ag_percbio' = "ag",
                               'lap_percbio' = "lap",
                               'abts_percbio' = "abts")) %>% 
  filter(time != "Baseline") %>% 
  mutate(summary = paste(abundance, "\u00b1", se)) %>% 
  dplyr::select(-abundance, -se) %>% 
  pivot_wider(names_from = "enzymes_type", values_from = "summary")


enzymes_nocon_forstats =
  incub_dat_enzymeslonger %>% 
  # group_by(ctrt, ftrt, time, enzymes_type) %>%
  # dplyr::summarise(abundance = round(mean(enzymes_percbio), 2),
  #                  se = round(sd(enzymes_percbio)/sqrt(n()),2)) %>% 
  mutate(enzymes_type = recode(enzymes_type, "phos_percbio" = "phos",
                               'bg_percbio' = "bg",
                               'nag_percbio' = "nag",
                               'ag_percbio' = "ag",
                               'lap_percbio' = "lap",
                               'abts_percbio' = "abts")) %>% 
  filter(time != "Baseline") %>% 
  # mutate(summary = paste(abundance, "\u00b1", se)) %>% 
  # dplyr::select(-abundance, -se) %>% 
  #pivot_wider(names_from = "enzymes_type", values_from = "enzymes_percbio") %>% 
  force()


abts_aov <- aov(abts_percbio ~ ctrt*ftrt*time, data = incub_dat2 %>% filter(time != "Baseline" & ctrt != "Control"))
summary.aov(abts_aov)

abts.hsd <- HSD.test(abts_aov, "ctrt")
print(abts.hsd)

abts.hsd <- HSD.test(abts_aov, "time")
print(abts.hsd)


lap_aov <- aov(lap_percbio ~ ctrt*ftrt*time, data = incub_dat2 %>% filter(time != "Baseline" & ctrt != "Control"))
summary.aov(lap_aov)

lap.hsd <- HSD.test(lap_aov, "ctrt")
print(lap.hsd)

lap.hsd <- HSD.test(lap_aov, "time")
print(lap.hsd)

#AG

ag_aov <- aov(ag_percbio ~ ctrt*ftrt*time, data = incub_dat2 %>% filter(time != "Baseline" & ctrt != "Control"))
summary.aov(ag_aov)

ag.hsd <- HSD.test(ag_aov, "ctrt")
print(ag.hsd)

ag.hsd <- HSD.test(ag_aov, "time")
print(ag.hsd)

#BG

bg_aov <- aov(bg_percbio ~ ctrt*ftrt*time, data = incub_dat2 %>% filter(time != "Baseline" & ctrt != "Control"))
summary.aov(bg_aov)

bg.hsd <- HSD.test(bg_aov, "ctrt")
print(bg.hsd)

bg.hsd <- HSD.test(bg_aov, "time")
print(bg.hsd)

#PHOS

phos_aov <- aov(phos_percbio ~ ctrt*ftrt*time, data = incub_dat2 %>% filter(time != "Baseline" & ctrt != "Control"))
summary.aov(phos_aov)

phos.hsd <- HSD.test(phos_aov, "ctrt")
print(phos.hsd)

phos.hsd <- HSD.test(phos_aov, "time")
print(phos.hsd)


phos_aov <- aov(phos ~ ftrt*time, data = incub_dat2 %>% filter(time != "Baseline" & ctrt == "Control"))
summary.aov(phos_aov)

phos.hsd <- HSD.test(phos_aov, "ftrt")
print(phos.hsd)

phos.hsd <- HSD.test(phos_aov, "time")
print(phos.hsd)


# abts.hsd %>% knitr::kable() # prints a somewhat clean table in the console
# 
# 
# write.csv(abts.hsd, "abts.hsd.csv", row.names = FALSE)

phos_compost = 
  enzymes_nocon_forstats %>% 
  filter(enzymes_type == "phos" & ftrt == "Compost")

phos_control = 
  enzymes_nocon_forstats %>% 
  filter(enzymes_type == "phos" & ftrt == "Control")

phos = 
  enzymes_nocon_forstats %>% 
  filter(enzymes_type == "phos")

phoscomp.aov <- aov(enzymes_percbio ~ ctrt, data = phos_compost) 
phoscont.aov <- aov(enzymes_percbio ~ ctrt, data = phos_control) 

p <- aov(phos ~ ftrt, data = enzymes_nocon_forstats)
summary.aov(p)

h <- HSD.test(p, "ftrt")
h
  
summary.aov(phoscomp.aov)
summary.aov(phoscont.aov)


l_comp = lme(enzymes_percbio ~ ctrt, random = ~1|time, na.action = na.omit, data = phos_compost)
summary(l_comp)
print(l_comp)
anova(l_comp)




l_cont = lme(enzymes_percbio ~ ctrt, random = ~1|time, na.action = na.omit, data = phos_control)
summary(l_cont)
print(l_cont)
anova(l_cont)

#hsd

phoscomp_hsd = HSD.test(phoscomp.aov, "ctrt")
print(phoscomp_hsd)

phoscont_hsd = HSD.test(phoscont.aov, "ctrt")
print(phoscont_hsd)

phoscomp_table = phoscomp_hsd %>%
  phoscomp_hsd$groups %>% 
  mutate(ctrt = row.names(.)) %>% 
  rename(label = groups) %>%  
  mutate(value = paste(enzymes_percbio, group),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -label) %>% 
  pivot_wider(names_from = "ctrt", values_from = "value") %>% 
  force()

phoscomp_hsd %>% knitr::kable() # prints a somewhat clean table in the console


write.csv(enzymes_nocon, "enzymes_percbio.csv", row.names = FALSE)



#relabund processing (adding and then dividing by total)-------------------

p_relabund_longer_se= 
  incub_dat2 %>%
  na.omit() %>% 
  select(time, ctrt, ftrt, pbic_percbio, amac_percbio, unavp_percbio, porg_percbio) %>% 
  #is this where I need to summarize? pivot longer then pivot wider again?
  pivot_longer(-c(time, ctrt, ftrt), 
               names_to = 'phosphorus_pool', values_to = 'relabund') %>% 
  group_by(ctrt, ftrt, time, phosphorus_pool) %>%
  filter(ctrt != "Control" & time != "Baseline") %>% 
  dplyr::summarise(relabundance = round(mean(relabund), 2),
                   se = round(sd(relabund)/sqrt(n()),2))


p_relabund_longer= 
  incub_dat2 %>%
  na.omit() %>% 
  select(time, ctrt, ftrt, pbic_percbio, amac_percbio, unavp_percbio, porg_percbio) %>% 
  #is this where I need to summarize? pivot longer then pivot wider again?
  # pivot_longer(-c(time, ctrt, ftrt), 
  #              names_to = 'phosphorus_pool', values_to = 'relabund') %>% 
  # group_by(ctrt, ftrt, time, phosphorus_pool) %>%
  filter(ctrt != "Control" & time != "Baseline") %>% 
  # dplyr::summarise(relabundance = round(mean(relabund), 2),
  #                  se = round(sd(relabund)/sqrt(n()),2)) %>% 
  rename("Available P" = "pbic_percbio",
         "Reserve P" = 'amac_percbio',
         "Organic P" = 'porg_percbio',
         "Fixed P" = 'unavp_percbio')

p_relabund_longer_figure= 
  incub_dat2 %>%
  na.omit() %>% 
  select(time, ctrt, ftrt, pbic_percbio, amac_percbio, unavp_percbio, porg_percbio) %>% 
  #is this where I need to summarize? pivot longer then pivot wider again?
  # pivot_longer(-c(time, ctrt, ftrt), 
  #              names_to = 'phosphorus_pool', values_to = 'relabund') %>% 
  # group_by(ctrt, ftrt, time, phosphorus_pool) %>%
  filter(ctrt != "Control" & time != "Baseline") %>% 
  # dplyr::summarise(relabundance = round(mean(relabund), 2),
  #                  se = round(sd(relabund)/sqrt(n()),2)) %>% 
  rename("Available P" = "pbic_percbio",
         "Reserve P" = 'amac_percbio',
         "Organic P" = 'porg_percbio',
         "Fixed P" = 'unavp_percbio') %>% 
  pivot_longer(-c(time, ctrt, ftrt), names_to = 'phosphorus_pool', values_to = 'abundance') %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P"))) %>% 
  force()

p_relabund_wider=
  p_relabund_longer_se %>% 
  select(ctrt, ftrt, time, phosphorus_pool, relabundance) %>% 
  pivot_wider(names_from = "phosphorus_pool", values_from = ("relabundance")) %>% 
  dplyr::mutate(rela_total = (pbic_percbio + amac_percbio + unavp_percbio + porg_percbio),
                rela_pbic = (pbic_percbio/rela_total),
                rela_amac = (amac_percbio/rela_total),
                rela_unavp = (unavp_percbio/rela_total),
                rela_porg = (porg_percbio/rela_total)
  ) 

############

#ggplot (Oat only, p pools across time x ftrt) pair with earlier enzyme figure

p_relabund_longer_figure %>%
  filter(ctrt == "Oat") %>% 
  ggplot()+
  geom_boxplot(aes(x=phosphorus_pool, y=abundance, fill = time), alpha = 0.7)+
  labs(y = "phosphorus concentration/ gram cc biomass", x = "")+
  facet_grid(ftrt~ctrt)+
  ylim(0,12)+
  scale_fill_manual(values = wes_palette('GrandBudapest1',3))+
  theme_er()




####################
#stats--------------





#############
#trying to do relabundances, really not working

# #relabund pivot longer
# p_relabund2 = 
#   p_relabund_wider %>% 
#   select(time, ctrt, ftrt, rela_pbic, rela_amac, rela_unavp, rela_porg) %>% 
#   pivot_longer(-c(time, ctrt, ftrt), 
#                names_to = 'phosphorus_pool', values_to = 'relabund') 
# 
# #trying to make everything add up to 1 so I can make a relabundance figure. Did not work.
# p_relabund3 =
#   p_relabund2 %>%
#   # group_by(ctrt, ftrt, time, phosphorus_pool) %>% 
#   #  dplyr::summarise(relabundance = round(mean(relabund), 2),
#   #                   se = round(sd(relabund)/sqrt(n()),2)) %>% 
#   # ungroup() %>% 
#   #select(time, ctrt, ftrt, phosphorus_pool, relabundance, se) %>% 
#   mutate(phosphorus_pool = recode(phosphorus_pool, "rela_pbic" = "Available P",
#                 'rela_amac' = "Reserve P",
#                 'rela_unavp' = "Fixed P",
#                 'rela_porg' = "Organic P")) %>% 
#   mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Fixed P", "Organic P")))
# 
# 
# #relabundance figure.
#   




#stats (aov to lme to hsd to failure) Started with Pbic by ctrt with time as a randomizing factor

p_noC =
p_relabund %>% 
  filter(ctrt != "Control" & ftrt == "CMPT")


ctrt.aov <- aov(pbic_percbio ~ ctrt, data = p_noC) 
summary.aov(ctrt.aov)

l = lme(pbic_percbio ~ ctrt, random = ~1|time, na.action = na.omit, data = p_noC)
summary(l)
print(l)
anova(l)

#hsd

ctrt_hsd = HSD.test(ctrt.aov, "ctrt")

# trying to make it into a cohesive, exportable table. DId not work.

# hsd_table_pbicctrt = ctrt_hsd %>%
#   ctrt_hsd$groups %>% mutate(ctrt = row.names(.)) %>% 
#   rename(label = groups) %>%  
#   mutate(value = paste(pbic_percbio, group),
#          # this will also add " NA" for the blank cells
#          # use str_remove to remove the string
#          #value = str_remove(value, " NA")
#   ) %>% 
#   dplyr::select(-summary, -label) %>% 
#   pivot_wider(names_from = "slopepos", values_from = "value") %>% 
#   force()


# %>% knitr::kable() # prints a somewhat clean table in the console

#write.csv(relabund_table_with_hsd_covertype, "output/slopepos_hsdstats.csv", row.names = FALSE)

#Relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

# relabund_table = 
#   p_relabund2 %>%
#   group_by(ctrt, ftrt, time, phosphorus_pool) %>% 
#   mutate(phosphorus_pool = recode(phosphorus_pool, "rela_pbic" = "Available P",
#                                   'rela_amac' = "Reserve P",
#                                   'rela_unavp' = "Fixed P",
#                                   'rela_porg' = "Organic P")) %>% 
#   mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Fixed P", "Organic P"))) %>% 
#   dplyr::summarise(relabundance = round(mean(relabund), 2),
#                    se = round(sd(relabund)/sqrt(n()),2)) %>% 
#   filter(ctrt != "Control") %>% 
#   mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
#   dplyr::select(-relabundance, -se)
# 
# ## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class
# 
# fit_aov_ppool = function(dat){
#   
#   aov(relabundance ~ ctrt, data = dat) %>% 
#     broom::tidy() %>% # convert to clean dataframe
#     rename(pvalue = `p.value`) %>% 
#     filter(term == "ctrt") %>% 
#     mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
#     dplyr::select(asterisk) %>% # we need only the asterisk column
#     # two steps below, we need to left-join. 
#     # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
#     #mutate(cover_type = "Open")   
#     force()
#   
# }
# 
# fit_hsd = function(dat){
#   a = aov(relabundance ~ ctrt, data = p_relabund3)
#   h = HSD.test(a, "ctrt")
#   h$groups %>% mutate(ctrt = row.names(.)) %>% 
#     rename(label = groups) %>%  
#     dplyr::select(ctrt, label)
# }


# ## step 3: run the fit_anova function 
# ## do this on the original relabund file, because we need all the reps
# 
# relabund_hsd_ppools = 
#   p_relabund3 %>% 
#   group_by(ctrt) %>% 
#   do(fit_hsd(.))
# 

# ## step 4: combine the summarized values with the asterisks
# relabund_table_ctrtppool = 
#   relabund_table %>% 
#   left_join(relabund_hsd_ppools) %>%
#   # combine the values with the label notation
#   mutate(value = paste(summary, label),
#          # this will also add " NA" for the blank cells
#          # use str_remove to remove the string
#          #value = str_remove(value, " NA")
#   ) %>% 
#   dplyr::select(-summary, -label) %>% 
#   pivot_wider(names_from = "phosphorus_pool", values_from = "value") %>% 
#   force()
# 
# 
# relabund_table_with_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console
# 
# write.csv(relabund_table_with_hsd_covertype, "output/slopepos_hsdstats.csv", row.names = FALSE)


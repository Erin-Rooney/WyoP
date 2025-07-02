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


incub_dat_enzymeslonger_control =
  incub_dat %>% 
  select(time, ctrt, ftrt, phos) %>% 
  pivot_longer(-c(time, ctrt, ftrt), names_to = 'enzymes_type', values_to = 'enzymes_conc') %>% 
  filter(ctrt == "Control")

#ggplot-----------------

#no longer in manuscript
#boxplot of enzyme activity (non cumulative) across cover crop * fert

phos_fig =
  incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "phos_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "Acid phosphatase activity (nmol/h/g soil)")+
  scale_fill_manual(values = wes_palette('Zissou1',3))+
  facet_grid(ftrt~.)+
  theme_er()+
  theme(legend.position = "bottom")

ggsave("output/phos_fig.tiff", plot = phos_fig, height = 5, width = 4)


#averaging acid phosphatase activity across cover crop, ftrt, and time 
#recoding weeks to numbers


incub_dat_enzymeslonger_forfig =
  incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "phos_percbio") %>% 
  group_by(ctrt, ftrt, time) %>% 
  dplyr::summarise(mean = mean(enzymes_percbio),
                   se = sd(enzymes_percbio)/sqrt(n()),
                   ) %>% 
  mutate(ctrt = recode(ctrt, "All" = "All Mixture")) %>% 
  mutate(time = recode(time, "Baseline" = "0", "T1" = "4", "T2" = "8",
                       "T3" = "12"))  %>% 
  mutate(time = factor(time, levels = c('0', '4', "8", "12")))

#cumulative sum for acid phosphatase activity for all cover crop treatments (not fallow)
incub_dat_enzymeslonger_forfig_cumsum =
  incub_dat_enzymeslonger_forfig %>% 
  group_by(ctrt, ftrt) %>% 
  dplyr::summarise(mean = cumsum(mean),
                   time = time,
                   sd = sd(mean))


#fallow acid phosphatase activity
#recode weeks by number 

incub_dat_enzymeslonger_control_2 =
  incub_dat_enzymeslonger_control %>% 
  mutate(time = recode(time, "Baseline" = "0", "T1" = "4", "T2" = "8",
                       "T3" = "12"))  %>% 
  mutate(time = factor(time, levels = c('0', '4', "8", "12"))) 

#summarise fallow acid phosphatase activity 
#cumulative acid phosphatase activity

incub_dat_enzymeslonger_control_summarised =
  incub_dat_enzymeslonger_control_2 %>% 
  group_by(time, ctrt, ftrt) %>% 
  dplyr::summarise(mean = mean(enzymes_conc),
                   sd = sd(enzymes_conc)) %>% 
  group_by(ctrt, ftrt)  %>% 
  dplyr::summarise(mean = cumsum(mean),
                   time = time,
                   sd = sd) 


#ggplot in manuscript
#acid phosphatase for ctrt (no fallow)
#cumulative sum

phos_fig_time =
  incub_dat_enzymeslonger_forfig_cumsum %>% 
  #filter(time != "0") %>%
  ggplot() +
  geom_point(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, x = time, color = ftrt), width=.2)+
  geom_line(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  labs(x = "weeks", y = "Acid phosphatase activity (nmol/h/g soil)")+
  scale_color_manual(values = c("#59629b","#e69b99"))+
  facet_wrap(ctrt~.)+
  theme_er()+
  theme(legend.position = "bottom")

#ggplot in manuscript
#acid phosphatase for fallow only 
#cumulative sum

fallow_enzymes =
  incub_dat_enzymeslonger_control_summarised %>% 
 # mutate(ctrt = factor(ctrt, levels = c("Control", "All Mixture", "Faba", "Faba Oat", "Oat", "Radish")))   %>%
  mutate(ctrt = recode(ctrt, "Control" = "Control (Fallow)")) %>% 
 # filter(time != "0") %>%
  ggplot() +
  geom_point(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, x = time, color = ftrt), width=.2)+
  geom_line(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  labs(x = "weeks", y = "Acid phosphatase activity (nmol/h/g soil)")+
  scale_color_manual(values = c("#59629b","#e69b99"))+
  facet_wrap(ctrt~., scales = "free_y")+
  theme_er()+
  theme(legend.position = "bottom")

ggsave("output/phos_fig_time.tiff", plot = phos_fig_time, height = 4.6, width = 5.9)
ggsave("output/phos_fig_time_fallow.tiff", plot = fallow_enzymes, height = 4.5, width = 4)



phos_fig_time =
  incub_dat_enzymeslonger_forfig_cumsum %>% 
  #filter(time != "0") %>%
  ggplot() +
  geom_point(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, x = time, color = ftrt), width=.2)+
  geom_line(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  labs(x = "weeks", y = "Acid phosphatase activity (nmol/h/g soil)")+
  scale_color_manual(values = c("#59629b","#e69b99"))+
  facet_wrap(ctrt~.)+
  theme_er()+
  theme(legend.position = "bottom")

#ggplot in manuscript
#acid phosphatase for fallow only 
#cumulative sum

fallow_enzymes =
  incub_dat_enzymeslonger_control_summarised %>% 
  # mutate(ctrt = factor(ctrt, levels = c("Control", "All Mixture", "Faba", "Faba Oat", "Oat", "Radish")))   %>%
  mutate(ctrt = recode(ctrt, "Control" = "Control (Fallow)")) %>% 
  # filter(time != "0") %>%
  ggplot() +
  geom_point(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, x = time, color = ftrt), width=.2)+
  geom_line(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  labs(x = "weeks", y = "Acid phosphatase activity (nmol/h/g soil)")+
  scale_color_manual(values = c("#59629b","#e69b99"))+
  facet_wrap(ctrt~., scales = "free_y")+
  theme_er()+
  theme(legend.position = "bottom")
























#Not in manuscript
#acid phosphatase for ctrt (no fallow)
#not cumulative sum

incub_dat_enzymeslonger_forfig %>% 
  filter(time != "0") %>%
  ggplot() +
  geom_point(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  geom_line(aes(x = time, y = mean, color = ftrt, group = ftrt), size = 1.25) +
  labs(x = "weeks", y = "Acid phosphatase activity (nmol/h/g soil)")+
  scale_color_manual(values = c("#59629b","#e69b99"))+
  facet_wrap(ctrt~.)+
  theme_er()+
  theme(legend.position = "bottom")


#ggsave for manuscript figures



# phos_ftrt_fallow_fig =
#   incub_dat_enzymeslonger_control_2 %>% 
#   filter(time != "Baseline") %>% 
#   ggplot() +
#   geom_boxplot(aes(x = ftrt, y = enzymes_conc, fill = time), alpha = 0.7) +
#   labs(x = "", y = "Acid phosphatase activity 
#        (nmol/h/g soil)", fill = "weeks")+
#   scale_fill_manual(values = wes_palette('Zissou1',3))+
#   #facet_grid(ftrt~.)+
#   theme_er()+
#   theme(legend.title = element_text(size=12, color="black"), legend.position = "bottom")

ggsave("output/phos_ftrt.tiff", plot = phos_ftrt_fallow_fig, height = 3, width = 3)

#phosphorus pools, no fallow

incub_dat2_forfig =
  incub_dat2 %>% 
  filter(ctrt == "Control") %>% 
  dplyr::select(c(time, ftrt, pbic, amac, unavp, porg)) %>% 
  pivot_longer(-c(time, ftrt), names_to = 'p_type', values_to = 'concentration') %>% 
  dplyr::mutate(p_type = recode(p_type, "pbic" = "available P",
                                "amac" = "reserve P",
                                "porg" = "organic P", 
                                "unavp" = "fixed P")) %>% 
  dplyr::mutate(p_type = factor(p_type, levels = c("available P", "reserve P", "organic P", "fixed P"))) 
  
#phosphorus pools, no fallow, averaged
#summarise by mean, for table in manuscript

incub_dat2_forfig_summarised =
  incub_dat2_forfig %>% 
  group_by(time, ftrt, p_type) %>% 
  dplyr::summarise(mean = mean(concentration),
                   sd = sd(concentration)) 

#ggplot in manuscript
#phosphorus pools, fallow only

phosphorus_ftrt_fallow_fig =
  incub_dat2_forfig %>% 
  filter(time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ftrt, y = concentration, fill = time), alpha = 0.7) +
  labs(x = "", y = "P concentration mg/kg" 
       )+
  scale_fill_manual(values = wes_palette('Zissou1',3))+
  facet_wrap(p_type~., scales = "free_y")+
  theme_er()+
  theme(legend.position = "NONE")

ggsave("output/p_ftrt.tiff", plot = phosphorus_ftrt_fallow_fig, height = 5, width = 5.45)

######

#stats included in mansucript
#this should really be a function


fallow_aov <- aov(concentration ~ time*ftrt, data = incub_dat2_forfig %>% 
                        filter(time != "Baseline" & p_type == "available P"))
summary.aov(fallow_aov)

fallow_aov <- aov(concentration ~ time*ftrt, data = incub_dat2_forfig %>% 
                    filter(time != "Baseline" & p_type == "reserve P"))
summary.aov(fallow_aov)

fallow_aov <- aov(concentration ~ time*ftrt, data = incub_dat2_forfig %>% 
                    filter(time != "Baseline" & p_type == "organic P"))
summary.aov(fallow_aov)


fallow_aov <- aov(concentration ~ time*ftrt, data = incub_dat2_forfig %>% 
                    filter(time != "Baseline" & p_type == "fixed P"))
summary.aov(fallow_aov)


phosphorus_table_stats_control_fallow =
  incub_dat2_forfig %>% 
  filter(time != "Baseline" & ftrt == "Control") 


phosphorus_table_stats_compost_fallow =
  incub_dat2_forfig %>% 
  filter(time != "Baseline" & ftrt == "Compost") 

#available P for compost and control ftrt treatments

avapfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_control_fallow %>% filter(p_type == "available P"))
summary.aov(avapfallow_aov)

avapfallow.hsd <- HSD.test(avapfallow_aov, "time")
print(avapfallow.hsd)

avapfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_compost_fallow %>% filter(p_type == "available P"))
summary.aov(avapfallow_aov)

avapfallow.hsd <- HSD.test(avapfallow_aov, "time")
print(avapfallow.hsd)



reservepfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_control_fallow %>% filter(p_type == "reserve P"))
summary.aov(reservepfallow_aov)

reservepfallow.hsd <- HSD.test(reservepfallow_aov, "time")
print(reservepfallow.hsd)

reservepfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_compost_fallow %>% filter(p_type == "reserve P"))
summary.aov(reservepfallow_aov)

reservepfallow.hsd <- HSD.test(reservepfallow_aov, "time")
print(reservepfallow.hsd)




porgfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_control_fallow %>% filter(p_type == "organic P"))
summary.aov(porgfallow_aov)

porgfallow.hsd <- HSD.test(porgfallow_aov, "time")
print(porgfallow.hsd)

porgfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_compost_fallow %>% filter(p_type == "organic P"))
summary.aov(porgfallow_aov)

porgfallow.hsd <- HSD.test(porgfallow_aov, "time")
print(porgfallow.hsd)



unavpfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_control_fallow %>% filter(p_type == "fixed P"))
summary.aov(unavpfallow_aov)

unavpfallow.hsd <- HSD.test(unavpfallow_aov, "time")
print(unavpfallow.hsd)

unavpfallow_aov <- aov(concentration ~ time, data = phosphorus_table_stats_compost_fallow %>% filter(p_type == "fixed P"))
summary.aov(unavpfallow_aov)

unavpfallow.hsd <- HSD.test(unavpfallow_aov, "time")
print(unavpfallow.hsd)

#####





#not in manuscript 
#all enzymes ftrt x ctrt

all_enzymes =
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
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = ftrt), alpha = 0.7) +
  labs(x = "", y = "Enzyme concentrations per gram of cover crop biomass")+
  scale_fill_manual(values = wes_palette('Royal1'))+
  facet_wrap(.~enzymes_type, scales = 'free_y')+
  theme_er()+
  theme(legend.position = "bottom")


ggsave("output/allenzymes.tiff", plot = all_enzymes, height = 6, width = 10)


#not in manuscript 
#BG ftrt x ctrt x time

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "bg_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "BG Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()

#not in manuscript 
#NAG ftrt x ctrt x time

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "nag_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "NAG Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()

#not in manuscript 
#LAP ftrt x ctrt x time

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "lap_percbio" & time != "Baseline") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "LAP Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('Royal1',3))+
  facet_grid(ftrt~.)+
  theme_er()


#not in manuscript 
#ABTS ftrt x ctrt x time

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

#all enzyme averges + se table
#not in manuscript

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

#####

#stats for acid phosphatase
#stats in manuscript
#should definitely be function

phos_aov <- aov(enzymes_percbio ~ ctrt*time*ftrt, data = incub_dat_enzymeslonger %>% filter(enzymes_type == "phos_percbio"))
summary.aov(phos_aov)

phos2_aov <- aov(enzymes_conc ~ time*ftrt, data = incub_dat_enzymeslonger_control_2)
summary.aov(phos2_aov)

phos2compost_aov <- aov(enzymes_conc ~ time, data = incub_dat_enzymeslonger_control_2 %>% filter(ftrt == "Compost"))
summary.aov(phos2compost_aov)

phos2compost.hsd <- HSD.test(phos2compost_aov, "time")
print(phos2compost.hsd)

phos2control_aov <- aov(enzymes_conc ~ time, data = incub_dat_enzymeslonger_control_2 %>% filter(ftrt == "Control"))
summary.aov(phos2control_aov)

phos2control.hsd <- HSD.test(phos2control_aov, "time")
print(phos2control.hsd)

#######

#phosphorus pools stats
#included in manuscript

phosphorus_table_stats_control =
  incub_dat2 %>% 
  filter(time != "Baseline" & ctrt != "Control" & ftrt == "Control") 


phosphorus_table_stats_compost =
  incub_dat2 %>% 
  filter(time != "Baseline" & ctrt != "Control" & ftrt == "Compost") 

#available P for compost and control ftrt treatments
  
avap1_aov <- aov(pbic_percbio ~ ctrt, data = phosphorus_table_stats_control)
summary.aov(avap1_aov)

avap1.hsd <- HSD.test(avap1_aov, "ctrt")
print(avap1.hsd)


avap2_aov <- aov(pbic_percbio ~ ctrt, data = phosphorus_table_stats_compost)
summary.aov(avap2_aov)

avap2.hsd <- HSD.test(avap2_aov, "ctrt")
print(avap2.hsd)

#reserve P for compost and control ftrt treatments

reserve1_aov <- aov(amac_percbio ~ ctrt, data = phosphorus_table_stats_control)
summary.aov(reserve1_aov)

reserve1.hsd <- HSD.test(reserve1_aov, "ctrt")
print(reserve1.hsd)


reserve2_aov <- aov(amac_percbio ~ ctrt, data = phosphorus_table_stats_compost)
summary.aov(reserve2_aov)

reserve2.hsd <- HSD.test(reserve2_aov, "ctrt")
print(reserve2.hsd)

#fixed P for compost and control ftrt treatments

fixed1_aov <- aov(unavp_percbio ~ ctrt, data = phosphorus_table_stats_control)
summary.aov(fixed1_aov)

fixed1.hsd <- HSD.test(fixed1_aov, "ctrt")
print(fixed1.hsd)


fixed2_aov <- aov(unavp_percbio ~ ctrt, data = phosphorus_table_stats_compost)
summary.aov(fixed2_aov)

fixed2.hsd <- HSD.test(fixed2_aov, "ctrt")
print(fixed2.hsd)

#organic P for compost and control ftrt treatments

organic1_aov <- aov(porg_percbio ~ ctrt, data = phosphorus_table_stats_control)
summary.aov(organic1_aov)

organic1.hsd <- HSD.test(organic1_aov, "ctrt")
print(organic1.hsd)


organic2_aov <- aov(porg_percbio ~ ctrt, data = phosphorus_table_stats_compost)
summary.aov(organic2_aov)

organic2.hsd <- HSD.test(organic2_aov, "ctrt")
print(organic2.hsd)

#

#phosphorus pools table ctrt*ftrt
#included in manuscript

phosphorus_table =
  incub_dat2 %>% 
  filter(time != "Baseline" & ctrt != "Control") %>% 
  na.omit() %>% 
  group_by(ftrt, ctrt) %>%
  dplyr::summarise(availablepmean = round(mean(pbic_percbio), 2),
                   availablepse = round(sd(pbic_percbio)/sqrt(n()),2),
                   reservepmean = round(mean(amac_percbio), 2),
                   reservepse = round(sd(amac_percbio)/sqrt(n()),2),
                   fixedpmean = round(mean(unavp_percbio), 2),
                   fixedpse = round(sd(unavp_percbio)/sqrt(n()),2),
                   organicpmean = round(mean(porg_percbio), 2),
                   organicpse = round(sd(porg_percbio)/sqrt(n()),2)) %>%
  # mutate(enzymes_type = recode(enzymes_type, "phos_percbio" = "phos",
  #                              'bg_percbio' = "bg",
  #                              'nag_percbio' = "nag",
  #                              'ag_percbio' = "ag",
  #                              'lap_percbio' = "lap",
  #                              'abts_percbio' = "abts")) %>% 
  mutate(available_p_summary = paste(availablepmean, "\u00b1", availablepse),
         reserve_p_summary = paste(reservepmean, "\u00b1", reservepse),
         fixed_p_summary = paste(fixedpmean, "\u00b1", fixedpse), 
         organic_p_summary = paste(organicpmean, "\u00b1", organicpse)) %>% 
  dplyr::select(c(ctrt, ftrt, available_p_summary, reserve_p_summary, fixed_p_summary,
                  organic_p_summary))

phosphorus_table %>% knitr::kable() 

write.csv(phosphorus_table, "output/phosphorus_table_inc.csv")


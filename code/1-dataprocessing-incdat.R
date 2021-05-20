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
  pivot_longer(-c(time, ctrt, ftrt), names_to = 'enzymes_type', values_to = 'enzymes_percbio') 

#ggplot-----------------

incub_dat_enzymeslonger %>% 
  filter(enzymes_type == "phos_percbio", ctrt != "Control") %>% 
  ggplot() +
  geom_boxplot(aes(x = ctrt, y = enzymes_percbio, fill = time), alpha = 0.7) +
  labs(x = "", y = "Phosphotase Enzyme concentrations/g ccbiomass")+
  scale_fill_manual(values = wes_palette('GrandBudapest2',4))+
  theme_er()
  
#relabund-------------------

p_relabund = 
  incub_dat2 %>% 
  select(time, ctrt, ftrt, pbic_percbio, amac_percbio, unavp_percbio, porg_percbio) %>% 
  dplyr::mutate(rela_total = (pbic_percbio + amac_percbio + unavp_percbio + porg_percbio),
                rela_pbic = (pbic_percbio/rela_total),
                rela_amac = (amac_percbio/rela_total),
                rela_unavp = (unavp_percbio/rela_total),
                rela_porg = (porg_percbio/rela_total)
  ) 

p_relabund2 = 
  p_relabund %>% 
  select(time, ctrt, ftrt, rela_pbic, rela_amac, rela_unavp, rela_porg) %>% 
  pivot_longer(-c(time, ctrt, ftrt), 
               names_to = 'phosphorus_pool', values_to = 'relabund') 

p_relabund3 =
  p_relabund2 %>%
  group_by(ctrt, ftrt, time, phosphorus_pool) %>% 
   dplyr::summarise(relabundance = round(mean(relabund), 2),
                    se = round(sd(relabund)/sqrt(n()),2)) %>% 
  ungroup() %>% 
  select(time, ctrt, ftrt, phosphorus_pool, relabundance, se) %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "rela_pbic" = "Available P",
                'rela_amac' = "Reserve P",
                'rela_unavp' = "Fixed P",
                'rela_porg' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Fixed P", "Organic P")))


  
p_relabund3 %>% 
  filter(ctrt != "Control") %>% 
  ggplot(aes(x = time, y = relabundance))+
  geom_bar(aes(fill = phosphorus_pool), stat = "identity")+
  facet_grid(ftrt ~ ctrt)+
  labs(x = "Time", 
       y = "Relative Abundance")+
  scale_fill_manual(values = rev(pnw_palette("Shuksan2",4)))+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  ylim(0,1)+
  theme(legend.position = 'bottom')+
  NULL






# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

relabund_table = 
  fticr_water_relabund_summarized %>% 
  #filter(cover_type == "Open") %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class

fit_aov_ppool = function(dat){
  
  aov(relabundance ~ ctrt, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "ctrt") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    #mutate(cover_type = "Open")   
    force()
  
}

fit_hsd = function(dat){
  a = aov(relabundance ~ ctrt * phosphorus_pools, data = p_relabund3)
  h = HSD.test(a, "ctrt" * "phosphorus_pools")
  h$groups %>% mutate(ctrt = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(ctrt, label)
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

relabund_hsd_ppools = 
  p_relabund3 %>% 
  filter(ctrt != "Control") %>% 
  group_by(ctrt) %>% 
  do(fit_hsd(.))

## step 4: combine the summarized values with the asterisks
relabund_table_with_hsd_covertype = 
  relabund_table_covertype %>% 
  left_join(relabund_hsd_covertype) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, label),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -label) %>% 
  pivot_wider(names_from = "slopepos", values_from = "value") %>% 
  force()


relabund_table_with_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_table_with_hsd_covertype, "output/slopepos_hsdstats.csv", row.names = FALSE)


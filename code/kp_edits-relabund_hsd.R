### RELABUND

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


p_relabund_by_sample = 
  incub_dat2 %>%
  na.omit() %>% 
  select(X, time, ctrt, ftrt, pbic_percbio, amac_percbio, unavp_percbio, porg_percbio) %>% 
  #is this where I need to summarize? pivot longer then pivot wider again?
  pivot_longer(-c(X, time, ctrt, ftrt), 
               names_to = 'phosphorus_pool', values_to = 'abund') %>% 
  mutate(abund = if_else(abund < 0, 0, abund)) %>% 
  group_by(X, ctrt, ftrt, time) %>%
  dplyr::mutate(total = sum(abund)) %>% 
  ungroup() %>% 
  mutate(relabund = (abund/total)*100)

p_relabund_summary = 
  p_relabund_by_sample %>% 
  group_by(ctrt, ftrt, time, phosphorus_pool) %>% 
  dplyr::summarise(relabund_mean = mean(relabund)) %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "pbic_percbio" = "Available P",
                                  'amac_percbio' = "Reserve P",
                                  'unavp_percbio' = "Fixed P",
                                  'porg_percbio' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P")))


p_relabund_summary %>% 
  filter(ctrt != "Control" & time != "Baseline") %>% 
  ggplot(aes(x = time, y = relabund_mean))+
  geom_bar(aes(fill = phosphorus_pool), stat = "identity")+
  facet_grid(ftrt ~ ctrt)+
  labs(x = "Time", 
       y = "Relative Abundance")+
  scale_fill_manual(values = (wes_palette("Moonrise3",4)))+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  #ylim(0,1)+
  theme(legend.position = 'bottom')+
  NULL
  

# 3. PCA ---------------------------------------------------------------------
## you will need relative abundance data for PCA 

## install the ggbiplot package from github
## install the miraKlein version, not vqv

## devtools::install_github("miraKlein/ggbiplot")
library(ggbiplot)


## 3a. all samples ---------------------------------------------------------
## step i. make wider
relabund_wide =
  p_relabund_summary %>%
  ungroup %>% 
  pivot_wider(names_from = "phosphorus_pool", values_from = "relabund_mean") %>%
  filter(ctrt != "Control" & time != "Baseline") %>% 
  replace(is.na(.),0) 

## step ii. split into numeric/factor dataframes, and run PCA on those
num = 
  relabund_wide %>% 
  dplyr::select(c(`Available P`, `Reserve P`, `Organic P`, `Fixed P`))

grp = 
  relabund_wide %>% 
  dplyr::select(-c(`Available P`, `Reserve P`, `Organic P`, `Fixed P`),
                ctrt,ftrt, time) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$ctrt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp$ftrt))+
  labs(title = "By Covercrop")+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 5)))+
  theme_er()+
  NULL

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$ctrt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = groups))+
  labs(title = "By Covercrop")+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 5)))+
  theme_er()+
  NULL

#PERMANOVA all P values are 1 (???)
#create issue in github


relabund_wide_sample =
  p_relabund_by_sample %>%
  ungroup %>% 
  select(!abund, !total, !X, 
         time, ctrt, ftrt, phosphorus_pool, relabund) %>% 
   mutate(phosphorus_pool = recode(phosphorus_pool, "pbic_percbio" = "Available P",
                                  'amac_percbio' = "Reserve P",
                                  'unavp_percbio' = "Fixed P",
                                  'porg_percbio' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P"))) %>%
  pivot_wider(names_from = "phosphorus_pool", values_from = "relabund") %>%
  filter(ctrt != "Control" & time != "Baseline") %>% 
  replace(is.na(.),0) 


library(vegan)
# permanova_fticr_all = 
adonis(relabund_wide %>% select(c(`Available P`, `Reserve P`, `Organic P`, `Fixed P`)) ~ 
         (ctrt*ftrt*time), 
       data = relabund_wide) 









#
table = 
  p_relabund_summary %>% 
  filter(phosphorus_pool == "Reserve P" & time == "T3") %>% 
  pivot_wider(names_from = ftrt, values_from = relabund_mean)
  
  


################


## setting up aov function
aov_fit -- relabund ~ ftrt

p_relabund_by_sample_t3 =
  p_relabund_by_sample %>% 
  filter(time == "T3" & ctrt != "Control") %>% 
  select(ctrt, ftrt, phosphorus_pool, relabund)
  

aov_fit = function(dat){
  
  aov(relabund ~ ftrt, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>%
    filter(term == "ftrt") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    #mutate(cover_type = "Open")   
    force()
  
}


aov_fit_byftrt = 
  p_relabund_by_sample_t3 %>% 
  group_by(ctrt, phosphorus_pool) %>% 
  do(aov_fit(.))


relabund_table = 
  p_relabund_summary %>% 
  filter(time == "T3" & ctrt != "Control") %>% 
  mutate(phosphorus_pool = recode(phosphorus_pool, "rela_pbic" = "Available P",
                                  'rela_amac' = "Reserve P",
                                  'rela_unavp' = "Fixed P",
                                  'rela_porg' = "Organic P")) %>% 
  mutate(phosphorus_pool = factor(phosphorus_pool, levels = c('Available P', "Reserve P", "Organic P", "Fixed P"))) %>% 
  pivot_wider(names_from = ftrt, values_from = relabund_mean)%>%
  group_by(ctrt, phosphorus_pool) %>% 
  # dplyr::summarise(relabundance = round(mean(relabund), 2),
  #                  se = round(sd(relabund)/sqrt(n()),2)) %>% 
  # mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  # dplyr::select(-relabundance, -se) %>% 
  force()


## step 4: combine the summarized values with the asterisks
relabund_table_ftrtppool = 
  relabund_table %>% 
  left_join(aov_fit_byftrt) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, label),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  # dplyr::select(-summary, -label) %>% 
  # pivot_wider(names_from = "phosphorus_pool", values_from = "value") %>% 
  force()



## setting up hsd function
hsd_fit -- relabund ~ ctrt

relabund_bysample %>% 
  group_by(ftrt, phosphorus_pool) %>% 
  do(hsd_fit(.))
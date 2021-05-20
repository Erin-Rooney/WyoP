#Greenhouse incubation
#E Rooney
#5 19 2021

#load packages----------------
source("code/0-packages.R")

#load raw data--------------
gh_dat = read.csv("raw/gh_pfrac.csv") %>%  rename('ctrt' = 'ï..ctrt')
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
  dplyr::mutate(pbic = as.numeric(pbic),
                cbio = as.numeric(cbio)) %>% 
  dplyr::mutate(pbic_per_cbio = (pbic/cbio))
  

gh_dat %>% 
  ggplot(aes(x = as.character(time), y = as.numeric(amac)))+
  geom_point(aes(group = ctrt))+
  facet_wrap(ctrt~.)+
  theme_er()+
  NULL

pall2 %>% 
  filter(ftrt == "CMPT") %>% 
  mutate(ftrt, recode(ftrt, "CPMT" = 'Compost')) %>% 
  ggplot()+
  geom_boxplot(aes(x = as.character(time), y = as.numeric(pbic), group = time, fill = ftrt), alpha = 0.5)+
  facet_wrap(ctrt~.)+
  labs(x = "Rotation", y = "Available P")+
  scale_fill_manual(values = wes_palette('Darjeeling2'))+
  theme_er()+
  NULL

pall2 %>% 
  ggplot()+
  geom_boxplot(aes(x = as.character(time), y = as.numeric(pbic), group = time, fill = ftrt), alpha = 0.5)+
  facet_grid(ftrt~ctrt)+
  labs(x = "Rotation", y = "Available P")+
  scale_fill_manual(values = wes_palette('Darjeeling1',3))+
  theme_er()+
  NULL

pall2 %>% 
  ggplot()+
  geom_boxplot(aes(x = as.character(time), y = as.numeric(pbic_per_cbio), group = time, fill = ftrt), alpha = 0.5)+
  facet_grid(ftrt~ctrt)+
  labs(x = "Rotation", y = "Available P/gram cc biomass")+
  scale_fill_manual(values = wes_palette('Darjeeling1',3))+
  theme_er()+
  ylim(0,50)+
  NULL


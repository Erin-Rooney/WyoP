### 3 Factor ANOVAs for all SOM parameters

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

#standardized 3 way anovas

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


#not standardized + fallow

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

unavp2_aov <- aov(unavp ~ ftrt*ctrt*time, data = pall2)
summary.aov(unavp2_aov)

porg2_aov <- aov(porg ~ ftrt*ctrt*time, data = pall2)
summary.aov(porg2_aov)


#not standardized minus fallow

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


  
pall2 %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

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


#

allmix = 
  pall2 %>% 
  filter(ctrt == "All Mixture") 

pbic4_aov <- aov(pbic ~ ftrt*time, data = allmix)
summary.aov(pbic4_aov)

pbic4hst <- HSD.test(pbic4_aov, "ftrt")
pbic4hst

cbio4_aov <- aov(cbio ~ ftrt*time, data = allmix)
summary.aov(cbio4_aov)

pbic4hst <- HSD.test(pbic4_aov, "ftrt")
pbic4hst

oat = 
  pall2 %>% 
  filter(ctrt == "Oat") 


pbic5_aov <- aov(pbic ~ ftrt*time, data = oat)
summary.aov(pbic5_aov)


pbic5hsd <- HSD.test(pbic5_aov, "ftrt")
pbic5hsd


radish = 
  pall2 %>% 
  filter(ctrt == "Radish") 

pbic6_aov <- aov(pbic ~ ftrt*time, data = radish)
summary.aov(pbic6_aov)


pbic6hsd <- HSD.test(pbic6_aov, "ftrt")
pbic6hsd


#############



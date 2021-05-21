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
                pmc_percbio = (pmc/cbio)) 

#anovas

ctrt_nit_aov <- aov(nit_percbio ~ ctrt * ftrt, data = pall3) 
summary.aov(ctrt_nit_aov)

pall3 = 
  pall2 %>% 
  na.omit()

l = lme(nit_percbio ~ ctrt * ftrt, random = ~1|time, na.action = na.omit, data = pall3)
summary(l)
print(l)
anova(l)

#hsd

ctrt_hsd = HSD.test(ctrt_nit_aov, "ctrt")

print(ctrt_hsd)





#correlation matrix for all cover crops (no fallow) no caco3/inorganic c

corr_data =
  pall2 %>%
  filter(ctrt != 'Fallow') %>% 
  select(cbio, wbio, pbic_percbio, amac_percbio, unavp_percbio, 
         porg_percbio, amm_percbio, nit_percbio,
         pmn_percbio, pmc_percbio) %>% 
  na.omit() %>% 
  force()

matrix1 <- rcorr(as.matrix(corr_data))

matrix1


# Extract the correlation coefficients
corrpercbio_r <- matrix1$r
# Extract p-values
corrpercbio_p <- matrix1$P

write.csv(corrpercbio_r, "corrpercbio_r.csv", row.names = TRUE)
write.csv(corrpercbio_p, "corrpercbio_p.csv", row.names = TRUE)


#correlation matrix for all cover crops with caco3/inorganic c (time1 only)


corr_data2 =
  pall2 %>%
  filter(time == '1') %>% 
  select(cbio, wbio, pbic_percbio, amac_percbio, unavp_percbio, 
         porg_percbio, inorgcarbon_percbio, amm_percbio, nit_percbio,
         pmn_percbio, pmc_percbio) %>% 
  force()

matrix2 <- rcorr(as.matrix(corr_data2))

matrix2

##rcorr(matrix2, type = c("pearson"))


# Extract the correlation coefficients
corrpercbiocaco3_r <- matrix2$r
# Extract p-values
corrpercbiocaco3_p <- matrix2$P

write.csv(corrpercbiocaco3_r, "corrpercbiocaco3_r.csv", row.names = TRUE)
write.csv(corrpercbiocaco3_p, "corrpercbiocaco3_p.csv", row.names = TRUE)

#correlation matrix no standardization, no caco3

corr_data3 =
  pall2 %>%
  #filter(time == '1') %>% 
  select(cbio, wbio, pbic, amac, unavp, 
         porg, amm, nit,
         pmn, pmc) %>% 
  force()

matrix3 <- rcorr(as.matrix(corr_data3))

matrix3


# Extract the correlation coefficients
corr_r <- matrix3$r
# Extract p-values
corr_p <- matrix3$P

write.csv(corr_r, "corr_r.csv", row.names = TRUE)
write.csv(corr_p, "corr_p.csv", row.names = TRUE)




#################
#ggplots

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


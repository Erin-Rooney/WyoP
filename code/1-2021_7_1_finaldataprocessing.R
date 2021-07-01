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
  dplyr::mutate(wbio_per_cbio = (wbio/cbio))

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

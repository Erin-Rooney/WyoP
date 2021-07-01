#Greenhouse incubation
#E Rooney
#5 19 2021

#load packages----------------
source("code/0-packages.R")

#load raw data--------------
incub_dat = read.csv("raw/Inc_finaldata_July2.csv")
bio_dat = read.csv("raw/biocorr_1.csv") %>%  rename('ctrt' = 'ï..ctrt')
bio_dat_longer = read.csv("raw/allfert_allcrop_P.csv") 
final_dat = read.csv("raw/2021_7_1_finaldat.csv")
gh_dat = read.csv("raw/gh_pfrac.csv") %>%  rename('ctrt' = 'ï..ctrt')

#data processing

# bio_dat2 =
#   bio_dat %>% 
#   mutate(ctrt = factor(ctrt, levels = c('ALL ', 'BO', 'FO', 'RO', 'B', 'F', 'R', 'O', 'FLW')))
  
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


#group to separate cover crops and fallow

bio_dat2_grouped =
  bio_dat2 %>%
  dplyr::mutate(grouping = if_else(ctrt == "Fallow", "Fallow", "Cover Crop")) 


#means, no reason

biomass_means = 
  bio_dat2 %>% 
  group_by(ctrt) %>% 
  dplyr::mutate(wbio = as.numeric(wbio),
                cbio = as.numeric(cbio)) %>% 
  dplyr::summarise(wbio_mean = round(mean(wbio)),
                   cbio_mean = round(mean(cbio, na.rm = TRUE)))


# bio_dat2_longer = 
#   bio_dat_longer %>% 
#   mutate(ctrt = recode(ctrt, "ALL " = 'All Mixture',
#                        "B" = 'Buckwheat',
#                        "BO" = 'Buckwheat Oat',
#                        "F" = "Faba Bean",
#                        "FLW" = "Fallow",
#                        "FO" = "Faba Bean Oat",
#                        "O" = 'Oat',
#                        "R" = 'Radish',
#                        "RO" = 'Radish Oat')) %>% 
#   # dplyr::mutate(species_group = case_when(grepl("All", ctrt)~"All",
#   #                                grepl("Buckwheat", ctrt)~"B",
#   #                                grepl("Faba", ctrt)~"F",
#   #                                grepl("Radish", ctrt)~"R",
#   #                                grepl("Fallow", ctrt)~"Fal")) 
#   mutate(ctrt = factor(ctrt, levels = c('All Mixture', 'Buckwheat Oat', 'Buckwheat', 
#                                         'Faba Bean Oat', "Faba Bean", 'Radish Oat', 
#                                         "Radish", 'Oat', 'Fallow'))) %>% 
#   # dplyr::mutate(wbio = as.numeric(wbio),
#   #               cbio = as.numeric(cbio)) %>% 
#   # dplyr::mutate(wbio_per_cbio = (wbio/cbio)) %>% 
#   na.omit()



#   
# bio_dat3$species_group[is.na(bio_dat3$species_group)] <- "O"

gh_dat

gh_dat2 =
  gh_dat %>% 
  mutate(ctrt = factor(ctrt, levels = c('ALL ', 'BO', 'B", FO', "F", 'RO', "R", 'O', 'FLW')))

# gh_dat3 = 
#   gh_dat2 %>% 
#   mutate(ctrt = recode(ctrt, "ALL " = 'All Mixture',
#                        "B" = 'Buckwheat',
#                        "BO" = 'Buckwheat Oat',
#                        "F" = "Faba Bean",
#                        "FLW" = "Fallow",
#                        "FO" = "Faba Bean Oat",
#                        "O" = 'Oat',
#                        "R" = 'Radish',
#                        "RO" = 'Radish Oat')) %>% 
#   mutate(sample = "n") %>% 
#   # dplyr::mutate(spec = case_when(grepl("All", ctrt,~"All"),
#   #                                grepl("Buckwheat", ctrt)~"B",
#   #                                grepl("Faba", ID)~"F",
#   #                                grepl("Radish", ID)~"R")) %>% 
#   mutate(ctrt = factor(ctrt, levels = c('All Mixture', 'Buckwheat Oat', 'Buckwheat', 
#                                         'Faba Bean Oat', "Faba Bean", 'Radish Oat', 
#                                         "Radish", 'Oat', 'Fallow'))) %>% 
#   dplyr::mutate(wbio = as.numeric(wbio),
#                 cbio = as.numeric(cbio)) %>% 
#   dplyr::mutate(wbio_per_cbio = (wbio/cbio))



# ggscatter plots

bio_dat2 %>% 
  ggplot(aes(x = cbio, y = wbio))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,4.5)+
  geom_text(x = 15, y = 4.5, label = eq(bio_dat2$cbio,bio_dat2$wbio), parse = TRUE)+
  labs(y = "wheat biomass, grams/pot",
       x = "cover crop biomass, grams/pot")+
  theme_er()+
  NULL

mycolors = c(wes_palette("Darjeeling1"), wes_palette("Royal2"))

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


  

a = bio_dat2 %>%
  filter(ftrt == 'CNTL ' & ctrt == "Faba Bean") %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
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
  scale_fill_manual(values = c('#009474'))+
  scale_shape_manual(values = c(21))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "None")

b = bio_dat2 %>%
  filter(ftrt == 'CNTL ' & ctrt == c("Faba Bean Oat")) %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
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

bio_dat2 %>%
  filter(ftrt == 'CNTL ' & ctrt == "Oat") %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
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

bio_dat2 %>%
  filter(ftrt == 'CNTL ' & ctrt == "Faba Bean Oat") %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
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
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "None")


bio_dat2 %>%
  filter(ctrt == c("All Mixture", "Faba Bean Oat", "Faba Bean", "Oat")) %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
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
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
  facet_wrap(.~ctrt)+
  theme_er()



bio_dat2 %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  filter(ctrt == c("Oat", "Faba Bean", "Faba Bean Oat", "All Mixture")) %>% 
  ggplot(aes(x = as.numeric(cbio), y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5, alpha = 0.75)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
  stat_regline_equation(label.y = 4.75,label.x = 60, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 4.25, label.x = 60, aes(label = ..rr.label..)) +
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
  #facet_grid(.~ftrt)+
  theme_er()





# biomass_means %>% 
#   ggplot(aes(x = cbio_mean, y = wbio_mean))+
#   geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5)+
#   #geom_smooth(method = "lm", se = FALSE, group = 'sample')+
#   #stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
#   #stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
#   #stat_fit_glance(method = 'lm',
#   #               method.args = list(formula = formula),
#   #              geom = 'text',
#   #             aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
#   #            label.x = 15, label.y = 3.5, size = 3)+
#   labs(y = "wheat biomass, grams/pot",
#        x = "cover crop biomass, grams/pot")+
#   #scale_color_manual(values = mycolors)+
#   #scale_color_manual(values = pnw_palette('Shuksan',9))+
#   scale_fill_manual(values = pnw_palette('Shuksan2',9))+
#   #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
#   scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
#   theme_er()



# bio_dat2_grouped %>% 
#   ggplot()+
#   geom_point(aes(x = pbic, y = wbio, color = grouping, shape = ftrt), size = 4, alpha = 0.8)+
#   labs(x = "available P (absolute)", y = "wheat biomass, g/pot")+
#   scale_color_manual(values = pnw_palette("Bay", 2))+
#   facet_wrap(.~ftrt)+
#   theme_er()+
#   theme(legend.position = "bottom")+
#   NULL

bio_dat2_grouped %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       'CNTL ' = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot(aes(x = as.numeric(pbic), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 5, alpha = 0.8)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Available P, mg/kg")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = rev(pnw_palette('Winter', 2)))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,25))+
  ylim(0,4.5)+
  facet_wrap(.~ftrt)+
  theme_er()

g1 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(pbic), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Available P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,60)+
  theme_er()


g2 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(amac), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Reserve P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,100)+
  theme_er()

g3 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(porg), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Organic P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,600)+
  theme_er()

g4 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(unavp), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Fixed P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,400)+
  theme_er()

library(patchwork)
library(cowplot)

g1+g2+g3+g4+ #combines the two plots
   plot_layout(guides = "collect") # sets a common legend

#SOM

bio_dat2_groupedlonger =
  bio_dat2_grouped %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg, caco3, sph2, amm, nit, pmn, pmc, grouping)

p_longer = 
  bio_dat2_groupedlonger %>% 
  select(ftrt, ctrt, time, pbic, amac, unavp, porg) %>% 
  pivot_longer(-c(ftrt, ctrt, time), names_to= "p_pool", values_to= "p_value")

all_combo_plonger =
bio_dat2_groupedlonger %>% 
  select(ftrt, ctrt, time, caco3, sph2, amm, nit, pmn, pmc, grouping) %>% 
  left_join(p_longer) %>% 
  na.omit(.)

all_combo_plonger %>% 
  ggplot(aes(x = as.numeric(sph2), y = as.numeric(p_value)))+
  geom_point(aes(fill = p_pool, shape = p_pool), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "Available P, absolute",
       x = "pH")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',4))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22, 23, 24))+
  facet_wrap(ctrt~.)+
  #ylim(0,4.5)+
  #xlim(0,60)+
  theme_er()


all_combo_plonger %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                               'CNTL' = "Control",
                               'IFERT' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = as.numeric(p_value), fill = p_pool), 
           position = "stack", stat= "identity")+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "Absolute P, mg/kg",
       x = " ")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',4))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22, 23, 24))+
  facet_wrap(.~ftrt)+
  #ylim(0,4.5)+
  #xlim(0,60)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))


b1 = bio_dat2_grouped %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = as.numeric(amm), fill = ctrt), 
           position = "stack", stat= "identity")+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "Ammonium, mg/kg",
       x = " ")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22, 23, 24))+
  facet_wrap(.~ftrt)+
  #ylim(0,4.5)+
  #xlim(0,60)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

#SOM, wheat biomass, and cover crop biomass summary tables.

bio_dat2_grouped = 
  bio_dat2_grouped %>% 
  dplyr::mutate(amm = as.numeric(amm),
                nit = as.numeric(nit),
                pmn = as.numeric(pmn),
                pmc = as.numeric(pmc),
                cbio = as.numeric(cbio),
                wbio = as.numeric(wbio),
                )

SOM_sd = 
bio_dat2_grouped %>% 
  select(ftrt, ctrt, time, nit, amm, pmn, pmc) %>% 
  group_by(ftrt, ctrt) %>% 
  dplyr::summarise(nit_mean = round(mean(nit, na.rm= TRUE)),
            nit_se = round(sd(nit, na.rm= TRUE)/sqrt(n())),
            amm_mean = round(mean(amm, na.rm= TRUE)),
            amm_se = round(sd(amm, na.rm= TRUE)/sqrt(n())),
            pmc_mean = round(mean(pmc, na.rm= TRUE)),
            pmc_se = round(sd(pmc, na.rm= TRUE)/sqrt(n())),
            pmn_mean = round(mean(pmn, na.rm= TRUE)),
            pmn_se = round(sd(pmn, na.rm= TRUE)/sqrt(n())),
            ) 


biomass = 
  bio_dat2_grouped %>% 
  select(ftrt, ctrt, time, wbio, cbio) %>% 
  group_by(ftrt, ctrt) %>% 
  dplyr::summarise(wbio_mean = round(mean(wbio, na.rm= TRUE)),
                   wbio_se = round(sd(wbio, na.rm= TRUE)/sqrt(n())),
                   cbio_mean = round(mean(cbio, na.rm= TRUE)),
                   cbio_se = round(sd(cbio, na.rm= TRUE)/sqrt(n()))
                   ) %>% 
  mutate(wheat = paste(wbio_mean, "\u00b1", wbio_se),
         covercrop = paste(cbio_mean, "\u00b1", cbio_se)) %>% 
  select(ftrt, ctrt, wheat, covercrop) %>% 
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer"))
  
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
      
biomass %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(biomass, "biomass.csv", row.names = FALSE)
                   
   


SOM_sd %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = nit_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.7, color = "gray50")+
  geom_errorbar(aes(x = ctrt, ymin = nit_mean - nit_se, ymax = nit_mean + nit_se), width = .2,
                position = position_dodge(.9), color = 'gray50')+
  labs(y = "Nitrate, mg/kg",
       x = " ")+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

SOM_sd %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = amm_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.7, color = "gray50")+
  geom_errorbar(aes(x = ctrt, ymin = amm_mean - amm_se, ymax = amm_mean + amm_se), width = .2,
                position = position_dodge(.9), color = 'gray50')+
  labs(y = "Ammonium, mg/kg",
       x = " ")+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

SOM_sd %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = pmc_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.7, color = "gray50")+
  geom_errorbar(aes(x = ctrt, ymin = pmc_mean - pmc_se, ymax = pmc_mean + pmc_se), width = .2,
                position = position_dodge(.9), color = 'gray50')+
  labs(y = "Potentially Mineralizable Carbon, mg/kg",
       x = " ")+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

SOM_sd %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = pmn_mean, fill = ctrt), 
           position = "stack", stat= "identity", alpha = 0.7, color = "gray50")+
  geom_errorbar(aes(x = ctrt, ymin = pmn_mean - pmn_se, ymax = pmn_mean + pmn_se), width = .2,
                position = position_dodge(.9), color = 'gray50')+
  labs(y = "Potentially Mineralizable Nitrogen, mg/kg",
       x = " ")+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  facet_wrap(.~ftrt)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

bio_dat2_grouped %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = as.numeric(pmc), fill = ctrt), 
           position = "stack", stat= "identity")+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "Potentially Mineralizable Carbon, mg/kg",
       x = " ")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22, 23, 24))+
  facet_wrap(.~ftrt)+
  #ylim(0,4.5)+
  #xlim(0,60)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))

bio_dat2_grouped %>%
  mutate(ftrt = recode(ftrt, "CMPT" = "Compost",
                       "CNTL " = "Control",
                       'IFERT ' = "Inorganic Fertilizer")) %>% 
  ggplot()+
  geom_bar(aes(x = ctrt, y = as.numeric(pmn), fill = ctrt), 
           position = "stack", stat= "identity")+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "Potentially Mineralizable Nitrogen, mg/kg",
       x = " ")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22, 23, 24))+
  facet_wrap(.~ftrt)+
  #ylim(0,4.5)+
  #xlim(0,60)+
  theme_er()+
  theme(axis.text.x.bottom = element_text(angle = 90))


g2 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(amac), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Reserve P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,100)+
  theme_er()

g3 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(porg), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Organic P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,600)+
  theme_er()

g4 = bio_dat2_grouped %>% 
  ggplot(aes(x = as.numeric(unavp), y = wbio))+
  geom_point(aes(fill = grouping, shape = grouping), color = "black", size = 4, alpha = 0.6)+
  # geom_smooth(method = "lm", se = FALSE, group = 'grouping')+
  # stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  # stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                 label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/pot",
       x = "Fixed P, absolute")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Sunset2',2))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,22))+
  ylim(0,4.5)+
  xlim(0,400)+
  theme_er()

library(patchwork)
library(cowplot)

g1+g2+g3+g4+ #combines the two plots
  plot_layout(guides = "collect") # sets a common legend

#now anova. what a mess

wbio_aov <- aov(wbio ~ grouping, data = bio_dat2_grouped)
summary.aov(wbio_aov)

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


bio_dat3 =
  biodat_2 %>% 
  filter(!cbio == 'Fallow')


bio_dat2 %>%
  ggplot(aes(x = cbio, y = wbio_per_cbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
  stat_regline_equation(label.y = 0.325, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.3, aes(label = ..rr.label..)) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 15, label.y = 3.5, size = 3)+
  labs(y = "wheat biomass, grams/ ccbiomass gram",
       x = "cover crop biomass, grams/pot")+
  #scale_color_manual(values = mycolors)+
  #scale_color_manual(values = pnw_palette('Shuksan',9))+
  scale_fill_manual(values = pnw_palette('Shuksan2',9))+
  #scale_fill_manual(values = c('#009474', "#24492e", "#d7b1c5", "#5d74a5", "#b0cbe7", "#edd746", "#dd4124", "#bf9bdd", "#d8aedd"))+
  scale_shape_manual(values = c(21,18,23,15,22,17,24,3,4))+
  ylim(0,0.35)+
  theme_er()
########

# bio_dat3 %>% 
#   ggscatter(aes(x = cbio, y = wbio, add = 'reg.line'))+
#   stat_cor(label.x = 3, label.y = 34) +
#   stat_regline_equation(label.x = 3, label.y = 32)+
#   labs(y = "wheat biomass, grams/pot",
#        x = "cover crop biomass, grams/pot")+
#   geom_text(x = 2, y = 300, label = eq(bio_dat3$cbio,bio_dat3$wbio), parse = TRUE)+
#   theme_er()+
#   NULL



gh_dat3 %>% 
  

#statspreliminary-------------------






#stats_polished--------------

# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

relabund_table_covertype = 
  fticr_water_relabund_summarized %>% 
  #filter(cover_type == "Open") %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class

fit_aov_open = function(dat){
  
  aov(relabund ~ slopepos, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "slopepos") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    #mutate(cover_type = "Open")   
    force()
  
}

fit_hsd = function(dat){
  a = aov(relabund ~ slopepos, data = dat)
  h = HSD.test(a, "slopepos")
  h$groups %>% mutate(slopepos = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(slopepos, label)
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

relabund_hsd_covertype = 
  fticr_water_relabund %>% 
  #filter(cover_type == "Open") %>% 
  group_by(cover_type, Class) %>% 
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


# Site Position (CANOPY ONLY)


# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

relabund_table_canopy = 
  fticr_water_relabund_summarized %>% 
  filter(cover_type == "Canopy") %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class

fit_aov_canopy = function(dat){
  
  aov(relabund ~ slopepos, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "slopepos") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    mutate(cover_type = "Canopy")   
  
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

relabund_asterisk_canopy = 
  fticr_water_relabund %>% 
  filter(cover_type == "Canopy") %>% 
  group_by(Class) %>% 
  do(fit_aov_canopy(.))

## step 4: combine the summarized values with the asterisks
relabund_table_with_asterisk_canopy = 
  relabund_table_canopy %>% 
  left_join(relabund_asterisk_canopy) %>%
  # combine the values with the asterisk notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         value = str_remove(value, " NA")) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "slopepos", values_from = "value")

relabund_table_with_asterisk_canopy %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_table_with_asterisk_canopy, "output/slopeposcanopy_aovstats.csv", row.names = FALSE)

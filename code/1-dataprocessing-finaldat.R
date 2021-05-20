#Greenhouse incubation
#E Rooney
#5 19 2021

#load packages----------------
source("code/0-packages.R")

#load raw data--------------
incub_dat = read.csv("raw/Inc_finaldata_July2.csv")
bio_dat = read.csv("raw/biocorr_1.csv") %>%  rename('ctrt' = 'ï..ctrt')
gh_dat = read.csv("raw/gh_pfrac.csv") %>%  rename('ctrt' = 'ï..ctrt')

#data processing

# bio_dat2 =
#   bio_dat %>% 
#   mutate(ctrt = factor(ctrt, levels = c('ALL ', 'BO', 'FO', 'RO', 'B', 'F', 'R', 'O', 'FLW')))
  
bio_dat2 = 
  bio_dat %>% 
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
  dplyr::mutate(wbio_per_cbio = (wbio/cbio)) %>% 
  na.omit()
#   
# bio_dat3$species_group[is.na(bio_dat3$species_group)] <- "O"

gh_dat

gh_dat2 =
  gh_dat %>% 
  mutate(ctrt = factor(ctrt, levels = c('ALL ', 'BO', 'B", FO', "F", 'RO', "R", 'O', 'FLW')))

gh_dat3 = 
  gh_dat2 %>% 
  mutate(ctrt = recode(ctrt, "ALL " = 'All Mixture',
                       "B" = 'Buckwheat',
                       "BO" = 'Buckwheat Oat',
                       "F" = "Faba Bean",
                       "FLW" = "Fallow",
                       "FO" = "Faba Bean Oat",
                       "O" = 'Oat',
                       "R" = 'Radish',
                       "RO" = 'Radish Oat')) %>% 
  mutate(sample = "n") %>% 
  # dplyr::mutate(spec = case_when(grepl("All", ctrt,~"All"),
  #                                grepl("Buckwheat", ctrt)~"B",
  #                                grepl("Faba", ID)~"F",
  #                                grepl("Radish", ID)~"R")) %>% 
  mutate(ctrt = factor(ctrt, levels = c('All Mixture', 'Buckwheat Oat', 'Buckwheat', 
                                        'Faba Bean Oat', "Faba Bean", 'Radish Oat', 
                                        "Radish", 'Oat', 'Fallow'))) %>% 
  dplyr::mutate(wbio = as.numeric(wbio),
                cbio = as.numeric(cbio)) %>% 
  dplyr::mutate(wbio_per_cbio = (wbio/cbio))



# ggscatter plots

bio_dat2 %>% 
  ggplot(aes(x = cbio, y = wbio))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,4.5)+
  geom_text(x = 15, y = 4.5, label = eq(bio_dat3$cbio,bio_dat3$wbio), parse = TRUE)+
  labs(y = "wheat biomass, grams/pot",
       x = "cover crop biomass, grams/pot")+
  theme_er()+
  NULL

mycolors = c(wes_palette("Darjeeling1"), wes_palette("Royal2"))

bio_dat2 %>% 
  ggplot(aes(x = cbio, y = wbio))+
  geom_point(aes(fill = ctrt, shape = ctrt), color = "black", size = 5)+
  geom_smooth(method = "lm", se = FALSE, group = 'sample')+
  stat_regline_equation(label.y = 4, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 3.75, aes(label = ..rr.label..)) +
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
  ylim(0,4.5)+
  theme_er()


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

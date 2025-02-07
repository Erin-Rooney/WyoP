# UWYO cover crop and compost results in R
# ERIN ROONEY
# MAY 26 2021

################################################## #

# `c-functions_ggplots.R`

################################################## #

## this script will load functions for:
## (a) relabund ggplots

# INSTRUCTIONS:
## source this file in the `5_drakeplan.R` file, do not run the script here.
## This script can (generally) be used as is for most data that follow this format. No modifications needed 

## edit: all files are unique to this experiment because this is a unique experiment.


################################################## #
################################################## #

# 1. PROCESSING FUNCTIONS -------------------------------------------------
## LEVEL I FUNCTIONS -------------------------------------------------------


relabund_plot = function(p_relabund_summary){
  
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
}

relabund_sample_plot = function(p_relabund_by_sample){
  p_relabund_by_sample %>% 
  filter(time != "Baseline" & ctrt != "Control") %>% 
  ggplot()+
  geom_boxplot(aes(x = ctrt, y = abund, fill = ctrt), alpha = 0.6)+  
  scale_fill_manual(values = (wes_palette("Zissou1",5)))+
  facet_grid(ftrt~phosphorus_pool)+
  labs(x = "", y = 'phosphorus concentration')+
  theme_er()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}


###phosphorus (abs) geom points

pbic_abs_plot = function(standardized_p){
  standardized_p %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}


t1_pbic_abs_plot = function(standardized_p){
standardized_p %>%
  filter(ctrt != "Fallow" & time == "1") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}

t2_pbic_abs_plot = function(standardized_p){
standardized_p %>%
  filter(ctrt != "Fallow" & time == "2") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = pbic, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "available P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}
  
t1_amac_abs_plot = function(standardized_p){
standardized_p %>%
  filter(ctrt != "Fallow" & time == "1") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = amac, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "reserve P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}

t2_amac_abs_plot = function(standardized_p){
standardized_p %>%
  filter(ctrt != "Fallow" & time == "2") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = amac, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "reserve P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}


porg_abs_plot = function(standardized_p){
standardized_p %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = porg, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "organic P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}

amac_abs_plot = function(standardized_p){
standardized_p %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = amac, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "reserve P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}

unavp_abs_plot = function(standardized_p){
  
standardized_p %>%
  filter(ctrt != "Fallow") %>% 
  ggplot()+
  geom_point(aes(x = cbio, y = unavp, color = ftrt, shape = ftrt), size = 4, alpha = 0.8)+
  labs(y = "unavailable P (absolute)", x = "cover crop biomass, g/pot")+
  scale_color_manual(values = pnw_palette("Sunset2", 3))+
  facet_wrap(.~ctrt)+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL
}
#




### RELABUND

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
  dplyr::summarise(relabund_mean = mean(relabund))

p_relabund_summary %>% 
  filter(ctrt != "Control") %>% 
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
  
#
table = 
  p_relabund_summary %>% 
  filter(phosphorus_pool == "amac_percbio" & time == "T3") %>% 
  pivot_wider(names_from = ftrt, values_from = relabund_mean)
  
  


################


## setting up aov function
aov_fit -- relabund ~ ftrt

relabund_bysample %>% 
group_by(ctrt, phosphorus_pool) %>% 
  do(aov_fit(.))


## setting up hsd function
hsd_fit -- relabund ~ ctrt

relabund_bysample %>% 
  group_by(ftrt, phosphorus_pool) %>% 
  do(hsd_fit(.))
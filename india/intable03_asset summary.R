pcall <- readRDS(paste0(path_harmonization_folder,"/india/working/pcall.RDS"))

var_order <- c('c_crowding', 
               'd_airconditioner', 
               'd_bed', 
               'd_bicycle', 
               'd_bullockcart', 
               'd_cable', 
               'd_car', 
               'd_cellphone', 
               'd_chair', 
               'd_clock', 
               'd_computer', 
               'd_cooler', 
               'd_dishtv', 
               'd_electricity', 
               'd_fan', 
               'd_housetype', 
               'd_internet', 
               'd_mattress', 
               'd_mixergrinder', 
               'd_ownhouse', 
               'd_pressurecooker', 
               'd_radio', 
               'd_refrigerator', 
               'd_separatekitchen', 
               'd_sewingmachine', 
               'd_stove', 
               'd_table', 
               'd_telephone', 
               'd_television', 
               'd_televisionbw', 
               'd_televisioncolor', 
               'd_televisionplasma', 
               'd_thresher', 
               'd_tractor', 
               'd_twowheeler', 
               'd_washingmachine', 
               'd_waterpump', 
               'f_drinkingwater', 
               'f_drinkingwatershared', 
               'f_generalwater', 
               'f_generalwatershared', 
               'f_housetype', 
               'f_lighttype', 
               'f_sharedtoilet', 
               'f_toilettype'

               
)

tab03 <- pcall %>% 
  dplyr::select(-id) %>% 
  # Does not contain n_ and c_
  dplyr::select(year,starts_with("f_"),starts_with("d_"),starts_with("r_")) %>%
  
  mutate(non_na_assets = apply(.[,2:length(colnames(.))],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
  dplyr::filter(non_na_assets > 0) %>% 
  dplyr::select(-non_na_assets) %>% 
  mutate_at(vars(starts_with("d_")), function(x) factor(x,levels=c(0,1),labels=c("No","Yes"))) %>% 
  mutate_at(vars(-year),~as.character(.)) %>% 
  pivot_longer(cols=-year,names_to="variable",values_to="value") %>% 
  group_by(year,variable) %>% 
  mutate(n_total = n(),
         n_nonna = sum(!is.na(value))) %>% 
  ungroup() %>% 
  group_by(year,variable,n_total,n_nonna,value) %>% 
  tally(name="n_value") %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(prop_value = round_d(n_value*100/n_nonna,1)) %>% 
  dplyr::filter(!value %in% c("No")) %>% 
  ungroup() %>% 
  dplyr::select(year,variable,value,prop_value) %>% 
  
  pivot_wider(names_from = year,values_from=prop_value) %>% 
  mutate_at(vars(-one_of("value","variable")),function(x) case_when(is.na(x) ~ "",
                                                                    TRUE ~ x)) %>% 
  mutate(variable = factor(variable,levels=var_order,ordered=TRUE)) %>% 
  arrange(variable)


write.csv(tab03,paste0(path_harmonization_folder,"/india/working/table03.csv"))

pcall <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pcall.RDS"))

var_order <- c("d_car",
               "d_computer",
               "d_duplexfridge",
               "d_dvd",
               "d_housekeeper",
               "d_radio",
               "d_refrigerator",
               "d_television",
               "d_vaccumcleaner",
               "d_washingmachine",
               "f_pipedwater",
               
               "d_houseownership",
               "f_housingmaterial",
               "f_toilet",
               
               "d_aircon",
               "d_cleaninglady",
               "d_clothesdryer",
               "d_desktop",
               "d_dishwasher",
               "d_internet",
               "d_microwave",
               "d_motorcycle",
               "d_notebook",
               "d_stereo",
               "d_streetpaved",
               "d_videogame"
               
               )

tab03 <- pcall %>% 
  dplyr::select(-nquest) %>% 
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
  
  
write.csv(tab03,paste0(path_harmonization_folder,"/brazil 1993/working/table03.csv"))

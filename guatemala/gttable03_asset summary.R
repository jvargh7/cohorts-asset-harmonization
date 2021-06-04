pcall <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pcall.RDS"))

var_order <- read_csv(paste0(path_harmonization_folder,"/guatemala/working/cs all items indices loadings.csv")) %>% 
  dplyr::select(-iteration) %>% 
  pivot_longer(cols=-one_of(c("year")),names_to="variable",values_to="PC1") %>% 
  mutate(year = case_when(year == 0 ~ "Harmonized",
                          TRUE ~ paste0("CS",as.character(year)))) %>% 
  pivot_wider(names_from="year",values_from = "PC1") %>% 
  dplyr::select(variable) %>% 
  mutate(variable = str_replace(variable,"p_","f_")) %>% 
  pull()

tab03 <- pcall %>%
  dplyr::filter(census!=2004) %>% 
  # Does not contain n_ and c_
  dplyr::select(census,starts_with("f_"),starts_with("d_"),starts_with("r_")) %>%
  
  mutate(non_na_assets = apply(.[,2:length(colnames(.))],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
  dplyr::filter(non_na_assets > 0) %>% 
  dplyr::select(-non_na_assets) %>% 
  mutate_at(vars(starts_with("d_"),starts_with("r_")), function(x) factor(x,levels=c(0,1),labels=c("No","Yes"))) %>% 
  mutate_at(vars(-census),~as.character(.)) %>% 
  pivot_longer(cols=-census,names_to="variable",values_to="value") %>% 
  group_by(census,variable) %>% 
  mutate(n_total = n(),
         n_nonna = sum(!is.na(value))) %>% 
  ungroup() %>% 
  group_by(census,variable,n_total,n_nonna,value) %>% 
  tally(name="n_value") %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(prop_value = round_d(n_value*100/n_nonna,1)) %>% 
  dplyr::filter(!value %in% c("No")) %>% 
  ungroup() %>% 
  dplyr::select(census,variable,value,prop_value) %>% 
  
  pivot_wider(names_from = census,values_from=prop_value) %>% 
  mutate_at(vars(-one_of("value","variable")),function(x) case_when(is.na(x) ~ "",
                                                                    TRUE ~ x)) %>% 
  mutate(variable = factor(variable,levels=var_order,ordered=TRUE)) %>% 
  dplyr::filter(!is.na(variable)) %>% 
  arrange(variable)


write.csv(tab03,paste0(path_harmonization_folder,"/guatemala/working/table03.csv"))

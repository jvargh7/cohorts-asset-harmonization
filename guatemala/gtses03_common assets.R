

availability_pcall <- pcall %>% 
  dplyr::select(census,starts_with("p_"),starts_with("d_"),c_crowding) %>% 
  group_by(census) %>% 
  # summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  summarize_all(.funs=function(x)sum(x!=0)) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("census"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)


years = c(1967,1975,1987,1996,2002,2016,2018)

pca_vars <-availability_pcall %>%
  dplyr::filter(census %in% years ) %>% 
  dplyr::select(census,var_name) %>%
  group_by(var_name) %>%
  tally() %>% 
  # dplyr::filter(n >=length(years)-1,!var_name %in% exclude_vars) %>% 
  dplyr::filter(n >=length(years)-1) %>% 
  dplyr::select(var_name) %>% 
  pull()

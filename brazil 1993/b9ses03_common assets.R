
pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,
                # starts_with("r_"),
                starts_with("n_")) %>% 
  compareGroups::compareGroups(year ~ .,data=.,max.ylev = 10) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()

availability_pcall <- pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding) %>% 
  group_by(year) %>% 
  summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("year"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)

exclude_vars = c("d_internet")



# Selection ----------

years = c(1997,2004,2008,2011,2015)

pca_vars <- availability_pcall %>%
  dplyr::filter(year %in% years ) %>% 
  dplyr::select(year,var_name) %>%
  group_by(var_name) %>%
  tally() %>% 
  dplyr::filter(n >= length(years)-1,!var_name %in% exclude_vars) %>%  #
  dplyr::select(var_name) %>% 
  pull()

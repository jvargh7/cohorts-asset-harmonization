
pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_")) %>% 
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

# Low variance exclusions --------
exclude_vars = c("d_housetype",
                 # "d_mixergrinder",
                 "d_stove",
                 # "d_washingmachine",
                 # "p_drinkingwater",
                 # "p_drinkingwatershared",
                 # "p_generalwater",
                 # "p_generalwatershared",
                 
                 "d_electricity","d_fan"
)                                 
                 


# Selection ----------

years = c(1999,2006,2012,2016)

pca_vars <- availability_pcall %>%
  dplyr::filter(year %in% years,!var_name %in% exclude_vars ) %>%
  # dplyr::filter(year %in% years) %>%
  dplyr::select(year,var_name) %>%
  group_by(var_name) %>%
  tally() %>% 
  dplyr::filter(n >= length(years)-1) %>% 
  dplyr::select(var_name) %>% 
  pull()


pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_")) %>% 
  compareGroups::compareGroups(year ~ .,data=.,max.ylev = 10) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()

availability_pcall <- pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_")) %>% 
  group_by(year) %>% 
  summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("year"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)

# exclude_vars = c("p_housingmaterial","p_toilet",
#                  "p_occupation","d_motorcycle",
#                  "d_stereo","d_internet","d_houseownership",
#                  "d_videogame","d_aircon","d_desktop","d_notebook",
#                  "d_microwave","d_cleaninglady","d_dishwasher",
#                  "d_clothesdryer","d_streetpaved")



# Selection ----------

years = c(1990,1997,2002,2006,2012,2018)

pca_vars <- availability_pcall %>%
  dplyr::filter(year %in% years ) %>% 
  dplyr::select(year,var_name) %>%
  group_by(var_name) %>%
  tally() %>% 
  dplyr::filter(n >= length(years)-1) %>% 
  dplyr::select(var_name) %>% 
  pull()

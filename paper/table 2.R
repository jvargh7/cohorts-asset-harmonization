b9_df <- read.csv(paste0(path_harmonization_folder,"/brazil 1993/working/cs indices loadings.csv")) %>% 
  dplyr::filter(year == 0)

gt_df <- read.csv(paste0(path_harmonization_folder,"/guatemala/working/cs indices loadings.csv")) %>% 
  dplyr::filter(year == 0) %>% 
  rename(d_twowheeler = d_motorcycle)
  

in_df <- read.csv(paste0(path_harmonization_folder,"/india/working/cs indices loadings.csv")) %>% 
  dplyr::filter(year == 0) %>% 
  rename(p_pipedwater = p_drinkingwater,
         p_toilet = p_sharedtoilet)

ph_df <- read.csv(paste0(path_harmonization_folder,"/philippines/working/cs indices loadings.csv")) %>% 
  dplyr::filter(year == 0) %>% 
  rename(d_television = r_tv,
         d_airconditioner = d_aircon,
         p_pipedwater = p_sourcedw,
         p_stove = p_cookfuel,
         d_electricity = d_electric)

sa_df <- read.csv(paste0(path_harmonization_folder,"/south africa/working/cs indices loadings.csv")) %>% 
  dplyr::filter(year == 0) %>% 
  rename(d_television = d_tv,
         p_toilet = r_toilet2groups,
         p_pipedwater = r_water2groups)


table2 <- bind_rows(b9_df %>% 
            mutate(country = "Brazil"),
          gt_df %>% 
            mutate(country = "Guatemala"),
          in_df %>% 
            mutate(country = "India"),
          ph_df %>% 
            mutate(country = "Philippines"),
          sa_df %>% 
            mutate(country = "South Africa")) %>% 
  dplyr::select(-year,-iteration) %>% 
  pivot_longer(cols=-one_of("country"),names_to="var",values_to="loading") %>% 
  dplyr::filter(!is.na(loading)) %>% 
  pivot_wider(names_from="country",values_from="loading")

write.csv(table2,paste0(path_dissertation,"/aim 2/working/cohorts/table2.csv"),row.names = FALSE)

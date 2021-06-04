
sa_early_life <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "soweto") %>% 
  dplyr::mutate(bttid = pin - 50000000) %>% 
  dplyr::select(bttid,chsex,
                chbirtho,
                moage,moscho,fascho) %>% 
  
  left_join(read_csv(paste0(path_harmonization_folder,"/south africa/Cohort ethnicity.csv")) %>% 
              dplyr::select(bttid,ethnicity) %>% 
              mutate(ethnicity = factor(ethnicity,levels=c(1:4),labels=c("white","black","coloured","indian"))))


saveRDS(sa_early_life,paste0(path_harmonization_folder,"/south africa/working/early_life.RDS"))

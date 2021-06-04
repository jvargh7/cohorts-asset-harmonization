
in_early_life <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/NDBC 1969-1972 variables Jithin.sav")) %>% 
  dplyr::select(id,religion) %>% 
  left_join(readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "delhi") %>% 
  dplyr::mutate(id = pin - 30000000) %>% 
  dplyr::select(id,chsex,
                chbirtho,
                moage,moscho,fascho),
  by="id") %>% 

# Merge with birth year
  left_join(haven::read_sav(paste0(path_harmonization_folder,"/india/data/Date of Birth NDBC.sav")) %>% 
              dplyr::select(id,doby),
            by="id") %>% 

  mutate(religion = factor(religion,levels=c(1:6),labels=c("Hindu","Muslim","Sikh","Jain","Christian","Others")))


saveRDS(in_early_life,paste0(path_harmonization_folder,"/india/working/early_life.RDS"))

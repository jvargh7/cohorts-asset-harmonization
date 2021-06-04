early_life <- readRDS(paste0(path_harmonization_folder,"/south africa/working/early_life.RDS"))

pca_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS"))

table_df <- early_life %>% 
  mutate(status1997 = case_when(bttid %in% pca_df[pca_df$year==1997,]$bttid ~ 1,
                                TRUE ~ 0),
         status2002 = case_when(bttid %in% pca_df[pca_df$year==2002,]$bttid ~ 1,
                                TRUE ~ 0),
         status2006 = case_when(bttid %in% pca_df[pca_df$year==2006,]$bttid ~ 1,
                                TRUE ~ 0),
         status2012 = case_when(bttid %in% pca_df[pca_df$year==2012,]$bttid ~ 1,
                                TRUE ~ 0),
         status2018 = case_when(bttid %in% pca_df[pca_df$year==2018,]$bttid ~ 1,
                                TRUE ~ 0)
  ) %>% 
  pivot_longer(names_to = "year",values_to = "status",cols = starts_with("status")) %>% 
  mutate(year = str_replace(year,"status","") %>% as.numeric(.),
         status = factor(status,levels = c(0,1),labels=c("Not Available or Died","Available")))

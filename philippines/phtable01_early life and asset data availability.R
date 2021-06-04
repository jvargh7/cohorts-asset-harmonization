

early_life <- readRDS(paste0(path_harmonization_folder,"/philippines/working/early_life.RDS"))

pca_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS"))

table_df <- early_life %>% 
  mutate(status1983 = case_when(uncchdid %in% pca_df[pca_df$year==1983,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status1991 = case_when(uncchdid %in% pca_df[pca_df$year==1991,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status1994 = case_when(uncchdid %in% pca_df[pca_df$year==1994,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status1998 = case_when(uncchdid %in% pca_df[pca_df$year==1998,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status2002 = case_when(uncchdid %in% pca_df[pca_df$year==2002,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status2005 = case_when(uncchdid %in% pca_df[pca_df$year==2005,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status2009 = case_when(uncchdid %in% pca_df[pca_df$year==2009,]$uncchdid ~ 1,
                                TRUE ~ 0),
         status2018 = case_when(uncchdid %in% pca_df[pca_df$year==2018,]$uncchdid ~ 1,
                                TRUE ~ 0)
         
  ) %>% 
  pivot_longer(names_to = "year",values_to = "status",cols = starts_with("status")) %>% 
  mutate(year = str_replace(year,"status","") %>% as.numeric(.),
         status = factor(status,levels = c(0,1),labels=c("Not Available or Died","Available")))


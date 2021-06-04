early_life <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/early_life.RDS"))

pca_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS"))

table_df <- early_life %>% 
  mutate(status1997 = case_when(nquest %in% pca_df[pca_df$year==1997,]$nquest ~ 1,
                                TRUE ~ 0),
         status2004 = case_when(nquest %in% pca_df[pca_df$year==2004,]$nquest ~ 1,
                                TRUE ~ 0),
         status2008 = case_when(nquest %in% pca_df[pca_df$year==2008,]$nquest ~ 1,
                                TRUE ~ 0),
         status2011 = case_when(nquest %in% pca_df[pca_df$year==2011,]$nquest ~ 1,
                                TRUE ~ 0),
         status2015 = case_when(nquest %in% pca_df[pca_df$year==2015,]$nquest ~ 1,
                                TRUE ~ 0)
         ) %>% 
  pivot_longer(names_to = "year",values_to = "status",cols = starts_with("status")) %>% 
  mutate(year = str_replace(year,"status","") %>% as.numeric(.),
         status = factor(status,levels = c(0,1),labels=c("Not Available or Died","Available")))

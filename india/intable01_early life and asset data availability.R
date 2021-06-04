early_life <- readRDS(paste0(path_harmonization_folder,"/india/working/early_life.RDS"))

source(paste0(path_harmonization_repo,"/india/in_participation in first wave.R"))

pca_df <- readRDS(paste0(path_harmonization_folder,"/india/working/pca_df_unimputed.RDS"))

table_df <- early_life %>% 
  mutate(status1969 = case_when(id %in% assets1969_ids ~ 1,
                                TRUE ~ 0),
         status1999 = case_when(id %in% pca_df[pca_df$year==1999,]$id ~ 1,
                                TRUE ~ 0),
         status2006 = case_when(id %in% pca_df[pca_df$year==2006,]$id ~ 1,
                                TRUE ~ 0),
         status2012 = case_when(id %in% pca_df[pca_df$year==2012,]$id ~ 1,
                                TRUE ~ 0),
         status2016 = case_when(id %in% pca_df[pca_df$year==2016,]$id ~ 1,
                                TRUE ~ 0)
  ) %>% 
  pivot_longer(names_to = "year",values_to = "status",cols = starts_with("status")) %>% 
  mutate(year = str_replace(year,"status","") %>% as.numeric(.),
         status = factor(status,levels = c(0,1),labels=c("Not Available or Died","Available")))

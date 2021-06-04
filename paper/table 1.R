
b8_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1982/working/pca_cs.RDS"))
b9_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS"))
gt_df <- readRDS(paste0(path_incap_ses_dfa,"/ses_masters w96.RDS"))
in_df <- readRDS(paste0(path_harmonization_folder,"/india/working/pca_df_unimputed.RDS"))
ph_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS"))
sa_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS"))


b8_df %>% 
  group_by(year) %>% 
  tally() %>% 
  mutate(p = n/5914)


b9_df %>% 
  group_by(year) %>% 
  tally() %>% 
  mutate(p = n/5901)

# gt_df %>% 
#   group_by(census) %>% 
#   tally() %>% 
#   mutate(p = n/2392)
# 1967: 547/816 (816 unique mothers)
# 1975: 755/816

table(!is.na(gt_df$pcall1987_1))/2392
table(!is.na(gt_df$pcall1996_1))/2392
table(!is.na(gt_df$pcall2002_1))/2392
table(!is.na(gt_df$pcall2016_1))/2392
table(!is.na(gt_df$pcall2018_1))/2392



in_df %>% 
  group_by(year) %>% 
  tally() %>% 
  mutate(p = n/8181)

ph_df %>% 
  group_by(year) %>% 
  tally() %>% 
  mutate(p = n/3080)

sa_df %>% 
  group_by(year) %>% 
  tally() %>% 
  mutate(p = n/3273)


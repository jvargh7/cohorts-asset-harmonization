

early_life <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/early_life.RDS"))

pca_df <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS"))

response_df <- readRDS(paste0(path_incap_ses_dfa,"/response status.RDS")) %>% 
  dplyr::select(id_uni,starts_with("missing")) %>% 
  rename_at(vars(starts_with("missing")),function(x) str_replace(x,"missing","status"))

table_df <- early_life %>% 
  left_join(response_df,
            by = "id_uni"
         
  ) %>% 
  pivot_longer(names_to = "year",values_to = "status",cols = starts_with("status")) %>% 
  mutate(year = str_replace(year,"status","") %>% as.numeric(.)) %>% 
  mutate(status = case_when(status == "Missing" ~ "Data Not Available",
                            TRUE ~ as.character(status)))


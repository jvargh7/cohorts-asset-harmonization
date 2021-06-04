
ph_early_life <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "cebu") %>% 
  dplyr::mutate(uncchdid = pin - 40000000) %>% 
  dplyr::select(uncchdid,
                chsex,
                moage,moage1st,
                moht,momarst,
                moscho,
                chbirtho,
                pascho,
                chincomec3,
                chsoclass,
                chtoilet,
                chwater)



# SAVE ------------
saveRDS(ph_early_life,paste0(path_harmonization_folder,"/philippines/working/early_life.RDS"))
write_dta(ph_early_life,paste0(path_harmonization_folder,"/philippines/working/early_life.dta"),version=12)





source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))

ses_region <- full_join(
  meta_master %>% 
    left_join(pca_region %>% 
                dplyr::filter(census == 2016),
              by = c("id_selected"="id_uni")) %>%
    dplyr::select(id_uni,ur,pcr:pcr3) %>%
    dplyr::rename(pcall2016_1 = pcr,
                  pcall2016_2 = pcr2,
                  pcall2016_3 = pcr3,
                  ur2016 = ur),
  gates_master %>% 
    left_join(pca_region %>% 
                dplyr::filter(census == 2018),
              by = c("id_selected"="id_uni")) %>% 
    dplyr::select(id_uni,ur,pcr:pcr3) %>%
    dplyr::rename(pcall2018_1 = pcr,
                  pcall2018_2 = pcr2,
                  pcall2018_3 = pcr3,
                  ur2018 = ur),
  
  by = "id_uni") %>% 
  dplyr::filter(!(is.na(ur2016)&is.na(ur2018)))

with(ses_region,table(ur2016,ur2018,useNA="always"))
          # ur2018
# ur2016   1      2   <NA>
  #   1    262    0    52
  #   2      7    770   70
  # <NA>    72    154    1




# SAVE ---------------
saveRDS(ses_region,paste0(path_harmonization_folder,"/guatemala/working/ses_region for COHORTS.RDS"))
write_dta(ses_region,paste0(path_harmonization_folder,"/guatemala/working/ses_region for COHORTS.dta"),version=12)




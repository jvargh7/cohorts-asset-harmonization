gt_early_life <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "guatemala") %>% 
  dplyr::mutate(id_uni = pin - 20000000) %>% 
  dplyr::select(id_uni,chsex,
                # gtchvillage,
                chbirtho, chwt0,
                gtatole,gtchatoleexposurestatus,
                moage,moht,moscho,
                momarst,mopow,mopowq,
                pascho,gtchbyear
  )  %>% 
  left_join(read_dta(paste0(path_local_working,"/Processed Data/Stata datasets/gtm_structural.dta")) %>% 
              dplyr::select(site_pin,d_id_unim),
            by=c("id_uni"="site_pin"))  %>% 
  left_join(readRDS(paste0(path_incap_ses_dfa,"/ses_masters.RDS")) %>% 
              dplyr::select(id_uni,comun),
            by = "id_uni") %>% 
  mutate(gtvillage = factor(comun,levels=c(3,6,8,14),labels=c("FR_ES","AT_CO","FR_SD","AT_SJ"))) %>% 
  group_by(d_id_unim) %>% 
  mutate(moht_sib = case_when(is.na(moht) ~ mean(moht,na.rm=TRUE),
                              TRUE ~ moht),
         moscho_sib = case_when(is.na(moscho) ~ mean(moscho,na.rm=TRUE),
                                TRUE ~ moscho)
         
  ) %>% 
  ungroup() 


gt_early_life <- gt_early_life %>% 
  # left_join(incap_moage_sib,
  #           by = "d_id_unim") %>% 
  # mutate(moage_sib = case_when(is.na(moage) ~ moage1962 - 62 + gtchbyear,
  #                              TRUE ~ moage)) %>% 
  group_by(gtvillage) %>% 
  mutate(moht_imputed = case_when(is.na(moht_sib) ~ mean(moht_sib,na.rm=TRUE),
                                  TRUE ~ moht_sib),
         moscho_imputed = case_when(is.na(moscho_sib) ~ mean(moscho_sib,na.rm=TRUE),
                                    TRUE ~ moscho_sib),
         moage_imputed = case_when(is.na(moage) ~ mean(moage,na.rm=TRUE),
                                   TRUE ~ moage),
         
  ) %>% 
  ungroup() %>% 
  dplyr::select(-comun)

attr(gt_early_life$moht_imputed,"label") <- "(Imputed in 2020) Maternal Height"
attr(gt_early_life$moscho_imputed,"label") <- "(Imputed in 2020) Maternal Schooling"
attr(gt_early_life$moage_imputed,"label") <- "(Imputed in 2020) Maternal Age"


# SAVE ------------
saveRDS(gt_early_life,paste0(path_harmonization_folder,"/guatemala/working/early_life.RDS"))
write_dta(gt_early_life,paste0(path_harmonization_folder,"/guatemala/working/early_life.dta"),version=12)
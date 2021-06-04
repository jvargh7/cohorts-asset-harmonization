ph_region <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta")) %>% 
  dplyr::filter(survey %in% c(0.5,1991,1994,1998,2002,2005,2007,2009)) %>% 
  mutate(survey = case_when(survey == 0.5 ~ 1983,
                            TRUE ~ survey)) %>% 
  dplyr::filter(!is.na(cstratum)) %>% 
  dplyr::select(survey,uncchdid,cstratum) %>% 
  plyr::rbind.fill(assets18c %>% 
              dplyr::select(uncchdid,currstra) %>% 
              rename(cstratum = currstra) %>% 
              mutate(survey = 2018))


# CSTRATUM	From Baseline to 1994: Was sample mother’s or caregiver’s barangay of residence for that survey  urban or rural?
#   
# From 1998 onwards: Was index child’s barangay of residence for that survey urban or rural?
#   
# 1	Urban
# 2	Rural















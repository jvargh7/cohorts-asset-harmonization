phase3data <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/phase3data.sav")) %>% 
  rename(id = Id,
         sex = O3sex,
         adwt = O3wt,
         adht = O3ht,
         adfat_percentage = O3fatp,
         adfm = O3fatm,
         adffm = O3fatfm) %>% 
  mutate(adwc = apply(.[,c("O3wstci1","O3wstci2")],1,function(x) mean(x,na.rm=TRUE))) %>% 
  dplyr::select(id,sex,starts_with("ad")) %>% 
  mutate(adbmi = case_when(!is.na(adht) & !is.na(adwt) ~ adwt/(adht/100)^2,
                           TRUE ~ NA_real_),
         sex = factor(sex,levels=c(0,1),labels=c("Male","Female"))
         )
  
phase4data <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/phase4data.sav")) %>% 
  rename(id = id,
         sex = sexphase4,
         # adwt = O3wt,
         # adht = O3ht,
         adfat_percentage = o4fatp,
         adfm = o4fatm,
         adffm = o4fatfm) %>% 
  mutate(adwc = apply(.[,c("waist1","waist2","waist3")],1,function(x) mean(x,na.rm=TRUE)),
         adht = apply(.[,c("phase4ht1","phase4ht2","phase4ht3")],1,function(x) mean(x,na.rm=TRUE)),
         adwt = apply(.[,c("phase4wt1","phase4wt2","phase4wt3")],1,function(x) mean(x,na.rm=TRUE))
         ) %>% 
  dplyr::select(id,sex,starts_with("ad")) %>% 
  mutate(adbmi = case_when(!is.na(adht) & !is.na(adwt) ~ adwt/(adht/100)^2,
                           TRUE ~ NA_real_),
         sex = factor(sex,levels=c(1,2),labels=c("Male","Female")))

ndbc_aph <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/NDBC APH 3 & 4 variables.sav"))

in_adult_outcomes <- bind_rows(ndbc_aph %>% 
                                 dplyr::select(Id,O3doe,O3marita,O3numchild,O3child,O3educat,O3occupa) %>% 
                                 rename(id = Id,
                                        date_of_examination = O3doe,
                                        marital_status = O3marita,
                                        any_children = O3numchild,
                                        num_children = O3child,
                                        educat = O3educat,
                                        occupation = O3occupa
                                        ) %>% 
                                 mutate(non_na_rows = apply(.[,c("marital_status","any_children","educat","occupation")],
                                                            1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
                                 dplyr::filter(non_na_rows > 0) %>% 
                                 dplyr::select(-non_na_rows) %>% 
                                 mutate(year = 2012) %>% 
                                 left_join(phase3data,by="id")
                                 
                                 ,
                               ndbc_aph %>% 
                                 dplyr::select(Id,doe,numchild,child_total,edu, occ_cohort,fam_type) %>% 
                                 rename(id = Id,
                                        date_of_examination = doe,
                                        # marital_status = O3marita,
                                        any_children = numchild,
                                        num_children = child_total,
                                        educat = edu,
                                        occupation = occ_cohort,
                                        type_family = fam_type
                                 ) %>% 
                                 mutate(non_na_rows = apply(.[,c("any_children","educat","occupation","type_family")],
                                                            1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
                                 dplyr::filter(non_na_rows > 0) %>% 
                                 dplyr::select(-non_na_rows) %>% 
                                 mutate(year = 2016)  %>% 
                                 left_join(phase4data,by="id")) %>% 
  mutate(educat_years = case_when(educat == 1 ~ 0,
                                educat == 2 ~ 5,
                                educat == 3 ~ 8,
                                educat == 4 ~ 10,
                                educat == 5 ~ 12,
                                educat == 6 ~ 15,
                                educat == 7 ~ 18,
                                TRUE ~ NA_real_),
         occupation = factor(occupation,levels=c(1:8),
                             labels = c("Homemaker","Unemployed",
                                        "Unskilled Manual","Semi-skilled Manual",
                                        "Skilled Manual","Clerical","Professional","Other")),
         marital_status = factor(marital_status,levels=c(1:5),labels=c("single","married","widowed","separated","married"))
         )

saveRDS(in_adult_outcomes,paste0(path_harmonization_folder,"/india/working/adultoutcomes.RDS"))
          
  
  

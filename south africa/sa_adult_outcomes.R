

bt20_gates <- readRDS(paste0(path_local_working,"/Processed Data/R datasets/cohorts_ses.RDS")) %>% 
  dplyr::filter(site == "BT20") %>% 
  dplyr::select(site_pin,age,jobcurr,relstat) %>%
  mutate(relstat = as.character(relstat)) %>% 
  mutate(relstat = factor(relstat,labels=c("Single","Dating","In relationship","Living together",
                                           "Married","Divorced/Separated","Widow/widower"))) %>% 
  rename(bttid = site_pin,
         age = age,
         employed_currently = jobcurr,
         relstat = relstat) %>% 
  left_join(
    
    readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
      dplyr::filter(site == "soweto") %>% 
      dplyr::mutate(bttid = pin - 50000000) %>% 
      dplyr::select(bttid,
                    chsex,
                    adeduyr,
                    
                    
                    saadravenstotscore2018,
                    saadsrq2018,
                    saadhappy2018) %>% 
      rename(schooling = adeduyr,
             ravens = saadravenstotscore2018,
             srq = saadsrq2018,
             happy = saadhappy2018
             ),
    by = "bttid"
  )


bt20_yas <- haven::read_dta(paste0(path_harmonization_folder,"/south africa/Jithin data.dta")) %>% 
  dplyr::select(bttid,yas_age,marital_status,
                educ_schoolyearretro,have_job) %>% 
  mutate(marital_status = factor(marital_status,levels=c(1:6),labels=c("Single","In relationship","Living together",
                                                                       "Married","Divorced/Separated","Widow/widower"))) %>% 
  rename(age = yas_age,
         relstat = marital_status,
         schooling = educ_schoolyearretro,
         employed_currently = have_job
  ) %>% 
  left_join(
    readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
      dplyr::filter(site == "soweto") %>% 
      dplyr::mutate(bttid = pin - 50000000) %>% 
      dplyr::select(bttid,
                    chsex,saadbmi2012) %>% 
      rename(bmi = saadbmi2012),
    by = "bttid"
    
    
    
  )


adultoutcomes <- bind_rows(bt20_yas %>% 
                                 mutate(year = 2012),
                               bt20_gates %>% 
                                 mutate(year = 2018))


source(paste0(path_harmonization_repo,"/package/label_variables.R"))
sapcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/south africa/SA Variable List.xlsx"),sheet="pcall")


adultoutcomes <- label_variables(adultoutcomes,cohorts_labels = sapcall_labels,overwrite_var_label = TRUE)


saveRDS(adultoutcomes,paste0(path_harmonization_folder,"/south africa/working/adultoutcomes.RDS"))
write_dta(adultoutcomes,paste0(path_harmonization_folder,"/south africa/working/adultoutcomes.dta"),version=12)
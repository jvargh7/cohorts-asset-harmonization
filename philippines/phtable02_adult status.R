source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
source(paste0(path_harmonization_repo,"/philippines/ph_region.R"))


table2_df <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "cebu") %>% 
  dplyr::mutate(uncchdid = pin - 40000000) %>% 
  dplyr::select(uncchdid,
                chsex,
                ademployment,
                adschooling,
                
                adrelstat,
                
                phadbmi2018,
                phadravenstotscore2018,
                phadsrq2018,
                phadhappy2018) %>% 
  dplyr::filter(!is.na(phadbmi2018)|!is.na(phadravenstotscore2018)|!is.na(phadsrq2018)|!is.na(phadhappy2018)) %>% 
  left_join(ph_region %>% dplyr::filter(survey == 2018),
            by = c("uncchdid")) %>% 
  mutate(cstratum = factor(cstratum,labels=c("Urban","Rural"))) %>% 
  dplyr::filter(!is.na(cstratum))

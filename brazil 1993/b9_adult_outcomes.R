# outcome_vars <- c("limcbod","lestnutbod",
#                   "lescoljovemano","lescoljovemcat","lsrqtot",
#                   "lsrqdic","lbestarcont","lbestarq",
#                   "ld006","ld008","ld009","ld011","lfilhos")

b9_cohorts_dfa_adultoutcomes <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "pelotas1993") %>% 
  dplyr::select(b9numero,chsex) %>% 
  mutate(nquest = b9numero + 930000) %>% 
  dplyr::select(-b9numero)


b9_pregnancy <- read_dta(paste0(path_harmonization_folder,"/brazil 1993/Dataset_2020_01_20.dta")) %>% 
  dplyr::select(nquest,lgravida_antro) %>% 
  mutate(year = 2015)



# Called in b9ses02_merge ---------------


adultoutcomes <- pcall %>% 
  dplyr::select(nquest,year,lifestatus, age,income,
                p_occupation,f_occupation,
                lifestatus, children, iq, 
                contains("employed"),
                contains("schooling"),
                contains("bmi"),
                contains("srq"),
                contains("wellbeing")
  ) %>% 
  left_join(b9_pregnancy,
            by=c("nquest","year")) %>% 
  mutate_at(vars(employed_ever,employed_lastmonth,employed_currently),
            .funs = function(x) {case_when(x==0 ~ 0,
                                           x==0 ~ 1,
                                           TRUE ~ NA_real_)}) %>% 
  mutate(employed_lastyear = case_when(employed_lastyear == 0 ~ 0,
                                       employed_lastyear == 1 ~ 1,
                                       TRUE ~ NA_real_),
         pregnant_anthro = case_when(lgravida_antro == 0 ~ 0,
                                     lgravida_antro == 1 ~ 1,
                                     TRUE ~ NA_real_), 
         bmi_category = case_when(bmi_category == 2 ~ 4,
                                  bmi_category == 1 ~ 3,
                                  bmi_category == 0 ~ 2,
                                  TRUE ~ NA_real_),
         srq_category = case_when(srq_category == 0 ~ 0,
                                  srq_category == 1 ~ 1,
                                  TRUE ~ NA_real_),
         wellbeing_category = as.numeric(wellbeing_category)
  ) %>% 
  left_join(b9_cohorts_dfa_adultoutcomes,
            by = "nquest")

pcall <- pcall %>% 
  dplyr::select(-one_of(names(adultoutcomes)[-c(1,2)]))


source(paste0(path_harmonization_repo,"/package/label_variables.R"))
b9pcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/brazil 1993/B9 Variable List.xlsx"),sheet="pcall")


adultoutcomes <- label_variables(adultoutcomes,cohorts_labels = b9pcall_labels,overwrite_var_label = TRUE)


saveRDS(adultoutcomes,paste0(path_harmonization_folder,"/brazil 1993/working/adultoutcomes.RDS"))
write_dta(adultoutcomes,paste0(path_harmonization_folder,"/brazil 1993/working/adultoutcomes.dta"),version=12)

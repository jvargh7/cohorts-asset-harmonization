
# COHORTS DATA FOR ANALYSIS ------------
b9_cohorts_dfa_earlylife <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "pelotas1993") %>% 
  dplyr::select(b9numero,b9fascho1993,b9fascho1998,b9fawork1993,
                b9fpond,b9hhscho1993,b9moage1993,b9mopregsmoke,
                b9moemployment1994,b9moemployment1998,
                b9moscho1993,b9moscho1998,b9moscho2011,chsex,chbirtho,
                b9moskincolor) %>% 
  mutate(nquest = b9numero + 930000) %>% 
  dplyr::select(-b9numero)

# STRUCTURAL ------------
b9_structural <- haven::read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::select(nquest,sex,samplewgt,arenfam,
                ac3mtscho,ac3ptscho,amatskco,aidadmae,aidadpai,
                atrabfor,atrabpai,jcorpel5) %>% 
  mutate(sex = case_when(sex == 2 ~ 0,
                         sex == 1 ~ 1,
                         TRUE ~ NA_real_)) %>% 
  mutate(moskincolor = case_when(amatskco == 1 ~ 1,
                                 amatskco == 2 ~ 2,
                                 amatskco == 3 ~ 3,
                                 TRUE ~ NA_real_),
         adskincolor = case_when(jcorpel5 == 1 ~ 1,
                                 jcorpel5 == 2 ~ 2,
                                 jcorpel5 %in% c(3:5) ~ 3,
                                 TRUE ~ NA_real_)) %>% 
  mutate_at(vars(one_of("moskincolor","adskincolor")), ~ factor(.,levels=c(1,2,3),labels=c("white","black","other"))) %>% 
  mutate(moemployedpreg = factor(atrabfor,levels=c(1:4),labels=c("yes","no","student","stay at home")),
         moemployedcurr = factor(atrabpai,levels=c(1:5),labels=c("yes","no","retired","leaning","student")),
         sex = factor(sex,levels=c(0,1),labels=c("Female","Male"))
  ) %>% 
  dplyr::select(-amatskco,-jcorpel5,-atrabfor,-atrabpai) %>% 
  rename(samplewt = samplewgt,
         income = arenfam,
         moscho = ac3mtscho,
         fascho = ac3ptscho,
         moage = aidadmae,
         faage = aidadpai)

# MERGE -----------
invalid_ids = c(936398)
b9_early_life <- left_join(b9_structural,
                           b9_cohorts_dfa_earlylife,
                           by="nquest") %>% 
  dplyr::filter(!nquest %in% invalid_ids) %>% 
  dplyr::select(nquest,
                contains("age"),
                contains("scho"),
                contains("employ"),
                everything())

# LABELS (MANUAL) -----------  

var_label(b9_early_life$moskincolor) <- "Maternal skin color"
var_label(b9_early_life$adskincolor) <- "Index child skin color"

# SAVE ------------
saveRDS(b9_early_life,paste0(path_harmonization_folder,"/brazil 1993/working/early_life.RDS"))
write_dta(b9_early_life,paste0(path_harmonization_folder,"/brazil 1993/working/early_life.dta"),version=12)

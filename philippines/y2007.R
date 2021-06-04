

hhold2007c <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2007/child/hhold.dta"))
hhold2007m <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2007/mother/hhold.dta"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hhold2007c,type="dta")
dictionary_file(hhold2007m,type="dta")

variables07 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y2007)) %>% 
  dplyr::select(question,variable,y2007)

# Does not have n_ variables. 
# Only asks for ownership
items07 <- variables07 %>% 
  dplyr::filter(!(y2007 %in% c("ownhouse","ownlots",
                               "numbper","tabsent",
                               "hhldtype")))

# CHILD ---------------------
assets07c <- hhold2007c %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    # brgay07,
    # hhnumb07,
    # woman07,
    
    uncchdid,
    momch07,
    uncmomid,

    one_of(variables07$y2007)
    
  ) %>% 
  
  # ID ---------------
rename(
  # brgy2007 = brgay07,
  # hhid2007 = hhnumb07,
  # wmanid2007 = woman07,
  
  # uncchdid,
  mochint2007 = momch07,
  # uncmomid,
  
  # Status variables are missing
  # houseneat = housneat
  
) %>% 
  
  # rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables07$y2007)),~variables07$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------

  # ASSETS ------------
rename_at(vars(aircon:washingm),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_washingm),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) 

# MOTHER ---------------------
assets07m <- hhold2007m %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    # brgay07,
    # hhnumb07,
    # woman07,
    
    uncchdid,
    momch07,
    uncmomid,
    
    one_of(variables07$y2007)
    
  ) %>% 
  
  # ID ---------------
rename(
  # brgy2007 = brgay07,
  # hhid2007 = hhnumb07,
  # wmanid2007 = woman07,
  
  # uncchdid,
  mochint2007 = momch07,
  # uncmomid,
  
  # Status variables are missing
  # houseneat = housneat
  
) %>% 
  
  # rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables07$y2007)),~variables07$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------

# ASSETS ------------
rename_at(vars(aircon:washingm),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_washingm),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) 



# DESCRIPTIVES ----------------
assets07m %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

assets07c %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

# SAVE -------------------
saveRDS(assets07m,paste0(path_harmonization_folder,"/philippines/working/assets07m.RDS"))
saveRDS(assets07c,paste0(path_harmonization_folder,"/philippines/working/assets07c.RDS"))

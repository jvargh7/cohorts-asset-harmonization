
hholdc <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2002/zip_child/hhold.dta"))
hholdm <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2002/zip_mother/hhold.dta"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hholdc,type="dta")
dictionary_file(hholdm,type="dta")

variables02 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y2002)) %>% 
  dplyr::select(question,variable,y2002)


items02 <- variables02 %>% 
  dplyr::filter(!(y2002 %in% c("ownhouse","ownlots","renovatn",
                               "numbper","tabsent","roomnumb",
                               "rental","feerent","rentfree",
                               "valhouse")))


# CHILD ---------------------
assets02c <- hholdc %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    brgay02,
    hhnumb02,
    woman02,
    
    uncchdid,
    momch02,
    uncmomid,
    
    
    sourcedw,
    toilet,
    garbage,
    litetype,
    cookfuel,
    foodstor,
    gencond,
    neighcon,
    neighgar,
    areafood,
    settlemn,
    resarea,
    area50m,
    electric,
    elecserv,
    material,
    neighmat,
    housneat,
    # bathrom2, #Inside or not
    
    one_of(variables02$y2002)
    
  ) %>% 
  
  # ID ---------------
rename(
  brgy2002 = brgay02,
  hhid2002 = hhnumb02,
  wmanid2002 = woman02,
  
  # uncchdid,
  mochint2002 = momch02,
  # uncmomid,

  # Status variables are missing
  houseneat = housneat,
  foodpurchase = foodstor,
  settlement = settlemn
 ) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables02$y2002)),~variables02$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
       f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
       f_garbage = poly_asset_cat(p_garbage, w = c(5:6), m = c(2,7), b = c(1,3,4)),
       f_litetype = poly_asset_cat(p_litetype, w = c(3,5),m = c(2,6),b = c(1,4)),
       
       f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:6), m = c(2), b = c(1,3)),
       
       f_gencond = poly_asset_cat(p_gencond,w = c(1), m = c(2,3), b = c(4)),
       f_neighcon = poly_asset_cat(p_neighcon,w = c(1), m = c(2,3), b = c(4)),
       f_neighgar = poly_asset_cat(p_neighgar,w = c(1), m = c(2,3), b = c(4)),
       
       f_areafood = poly_asset_cat(p_areafood, w = c(3), m = c(2), b = c(1)),
       f_resarea = factor(p_resarea,levels=c(1:5),labels=c("Mostly residential houses",
                                                           "Mostly commercial buildings",
                                                           "Mostly open space, used for farming and/or livestock",
                                                           "Mostly open space, not used",
                                                           "Mostly factories/manufacturing/industrial buildings")),
       f_area50m = factor(p_area50m,levels=c(1:5),labels=c("Mostly residential houses",
                                                           "Mostly commercial buildings",
                                                           "Mostly open space, used for farming and/or livestock",
                                                           "Mostly open space, not used",
                                                           "Mostly factories/manufacturing/industrial buildings")),
       f_neighmat = poly_asset_cat(p_neighmat,w = c(1), m = c(2), b = c(3)),
       f_houseneat = poly_asset_cat(p_houseneat, w = c(3), m = c(2), b = c(1)),
       f_material = poly_asset_cat(p_material,w = c(1), m = c(2), b = c(3))
       
) %>% 
  dplyr::select(-p_resarea,-p_area50m) %>% 
  
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
         p_toilet = poly_asset_num(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
         p_garbage = poly_asset_num(p_garbage, w = c(5:6), m = c(2,7), b = c(1,3,4)),
         p_litetype = poly_asset_num(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:6), m = c(2), b = c(1,3)),
         
         p_gencond = poly_asset_num(p_gencond,w = c(1), m = c(2,3), b = c(4)),
         p_neighcon = poly_asset_num(p_neighcon,w = c(1), m = c(2,3), b = c(4)),
         p_neighgar = poly_asset_num(p_neighgar,w = c(1), m = c(2,3), b = c(4)),
         
         p_areafood = poly_asset_num(p_areafood, w = c(3), m = c(2), b = c(1)),
         p_neighmat = poly_asset_num(p_neighmat,w = c(1), m = c(2), b = c(3)),
         p_houseneat = poly_asset_num(p_houseneat, w = c(3), m = c(2), b = c(1)),
         p_material = poly_asset_num(p_material,w = c(1), m = c(2), b = c(3))
         
  ) %>% 
  rename(d_electric = p_electric,
         d_elecserv = p_elecserv) %>% 
  
  # ASSETS ------------
  bind_cols(.,
            identity(.) %>% 
              dplyr::select(one_of(items02$variable)) %>% 
              rename_at(vars(one_of(items02$variable)),~paste0("n_",.)) %>% 
              dplyr::select(-n_cellphone,-n_telephone) %>% 
              mutate_all(~count2count_asset(.,na_vals = c(-8:-1)))
            ) %>% 
  rename_at(vars(one_of(items02$variable)),~paste0("d_",.)) %>% 
  mutate_at(vars(one_of(paste0("d_",items02$variable))),~count2bin_asset(.)) %>% 
  mutate_at(vars(d_electric,d_elecserv),~binary_asset(.)) %>% 
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) %>% 
  mutate(d_ownhouse = case_when(d_ownhouse == -4 ~ NA_real_,
                                TRUE ~ d_ownhouse)) %>% 
  mutate(c_crowding = case_when(numbper >= 0 & roomnumb > 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))


# MOTHER ---------------------
assets02m <- hholdm %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    brgay02,
    hhnumb02,
    woman02,
    
    # uncchdid,
    momch02,
    uncmomid,
    
    
    sourcedw,
    toilet,
    garbage,
    litetype,
    cookfuel,
    foodstor,
    gencond,
    neighcon,
    neighgar,
    areafood,
    settlemn,
    resarea,
    area50m,
    electric,
    elecserv,
    material,
    neighmat,
    housneat,
    # bathrom2, #Inside or not
    
    one_of(variables02$y2002)
    
  ) %>% 
  
  # ID ---------------
rename(
  currbrgy = brgay02,
  hhid2002 = hhnumb02,
  wmanid2002 = woman02,
  
  # uncchdid,
  mochint2002 = momch02,
  # uncmomid,
  
  # Status variables are missing
  houseneat = housneat,
  foodpurchase = foodstor,
  settlement = settlemn
  
) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables02$y2002)),~variables02$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
       f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
       f_garbage = poly_asset_cat(p_garbage, w = c(4:6), m = c(2,7), b = c(1,3)),
       f_litetype = poly_asset_cat(p_litetype, w = c(3,5),m = c(2,6),b = c(1,4)),
       
       f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:6), m = c(2), b = c(1,3)),
       
       f_gencond = poly_asset_cat(p_gencond,w = c(1), m = c(2,3), b = c(4)),
       f_neighcon = poly_asset_cat(p_neighcon,w = c(1), m = c(2,3), b = c(4)),
       f_neighgar = poly_asset_cat(p_neighgar,w = c(1), m = c(2,3), b = c(4)),
       
       f_areafood = poly_asset_cat(p_areafood, w = c(3), m = c(2), b = c(1)),
       f_resarea = factor(p_resarea,levels=c(1:5),labels=c("Mostly residential houses",
                                                           "Mostly commercial buildings",
                                                           "Mostly open space, used for farming and/or livestock",
                                                           "Mostly open space, not used",
                                                           "Mostly factories/manufacturing/industrial buildings")),
       f_area50m = factor(p_area50m,levels=c(1:5),labels=c("Mostly residential houses",
                                                           "Mostly commercial buildings",
                                                           "Mostly open space, used for farming and/or livestock",
                                                           "Mostly open space, not used",
                                                           "Mostly factories/manufacturing/industrial buildings")),
       f_neighmat = poly_asset_cat(p_neighmat,w = c(1), m = c(2), b = c(3)),
       f_houseneat = poly_asset_cat(p_houseneat, w = c(3), m = c(2), b = c(1)),
       f_material = poly_asset_cat(p_material,w = c(1), m = c(2), b = c(3))
       
) %>% 
  dplyr::select(-p_resarea,-p_area50m) %>% 
  
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
         p_toilet = poly_asset_num(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
         p_garbage = poly_asset_num(p_garbage, w = c(4:6), m = c(2,7), b = c(1,3)),
         p_litetype = poly_asset_num(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:6), m = c(2), b = c(1,3)),
         
         p_gencond = poly_asset_num(p_gencond,w = c(1), m = c(2,3), b = c(4)),
         p_neighcon = poly_asset_num(p_neighcon,w = c(1), m = c(2,3), b = c(4)),
         p_neighgar = poly_asset_num(p_neighgar,w = c(1), m = c(2,3), b = c(4)),
         
         p_areafood = poly_asset_num(p_areafood, w = c(3), m = c(2), b = c(1)),
         p_neighmat = poly_asset_num(p_neighmat,w = c(1), m = c(2), b = c(3)),
         p_houseneat = poly_asset_num(p_houseneat, w = c(3), m = c(2), b = c(1)),
         p_material = poly_asset_num(p_material,w = c(1), m = c(2), b = c(3))
         
  ) %>% 
  rename(d_electric = p_electric,
         d_elecserv = p_elecserv) %>% 
  
  # ASSETS ------------
  bind_cols(.,
            identity(.) %>% 
              dplyr::select(one_of(items02$variable)) %>% 
              rename_at(vars(one_of(items02$variable)),~paste0("n_",.))  %>% 
              dplyr::select(-n_cellphone,-n_telephone) %>% 
              mutate_all(~count2count_asset(.,na_vals = c(-8:-1)))
  ) %>% 
  rename_at(vars(one_of(items02$variable)),~paste0("d_",.)) %>% 
  mutate_at(vars(one_of(paste0("d_",items02$variable))),~count2bin_asset(.)) %>% 
  mutate_at(vars(d_electric,d_elecserv),~binary_asset(.)) %>% 
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) %>% 
  mutate(d_ownhouse = case_when(d_ownhouse == -4 ~ NA_real_,
                                TRUE ~ d_ownhouse)) %>% 
  mutate(c_crowding = case_when(numbper >= 0 & roomnumb > 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))

# DESCRIPTIVES ----------------
assets02m %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_"),starts_with("n_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

assets02c %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

# SAVE -------------------
saveRDS(assets02m,paste0(path_harmonization_folder,"/philippines/working/assets02m.RDS"))
saveRDS(assets02c,paste0(path_harmonization_folder,"/philippines/working/assets02c.RDS"))

# MOTHER DATA ---------
mother <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2002/zip_mother/mother.dta")) %>% 
  dplyr::select(basebrgy,basewman,uncmomid,MOMCH02,iclnumb)

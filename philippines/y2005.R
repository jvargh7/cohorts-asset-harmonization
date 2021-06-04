
hholdc <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2005/child/hhold.dta"))
hholdm <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2005/mother/hhold.dta"))


source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hholdc,type="dta")
dictionary_file(hholdm,type="dta")

variables05 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y2005)) %>% 
  dplyr::select(question,variable,y2005)

items05 <- variables05 %>% 
  dplyr::filter(!(y2005 %in% c("ownhouse","ownlots","renovatn",
                               "numbper","tabsent","roomnumb",
                               "rental","feerent","rentfree","valhouse")))

# CHILD ---------------------
assets05c <- hholdc %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    brgay05,
    hhnumb05,
    woman05,
    
    uncchdid,
    momch05,
    # uncmomid,
    
    
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
    
    one_of(variables05$y2005)
    
  ) %>% 
  
  # ID ---------------
rename(
  brgy2005 = brgay05,
  hhid2005 = hhnumb05,
  wmanid2005 = woman05,
  
  # uncchdid,
  mochint2005 = momch05,
  # uncmomid,
  
  # Status variables are missing
  houseneat = housneat,
  foodpurchase = foodstor,
  settlement = settlemn
  
) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables05$y2005)),~variables05$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
       f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
       f_garbage = poly_asset_cat(p_garbage, w = c(4:6), m = c(2,7), b = c(1,3)),
       f_litetype = poly_asset_cat(p_litetype, w = c(3,5),m = c(2,6),b = c(1,4)),
       
       f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:6), m = c(2), b = c(0,1,3)), # 0 is no cooking
       
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
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:6), m = c(2), b = c(0,1,3)),  # 0 is no cooking
         
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
              dplyr::select(one_of(items05$variable)) %>% 
              rename_at(vars(one_of(items05$variable)),~paste0("n_",.)) %>% 
              # n_cellphone: asks for number of members with cellphones
              dplyr::select(-n_cellphone,-n_telephone) %>% 
              mutate_all(~count2count_asset(.,na_vals = c(-8:-1)))
  ) %>% 
  rename_at(vars(one_of(items05$variable)),~paste0("d_",.)) %>% 
  mutate_at(vars(one_of(paste0("d_",items05$variable))),~count2bin_asset(.)) %>% 
  mutate_at(vars(d_electric,d_elecserv),~binary_asset(.)) %>% 
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) %>% 
  mutate(c_crowding = case_when(numbper >= 0 & roomnumb > 0~ roomnumb/numbper,
                                TRUE ~ NA_real_))

# MOTHER ---------------------
assets05m <- hholdm %>% 
  rename_all(~str_to_lower(.)) %>% 
  # CORRECTING FOR BICYLE --> BICYCLE
  rename(bicyle = bicycle) %>% 
  
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    brgay05,
    hhnumb05,
    woman05,
    
    # uncchdid,
    momch05,
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
    
    one_of(variables05$y2005)
    
  ) %>% 
  
  # ID ---------------
rename(
  brgy2005 = brgay05,
  hhid2005 = hhnumb05,
  wmanid2005 = woman05,
  
  # uncchdid,
  mochint2005 = momch05,
  # uncmomid,
  
  # Status variables are missing
  houseneat = housneat,
  foodpurchase = foodstor,
  settlement = settlemn
  
) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables05$y2005)),~variables05$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
       f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
       f_garbage = poly_asset_cat(p_garbage, w = c(4:6), m = c(2,7), b = c(1,3)),
       f_litetype = poly_asset_cat(p_litetype, w = c(3,5),m = c(2,6),b = c(1,4)),
       
       f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:6), m = c(2), b = c(0,1,3)), # 0 is no cooking
       
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
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:6), m = c(2), b = c(0,1,3)),  # 0 is no cooking
         
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
              dplyr::select(one_of(items05$variable)) %>% 
              rename_at(vars(one_of(items05$variable)),~paste0("n_",.)) %>% 
              # n_cellphone: asks for number of members with cellphones
              dplyr::select(-n_cellphone,-n_telephone) %>% 
              mutate_all(~count2count_asset(.,na_vals = c(-8:-1)))
  ) %>% 
  rename_at(vars(one_of(items05$variable)),~paste0("d_",.)) %>% 
  mutate_at(vars(one_of(paste0("d_",items05$variable))),~count2bin_asset(.)) %>% 
  mutate_at(vars(d_electric,d_elecserv),~binary_asset(.)) %>% 
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) %>% 
  mutate(c_crowding = case_when(numbper >= 0 & roomnumb > 0~ roomnumb/numbper,
                                TRUE ~ NA_real_))



# DESCRIPTIVES ----------------
assets05m %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_"),starts_with("n_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

assets05c %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_"),starts_with("n_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

# SAVE -------------------
saveRDS(assets05m,paste0(path_harmonization_folder,"/philippines/working/assets05m.RDS"))
saveRDS(assets05c,paste0(path_harmonization_folder,"/philippines/working/assets05c.RDS"))








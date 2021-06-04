
hholdfe <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/females/hhold.dta"))
hholdma <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/males/hhold.dta"))


source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hholdfe,type="dta")
dictionary_file(hholdma,type="dta")

variables09 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y2009)) %>% 
  dplyr::select(question,variable,y2009)

# Does not have n_ variables. 
# Only asks for ownership
items09 <- variables09 %>% 
  dplyr::filter(!(y2009 %in% c("ownhouse","ownlots",
                               "numbper","tabsent",
                               "hhldtype")))

# FEMALE ---------------------
assets09fe <- hholdfe %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    uncchdid,
    # momch07,
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
    # elecserv,
    material,
    neighmat,
    housneat,
    # bathrom2, #Inside or not
    
    one_of(variables09$y2009)
    
  ) %>% 
  
  # ID ---------------
rename(
  # Status variables are missing
  houseneat = housneat,
  foodpurchase = foodstor,
  settlement = settlemn
  
) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables09$y2009)),~variables09$variable) %>% 
  
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
  rename(d_electric = p_electric) %>% 
  
  # ASSETS ------------
rename_at(vars(aircon:washingm),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_washingm),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) 

# MALE ---------------------
assets09ma <- hholdma %>% 
  rename_all(~str_to_lower(.)) %>% 
  # CORRECTING FOR BICYLE --> BICYCLE
  # rename(bicyle = bicycle) %>% 
  
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    uncchdid,
    # momch07,
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
    # elecserv,
    material,
    neighmat,
    housneat,
    # bathrom2, #Inside or not
    
    one_of(variables09$y2009)
    
  ) %>% 
  
  # ID ---------------
rename(
  # Status variables are missing
  houseneat = housneat,
  foodpurchase = foodstor,
  settlement = settlemn
  
) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables09$y2009)),~variables09$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:10),m = c(4,5), b = c(1,2,3)),
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
  
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(6:10),m = c(4,5), b = c(1,2,3)),
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
  rename(d_electric = p_electric) %>% 
  
  # ASSETS ------------
rename_at(vars(aircon:washingm),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_washingm),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot) 



# DESCRIPTIVES ----------------
assets09ma %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

assets09fe %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

# SAVE -------------------
saveRDS(assets09ma,paste0(path_harmonization_folder,"/philippines/working/assets09ma.RDS"))
saveRDS(assets09fe,paste0(path_harmonization_folder,"/philippines/working/assets09fe.RDS"))








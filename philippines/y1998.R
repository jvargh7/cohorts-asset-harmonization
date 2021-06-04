hholdb <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/1998-1999/zip_MomBoys/hhold.dta"))
hholdg <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/1998-1999/zip_MomGirls/hhold.dta"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hholdb,type="dta")
dictionary_file(hholdg,type="dta")



variables98 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y1998)) %>% 
  dplyr::select(question,variable,y1998)

# BOYS ------------

assets98b <- hholdb %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    currbrgy,
    currstra,
    basebrgy,
    basehhno,
    basewman,

    mobrgy94,
    mohhno94,
    mowman94,
    chbrgy94,
    chhhno94,
    chwman94,
    
    brgay98,
    hhnumb98,
    woman98,
    uncmomid,
    
    momhh94,
    chldhh94,
    momch98,
    status94,
    status98,
    
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
    insideh,
    # bathrom2, #Inside or not
    
    one_of(variables98$y1998)
    
  ) %>% 
  
  # ID ---------------
rename(
       wmanbrgy1994 = mobrgy94,
       wmanhhid1994 = mohhno94,
       wmanid1994 = mowman94,
       
       chldbrgy1994 = chbrgy94,
       chldhhid1994 = chhhno94,
       chldwmanid1994 = chwman94,
       
       brgy1998 = brgay98,
       hhid1998 = hhnumb98,
       wmanid1998 = woman98,
       # uncmomid,
       momint1994 = momhh94,
       chint1994 = chldhh94,
       mochint1998 = momch98,
       status1994 = status94,
       status1998 = status98,
       houseneat = insideh,
       foodpurchase = foodstor,
       settlement = settlemn
       ) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables98$y1998)),~variables98$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
       f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
       f_garbage = poly_asset_cat(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
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
         p_garbage = poly_asset_num(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
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
  rename_at(vars(aircon:videocasrec),~paste0("d_",.)) %>% 
  mutate(n_carabaos = d_carabaos,
         n_chicken = d_chicken,
         n_cows = d_cows,
         n_goats = d_goats,
         n_pigs = d_pigs,
         n_otheranimals = d_otheranimals
  ) %>% 
  
  mutate_at(vars(d_carabaos,d_chicken,
                 d_cows,d_goats,d_pigs,
                 d_otheranimals), ~count2bin_asset(.)) %>%
  
  mutate_at(vars(d_aircon:d_videocasrec,d_electric,d_elecserv),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse) %>% 
  mutate(c_crowding = case_when(numbper >= 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))

# GIRLS ------------

assets98g <- hholdg %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    currbrgy,
    currstra,
    basebrgy,
    basehhno,
    basewman,
    
    mobrgy94,
    mohhno94,
    mowman94,
    chbrgy94,
    chhhno94,
    chwman94,
    
    brgay98,
    hhnumb98,
    woman98,
    uncmomid,
    
    momhh94,
    chldhh94,
    momch98,
    status94,
    status98,
    
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
    insideh,
    # bathrom2, #Inside or not
    
    one_of(variables98$y1998)
    
  ) %>% 
  
  # ID ---------------
rename(
  wmanbrgy1994 = mobrgy94,
  wmanhhid1994 = mohhno94,
  wmanid1994 = mowman94,
  
  chldbrgy1994 = chbrgy94,
  chldhhid1994 = chhhno94,
  chldwmanid1994 = chwman94,
  
  brgy1998 = brgay98,
  hhid1998 = hhnumb98,
  wmanid1998 = woman98,
  # uncmomid,
  
  momint1994 = momhh94,
  chint1994 = chldhh94,
  mochint1998 = momch98,
  status1994 = status94,
  status1998 = status98,
  
  houseneat = insideh,
  foodpurchase = foodstor,
  settlement = settlemn
) %>% 
  
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables98$y1998)),~variables98$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
       f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
       f_garbage = poly_asset_cat(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
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
         p_garbage = poly_asset_num(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
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
  rename_at(vars(aircon:videocasrec),~paste0("d_",.)) %>% 
  mutate(n_carabaos = d_carabaos,
         n_chicken = d_chicken,
         n_cows = d_cows,
         n_goats = d_goats,
         n_pigs = d_pigs,
         n_otheranimals = d_otheranimals
  ) %>% 
  
  mutate_at(vars(d_carabaos,d_chicken,
                 d_cows,d_goats,d_pigs,
                 d_otheranimals), ~count2bin_asset(.)) %>%
  
  mutate_at(vars(d_aircon:d_videocasrec,d_electric,d_elecserv),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse) %>% 
  mutate(c_crowding = case_when(numbper >= 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))

# DESCRIPTIVES ----------------
assets98b %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

assets98g %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

# SAVE -------------------
saveRDS(assets98b,paste0(path_harmonization_folder,"/philippines/working/assets98b.RDS"))
saveRDS(assets98g,paste0(path_harmonization_folder,"/philippines/working/assets98g.RDS"))

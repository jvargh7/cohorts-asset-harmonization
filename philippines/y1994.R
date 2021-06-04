hhold <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/1994-1995/zip_household/hhold.dta"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hhold,type="dta")

variables94 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y1994)) %>% 
  dplyr::select(question,variable,y1994)


assets94 <- hhold %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    curbrgy2,
    curstra2,
    
    basebrgy,
    # basehhno,
    basewman,
    basehhn2,
    
    momsbrg2,
    momshhn2,
    momswma2,
    
    
    chldbrg2,
    chldhhn2,
    chldwma2,
    
    brgay942,
    hhnum942,
    woman942,
    
    momsh912,
    chldh912,
    momch942,
   
    statu912,
    statu942,
    
    sourswa2,
    toilet2,
    garbage2,
    litetyp2,
    cookfue2,
    storfod2,
    gencond2,
    neighco2,
    neighga2,
    areafod2,
    settlem2,
    resarea2,
    areai502,
    electri2,
    elecser2,
    materia2,
    neighma2,
    # bathrom2, #Inside or not
    
    one_of(variables94$y1994)
    
  ) %>% 
  
  # ID ---------------
rename(currbrgy = curbrgy2, # with(assets94,table(currbrgy == brgy1994)): TRUE (2483)
       currstra = curstra2,
       # basewman,
       basehhno = basehhn2,
       
       wmanbrgy1991 = momsbrg2,
       wmanhhid1991 = momshhn2,
       wmanid1991 = momswma2,
       
       chldbrgy1991 = chldbrg2,
       chldhhid1991 = chldhhn2,
       chldwmanid1991 = chldwma2,
       
       brgy1994 = brgay942,
       hhid1994 = hhnum942,
       wmanid1994 = woman942,
       
       moint1991 = momsh912,
       chint1991 = chldh912,
       mochint1994 = momch942,

       status1991 = statu912,
       status1994 = statu942) %>% 
  
  rename(sourcedw = sourswa2,
         toilet = toilet2,
         garbage = garbage2,
         litetype = litetyp2,
         cookfuel = cookfue2,
         foodpurchase = storfod2,
         
         gencond = gencond2,
         neighcon = neighco2,
         neighgar = neighga2,
         
         areafood = areafod2,
         settlement = settlem2,
         resarea = resarea2,
         area50m = areai502,
         electric = electri2,
         elecserv = elecser2,
         
         material = materia2,
         # houseneat = housene2,
         neighmat = neighma2,
         # bathroom = bathrom2
         
  )   %>% 
  rename_at(vars(sourcedw:neighmat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables94$y1994)),~variables94$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
  mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8),m = c(4,5), b = c(1,2,3)),
         f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
         f_garbage = poly_asset_cat(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
         f_litetype = poly_asset_cat(p_litetype, w = c(3,5),m = c(2,6),b = c(1,4)),
         
         f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:5), m = c(2), b = c(0,1,3)), #0 is does not cook
         
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
         # f_houseneat = poly_asset_cat(p_houseneat, w = c(3), m = c(2), b = c(1)),
         f_material = poly_asset_cat(p_material,w = c(1), m = c(2), b = c(3))
         
  ) %>% 
  dplyr::select(-p_resarea,-p_area50m) %>% 
  
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(6:8),m = c(4,5), b = c(1,2,3)),
         p_toilet = poly_asset_num(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
         p_garbage = poly_asset_num(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
         p_litetype = poly_asset_num(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:5), m = c(2), b = c(0, 1,3)), #0 is does not cook
         
         p_gencond = poly_asset_num(p_gencond,w = c(1), m = c(2,3), b = c(4)),
         p_neighcon = poly_asset_num(p_neighcon,w = c(1), m = c(2,3), b = c(4)),
         p_neighgar = poly_asset_num(p_neighgar,w = c(1), m = c(2,3), b = c(4)),
         
         p_areafood = poly_asset_num(p_areafood, w = c(3), m = c(2), b = c(1)),
         p_neighmat = poly_asset_num(p_neighmat,w = c(1), m = c(2), b = c(3)),
         # p_houseneat = poly_asset_num(p_houseneat, w = c(3), m = c(2), b = c(1)),
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
assets94 %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_"),starts_with("n_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()

# SAVE -------------------
saveRDS(assets94,paste0(path_harmonization_folder,"/philippines/working/assets94.RDS"))

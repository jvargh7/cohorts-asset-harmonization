hhold91 <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/1991-1992/zip_household/hhold91.dta"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)

dictionary_file(hhold91,type="dta")

variables91 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y1991)) %>% 
  dplyr::select(question,variable,y1991)


assets91 <- hhold91 %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    currbrgy,
    currstra,
    currhhno,
    currwman,
    hhldid91,
    wmanid91,
    basebrgy,
    basehhno,
    basewman,
    stat1991,
    
    # stilnhhd,
    # indchild,
    
    soursw91,
    toilet91,
    garbag91,
    litety91,
    cookfu91,
    storfo91,
    gencon91,
    neighcon,
    neighgar,
    areafo91,
    settleme,
    resarea,
    areain50,
    electric,
    elecserv,
    materi91,
    neighmat,
    
    one_of(variables91$y1991)
    
  ) %>% 
  # ID ---------------
  rename(brgy1991 = currbrgy,
         wmanid1991 = wmanid91,
         hhldid1991 = hhldid91,
         status1991 = stat1991
         
         
         # stra1991 = currstra,
         # hhno1991 = currhhno, #with(assets91,table(hhldid1991==currhhno)) : FALSE (92), TRUE (2480)
         # currwman, #with(assets91,table(wmanid1991==currwman)) : FALSE (24), TRUE (2548)
         ) %>% 

# HOUSING CHARACTERISTICS -----
  
  rename(sourcedw = soursw91,
         toilet = toilet91,
         garbage = garbag91,
         litetype = litety91,
         cookfuel = cookfu91,
         foodpurchase = storfo91,
         settlement = settleme,
         gencond = gencon91,
         areafood = areafo91,
         area50m = areain50,
         material = materi91
  )   %>% 
  rename_at(vars(sourcedw:material),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables91$y1991)),~variables91$variable) %>% 
  mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8),m = c(4,5), b = c(1,2,3)),
         f_toilet = poly_asset_cat(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
         f_garbage = poly_asset_cat(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
         f_litetype = poly_asset_cat(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         
         f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:5), m = c(2), b = c(1,3)),
         
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
         
         f_material = poly_asset_cat(p_material,w = c(1), m = c(2), b = c(3))
         
  ) %>% 
  dplyr::select(-p_resarea,-p_area50m) %>% 
  
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(6:8),m = c(4,5), b = c(1,2,3)),
         p_toilet = poly_asset_num(p_toilet, w = c(5:6), m = c(3:4), b = c(1:2)),
         p_garbage = poly_asset_num(p_garbage, w = c(1,3), m = c(2,4), b = c(-1)),
         p_litetype = poly_asset_num(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:5), m = c(2), b = c(1,3)),
         
         p_gencond = poly_asset_num(p_gencond,w = c(1), m = c(2,3), b = c(4)),
         p_neighcon = poly_asset_num(p_neighcon,w = c(1), m = c(2,3), b = c(4)),
         p_neighgar = poly_asset_num(p_neighgar,w = c(1), m = c(2,3), b = c(4)),
         
         p_areafood = poly_asset_num(p_areafood, w = c(3), m = c(2), b = c(1)),
         p_material = poly_asset_num(p_material,w = c(1), m = c(2), b = c(3))
         
  ) %>% 
  rename(d_electric = p_electric,
         d_elecserv = p_elecserv) %>% 
  
  rename_at(vars(aircon:owntv),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_owntv,d_electric,d_elecserv),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse) %>% 
  mutate(c_crowding = case_when(numbper >= 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))

assets91 %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()


saveRDS(assets91,paste0(path_harmonization_folder,"/philippines/working/assets91.RDS"))

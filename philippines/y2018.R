# path2018 <- "C:/Cloud/Box Sync/COHORTS SES Harmonization/philippines/2018"

# causeofdeath <- haven::read_dta(paste0(path2018,"/causeofdeath-2018.dta"))
# interviewstat <- haven::read_dta(paste0(path2018,"/interviewstat-2018.dta"))

cebu1 <- read_dta(paste0(path_cebu_redcap_data,"/CLHNS part1 Aug 2019.dta"))
cebu2 <- read_dta(paste0(path_cebu_redcap_data,"/CLHNS part2 Aug 2019.dta"))

intersecting_col_names <- colnames(cebu1)[colnames(cebu1) %in% colnames(cebu2)]

cebu2 <- cebu2 %>% dplyr::select(-c(basebrgy:uncchdid))
cebu18 <- full_join(cebu1,cebu2,by="uncchid")

variables18 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y2018)) %>% 
  dplyr::select(question,variable,y2018)

assets18 <- cebu18 %>% 
  rename_all(~str_to_lower(.)) %>% 
  dplyr::select(
    
    basebrgy,
    basehhno,
    basewman,
    
    currbrgy,
    currstra,
    
    uncchdid,
    # momch07,
    uncchid,
    
    
    sourcedw,
    sourcenon,
    toilet,
    garbage,
    litetype,
    cookfuel,
    # foodstor,
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
    houseneat,
    # bathrom2, #Inside or not
    
    one_of(variables18$y2018)
    
  ) %>% 
  rename(settlement = settlemn) %>% 
  
  # ID ---------------
  rename_at(vars(sourcedw:houseneat),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables18$y2018)),~variables18$variable) %>% 
  
  # HOUSING CHARACTERISTICS ----------
mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(6:8,10),m = c(5), b = c(1,2,3,4,9)),
       f_sourcenon = poly_asset_cat(p_sourcenon,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
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
  
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(6:8,10),m = c(5), b = c(1,2,3,4,9)),
         p_sourcenon = poly_asset_num(p_sourcenon,w = c(6:8,10),m = c(4,5), b = c(1,2,3,9)),
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
rename_at(vars(aircon:waterdis),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_waterdis,d_electric,d_elecserv),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse,
         d_ownlot = ownlot)  %>% 
  mutate(c_crowding = case_when(numbper >= 0 & roomnumb > 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))

# DESCRIPTIVES ----------------
assets18 %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()  

# SAVE -------------------
saveRDS(assets18,paste0(path_harmonization_folder,"/philippines/working/assets18.RDS"))















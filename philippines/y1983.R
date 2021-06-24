mbase2 <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/1983-1986/zip_mother/mbase2.dta"))
source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))
set.seed(20201022)
variables83 <- readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="Variable Levels") %>% 
  dplyr::filter(is.na(original_level),!is.na(y1983)) %>% 
  dplyr::select(question,variable,y1983)



assets83 <- mbase2 %>% 
  dplyr::select(
    basebrgy,
    basehhno,
    basewman,
    
    sourswat,
    watercon,
    toilet,
    garbage,
    litetype,
    cookfuel,
    gencondn,
    areafood,
    material,
    
    one_of(variables83$y1983)
    
  ) %>% 
  rename(sourcedw = sourswat,
         gencond = gencondn
         )   %>% 
  rename_at(vars(sourcedw:material),~paste0("p_",.)) %>% 
  rename_at(vars(one_of(variables83$y1983)),~variables83$variable) %>% 
  mutate(f_sourcedw = poly_asset_cat(p_sourcedw,w = c(7,4),m = c(5,6,8), b = c(1,2,3)),
         f_watercon = poly_asset_cat(p_watercon, w= c(5,6),m = c(4,8,10,11), b = c(1,2,3,7,9)),
         f_toilet = poly_asset_cat(p_toilet, w = c(8,9), m = c(5,6,7), b = c(1:4)),
         f_garbage = poly_asset_cat(p_garbage, w = c(4,5,7), m = c(2,6), b = c(1,3)),
         f_litetype = poly_asset_cat(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         f_cookfuel = poly_asset_cat(p_cookfuel, w = c(4:6), m = c(2,7), b = c(1,3)),
         
         f_gencond = poly_asset_cat(p_gencond,w = c(1), m = c(2,3), b = c(4)),
         f_areafood = poly_asset_cat(p_areafood, w = c(3), m = c(2), b = c(1)),
         f_material = poly_asset_cat(p_material,w = c(1), m = c(2), b = c(3))
  ) %>% 
  mutate(p_sourcedw = poly_asset_num(p_sourcedw,w = c(7,4),m = c(5,6,8), b = c(1,2,3)),
         p_watercon = poly_asset_num(p_watercon, w= c(5,6),m = c(4,8,10,11), b = c(1,2,3,7,9)),
         p_toilet = poly_asset_num(p_toilet, w = c(8,9), m = c(5,6,7), b = c(1:4)),
         p_garbage = poly_asset_num(p_garbage, w = c(4,5,7), m = c(2,6), b = c(1,3)),
         p_litetype = poly_asset_num(p_litetype,w = c(3,5),m = c(2,6),b = c(1,4)),
         
         p_cookfuel = poly_asset_num(p_cookfuel, w = c(4:6), m = c(2,7), b = c(1,3)),
         
         p_gencond = poly_asset_num(p_gencond,w = c(1), m = c(2,3), b = c(4)),
         p_areafood = poly_asset_num(p_areafood, w = c(3), m = c(2), b = c(1)),
         p_material = poly_asset_num(p_material,w = c(1), m = c(2), b = c(3))
         ) %>% 
  rename_at(vars(aircon:weldingtools),~paste0("d_",.)) %>% 
  mutate_at(vars(d_aircon:d_weldingtools),~binary_asset(.)) %>% 
  
  rename(d_ownhouse = ownhouse,
         d_otherhouse = otherhouse,
         d_ownlot = ownlot) %>% 
  mutate(c_crowding = case_when(numbper >= 0 ~ roomnumb/numbper,
                                TRUE ~ NA_real_))

assets83 %>% 
  dplyr::select(starts_with("c_"),starts_with("f_"),starts_with("d_")) %>% 
  compareGroups::compareGroups(~.,data=.,include.label = TRUE,include.miss = TRUE) %>% 
  compareGroups::createTable()


saveRDS(assets83,paste0(path_harmonization_folder,"/philippines/working/assets83.RDS"))














# STRUCTURAL ----------

source(paste0(path_harmonization_repo,"/south africa/sa_early_life.R"))
source(paste0(path_harmonization_repo,"/south africa/sa_adult_outcomes.R"))

# YR 0 to 2 ----------------

yr_0_to_2 <- readRDS(paste0(path_harmonization_folder,"/south africa/Assets with PCA clean.RDS")) %>% 
  dplyr::select(bttid,
                
                starts_with("ses_ownership"),
                starts_with("ses_facilities"),
                starts_with("ses_assets"),
                houseown2groups,
                Water2groups,
                toilet2groups
                ) %>% 
  
  rename(housetype = ses_ownership_type_house,
         ownhouse = ses_ownership_ownhouse,
         watertype = ses_facilities_water_type,
         wateruse = ses_facilities_water_use,
         toilettype = ses_facilities_toilet_type,
         toiletuse = ses_facilities_toilet_use,
         refuse = ses_facilities_refuse,
         electricity = ses_facilities_electricity,
         tv = ses_assets_tv,
         car = ses_assets_car,
         refrigerator = ses_assets_fridge,
         washingmachine = ses_assets_washing_machine,
         telephone = ses_assets_phone,
         radio = ses_assets_radio,
         ownhouse2groups = houseown2groups,
         water2groups = Water2groups,
         toilet2groups = toilet2groups) %>% 
  
  # with(yr_0_to_2,table(ownhouse,ownhouse2groups,useNA = "always"))
  dplyr::select(-ownhouse2groups) %>% 
  
  rename_at(vars(housetype,watertype,toilettype,refuse),~paste0("p_",.)) %>% 
  rename_at(vars(water2groups,toilet2groups),~paste0("r_",.)) %>% 
  rename_at(vars(ownhouse,wateruse,toiletuse,electricity,tv,car,
                 refrigerator,washingmachine,telephone,
                 radio),~paste0("d_",.)) %>% 
  
  mutate(f_housetype = poly_asset_cat(p_housetype,w=0,m=1,b=2),
         f_watertype = poly_asset_cat(p_watertype, w=0,m=1,b=2),
         f_toilettype = poly_asset_cat(p_toilettype,w=0,m=1,b=2),
         f_refuse = poly_asset_cat(p_refuse,w=0,m=1,b=2)
         )
  



# YR 7 to 9 ----------

yr_7_to_9 <- readRDS(paste0(path_harmonization_folder,"/south africa/Assets with PCA clean.RDS")) %>% 
  dplyr::select(bttid,
                starts_with("y7q")
  ) %>% 
  dplyr::select(-y7q17_momhighesteduc,-y7q18_momoccupation,
                -y7q20_mommarital,-y7q1a_biomom,
                -y7q1b_relbtt,
                # -y7q17bhholdpedu,
                -contains("sumassets"),
                -contains("weightnoassets"),
                -contains("PCA"),
                -contains("wealthq")) %>% 
  rename(housetype = y7q9_housetype,
         ownhouse = y7q11_homeownership,
         watertype = y7q14_householdwater,
         toilettype = y7q15_toilet,
         
         electricity = y7q16a_elec,
         tv = y7q16b_tv,
         car = y7q16d_car,
         refrigerator = y7q16e_fridge,
         washingmachine = y7q16f_washingmach,
         telephone = y7q16g_phone,
         radio = y7q16c_radio,
         water2groups = y7q14_householdwater_recode,
         toilet2groups = y7q15_toilet_recode,
         dvd = y7q16h_video,
         microwave = y7q16i_microwave,
         
         # No idea what these are -----------
         medaid = y7q22dhholdmedaid,
         education = y7q17bhholdpedu
         ) %>% 
  
  # wateruse,toiletuse: not available
  
  rename_at(vars(housetype,watertype,toilettype),~paste0("p_",.)) %>% 
  rename_at(vars(water2groups,toilet2groups),~paste0("r_",.)) %>% 
  rename_at(vars(ownhouse,electricity,tv,car,
                 refrigerator,washingmachine,telephone,
                 radio,dvd,microwave),~paste0("d_",.)) %>% 
  
  mutate(f_housetype = poly_asset_cat(p_housetype,w=0,m=1,b=2),
         f_watertype = poly_asset_cat(p_watertype, w=0,m=1,b=2),
         f_toilettype = poly_asset_cat(p_toilettype,w=0,m=1,b=2)
  )

# YR 12 to 13 ---------------

yr_12_to_13 <- readRDS(paste0(path_harmonization_folder,"/south africa/Assets with PCA clean.RDS")) %>% 
  dplyr::select(bttid,
                starts_with("y12"),
                starts_with("y13"),
                yr12_13q2SEconHouse
  ) %>% 
  dplyr::select(-y13q2SEconOther,-y13q2SEconNotes
                # -yr12_13q1SEconSupGrnt, # numeric
                # -yr12_13q1SEconPens, # numeric
                # -yr12_13q1SEconDisab, # numeric
                # -yr12_13q2SEcoLivingPrtnr, # Yes/no
                # -yr12_13q2SEconMarStatus # Marital Status
                
                ) %>% 
  rename(housetype = yr12_13q2SEconHouse,
         
         electricity = y12q1SEconElectr,
         tv = y12q1SeconTv,
         car = y12q1SEconMotVeh,
         refrigerator = y12q1SEconFridge,
         washingmachine = y12q1SEconWashMach,
         telephone = y12q1SEconTel,
         cellphone = y12q1SEconCellTel,
         radio = y12q1SEcoRadio,
         water2groups = y13SESWaterRecode2Groups,
         toilet2groups = y13SESToiletRecode2Groups,
         dvd = y12q1SEconVidMach,
         microwave = y12q1SEconMwave,
         mnet = y12q1SEconMnet,
         satellitetv = y12q1SEconDstvSatellite,
         
         water_htcld_indoor = y13q2SEconHtCldIndr,
         water_cld_indoor = y13q2SEconCldIndr,
         water_outside = y13q2SEconOside,
         water_other = y13q2SEconOtherSource,
         
         
         toilet_inside = y13q2SEconTletIside,
         toilet_outside = y13q2SEconTletOside,
         toilet_pitlatrine = y13q2SEconPitLatrine,
         toilet_bucket = y13q2SEconBucketSys,
         toilet_other = y13q2SEconOtheTlet,
         
         education = Y12_13EDU_D
         
         ) %>% 
  # Note: Recode based on 0 to 2 ======
  mutate(
         watertype = case_when(water_htcld_indoor %in% c(2,1) ~ 2,
                               water_cld_indoor %in% c(2,1) ~ 1,
                               water_outside %in% c(2,1) ~ 0,
                               water_other %in% c(2,1) ~ 0,
                               TRUE ~ NA_real_),
         
         toilettype = case_when(toilet_inside %in% c(2,1) ~ 2,
                                toilet_outside %in% c(2,1) ~ 1,
                                toilet_pitlatrine %in% c(2,1) ~ 0,
                                toilet_bucket %in% c(2,1) ~ 0,
                                toilet_other %in% c(2,1) ~ 0,
                                TRUE ~ NA_real_),
         # Note: Decreasing order of importance =======
         wateruse = case_when(water_htcld_indoor == 2 ~ 1,
                              water_htcld_indoor == 1 ~ 0,
                              water_cld_indoor == 2 ~ 1,
                              water_cld_indoor == 1 ~ 0,
                              water_outside == 2 ~ 1,
                              water_outside == 1 ~ 0,
                              water_other == 2 ~ 1,
                              water_other == 1 ~ 0,
                              TRUE ~ NA_real_),
         
         
         toiletuse = case_when(toilet_inside == 2 ~ 1,
                              toilet_inside == 1 ~ 0,
                              toilet_outside == 2 ~ 1,
                              toilet_outside == 1 ~ 0,
                              toilet_pitlatrine == 2 ~ 1,
                              toilet_pitlatrine == 1 ~ 0,
                              toilet_bucket == 2 ~ 1,
                              toilet_bucket == 1 ~ 0,
                              toilet_other == 2 ~ 1,
                              toilet_other == 1 ~ 0,
                              TRUE ~ NA_real_),
         
         ) %>% 
  
  mutate(cable = case_when(mnet == 1 | satellitetv == 1 ~ 1,
                           mnet == 0 & satellitetv == 0 ~ 0,
                           TRUE ~ NA_real_)) %>% 
  rename_at(vars(housetype,watertype,toilettype),~paste0("p_",.)) %>% 
  rename_at(vars(water2groups,toilet2groups),~paste0("r_",.)) %>% 
  # Check ownhouse : not available =============
  rename_at(vars(wateruse,toiletuse,electricity,tv,car,
                 refrigerator,washingmachine,telephone,cellphone,
                 radio,dvd,microwave,mnet,satellitetv,cable),~paste0("d_",.)) %>% 
  # f_housetype not available
  mutate(f_housetype = poly_asset_cat(p_housetype,w=0,m=1,b=2),
         f_watertype = poly_asset_cat(p_watertype, w=0,m=1,b=2),
         f_toilettype = poly_asset_cat(p_toilettype,w=0,m=1,b=2)
  ) %>% 
  dplyr::select(-starts_with("toilet_"),-starts_with("water_"))

# YR 16 ---------------

yr_16 <- readRDS(paste0(path_harmonization_folder,"/south africa/Assets with PCA clean.RDS")) %>% 
  dplyr::select(bttid,
                starts_with("Yr16")
  ) %>% 
  dplyr::select(-contains("sumassets"),
                -contains("PCA"),
                -contains("weightnoassets"),
                -contains("wealthq"),
                -Yr16_Notes,-YR16_0,
                -Yr16_SESQ1OtherToiletSpecify,
                -Yr16_SESQ1OtherWaterSpecify
                
                ) %>% 
  rename(electricity = Yr16_SESQ2Electricity,
         tv = Yr16_SESQ2TV,
         car = Yr16_SESQ2Car,
         refrigerator = Yr16_SESQ2Fridge,
         washingmachine = Yr16_SESQ2WashingMachine,
         telephone = Yr16_SESQ2LandPhone,
         cellphone = Yr16_SESQ2CellPhone,
         radio = Yr16_SESQ2Radio,
         
         water2groups = Yr16_Water_recode_2groups,
         toilet2groups = Yr16_toilet_recoded_2groups,
         dvd = Yr16_SESQ2DVD,
         microwave = Yr16_SESQ2Microwave,
         computer = Yr16_SESQ2Computer,
         internet = Yr16_SESQ2Internet,
         
         mnet = Yr16_SESQ2Mnet,
         satellitetv = Yr16_SESQ2DSTV,
         water_htcld_indoor = Yr16_SESQ1HotCold,
         water_cld_indoor = Yr16_SESQ1Cold,
         water_outside = Yr16_SESQ1Outside,
         water_other = Yr16_SESQ1OtherWater,
         
         toilet_inside = Yr16_SESQ1FlushIn,
         toilet_outside = Yr16_SESQ1FlushOut,
         toilet_pitlatrine = Yr16_SESQ1Pit,
         toilet_bucket = Yr16_SESQ1Bucket,
         toilet_other = Yr16_SESQ1OtherToilet,
         
         medaid = Yr16_SESQ3MedAid,
         cover = Yr16_SESQ4Cover
         
         
         ) %>% 
  # Note: Recode based on 0 to 2 ======
mutate(
  watertype = case_when(water_htcld_indoor %in% c(2,1) ~ 2,
                        water_cld_indoor %in% c(2,1) ~ 1,
                        water_outside %in% c(2,1) ~ 0,
                        water_other %in% c(2,1) ~ 0,
                        TRUE ~ NA_real_),
  
  toilettype = case_when(toilet_inside %in% c(2,1) ~ 2,
                         toilet_outside %in% c(2,1) ~ 1,
                         toilet_pitlatrine %in% c(2,1) ~ 0,
                         toilet_bucket %in% c(2,1) ~ 0,
                         toilet_other %in% c(2,1) ~ 0,
                         TRUE ~ NA_real_),
  # Note: Decreasing order of importance =======
  wateruse = case_when(water_htcld_indoor == 2 ~ 1,
                       water_htcld_indoor == 1 ~ 0,
                       water_cld_indoor == 2 ~ 1,
                       water_cld_indoor == 1 ~ 0,
                       water_outside == 2 ~ 1,
                       water_outside == 1 ~ 0,
                       water_other == 2 ~ 1,
                       water_other == 1 ~ 0,
                       TRUE ~ NA_real_),
  
  
  toiletuse = case_when(toilet_inside == 2 ~ 1,
                        toilet_inside == 1 ~ 0,
                        toilet_outside == 2 ~ 1,
                        toilet_outside == 1 ~ 0,
                        toilet_pitlatrine == 2 ~ 1,
                        toilet_pitlatrine == 1 ~ 0,
                        toilet_bucket == 2 ~ 1,
                        toilet_bucket == 1 ~ 0,
                        toilet_other == 2 ~ 1,
                        toilet_other == 1 ~ 0,
                        TRUE ~ NA_real_),
  
) %>%  
  
  mutate(cable = case_when(mnet == 1 | satellitetv == 1 ~ 1,
                           mnet == 0 & satellitetv == 0 ~ 0,
                           TRUE ~ NA_real_)) %>% 
  
  # housetype, refuse: Not available 
  
  rename_at(vars(watertype,toilettype),~paste0("p_",.)) %>% 
  rename_at(vars(water2groups,toilet2groups),~paste0("r_",.)) %>% 
  
  # ownhouse : not available
  rename_at(vars(wateruse,toiletuse,electricity,tv,car,
                 refrigerator,washingmachine,telephone,cellphone,
                 radio,dvd,microwave,computer,internet,mnet,satellitetv,cable),~paste0("d_",.)) %>% 
  # f_housetype : not available
  mutate(
         f_watertype = poly_asset_cat(p_watertype, w=0,m=1,b=2),
         f_toilettype = poly_asset_cat(p_toilettype,w=0,m=1,b=2),
  ) %>% 
  dplyr::select(-starts_with("toilet_"),-starts_with("water_"))

# YR 22 -------------------
yr_22 <- readRDS(paste0(path_harmonization_folder,"/south africa/Assets with PCA clean.RDS")) %>% 
  dplyr::select(bttid,
                starts_with("yas_cq1e8_assets")
  ) %>% 
  rename (
    
    electricity = yas_cq1e8_assets___1,
    tv = yas_cq1e8_assets___8,
    car = yas_cq1e8_assets___2,
    refrigerator = yas_cq1e8_assets___3,
    washingmachine = yas_cq1e8_assets___5,
    telephone = yas_cq1e8_assets___6,
    cellphone = yas_cq1e8_assets___7,
    radio = yas_cq1e8_assets___9,
    dvd = yas_cq1e8_assets___10,
    microwave = yas_cq1e8_assets___4,
    computer = yas_cq1e8_assets___12,
    internet = yas_cq1e8_assets___13,
    cable = yas_cq1e8_assets___11
    
  ) %>% 
  # wateruse,toiletuse: not available
  
  rename_at(vars(electricity,tv,car,
                 refrigerator,washingmachine,telephone,cellphone,
                 radio,dvd,microwave,computer,internet,cable),~paste0("d_",.))


# YR 28 ---------------

yr_28 <- left_join(readRDS(paste0(path_bt20_data,"/Interim Data/bt20_y28_questionnaire_valid_ids.RDS")),
                  read_csv(paste0(path_bt20_data_old,"/bt20_id_master.csv")),
                  by="bttid") %>% 
  dplyr::select(bttid, 
                starts_with("housing_"),
                starts_with("assets"),
                starts_with("phone")
  ) %>% 
  dplyr::select(-contains("_dk"),-contains("spec")) %>% 
  rename(
    housetype = housing_type,
    water2groups = housing_indoorwater,
    toilet2groups = housing_indoorflushtoilet,
    tv = assets6,
    car = assets1,
    refrigerator = assets2,
    washingmachine = assets4,
    telephone = assets5,
    cellphone = phoneown,
    microwave = assets3,
    computer = assets8,
    internet = assets9,
    cable = assets7,
    numsleepingrooms = housing_numsleepingrooms,
    numpeople = housing_numpeople,
    cellphone_contract = phonecontract,
    cellphone_internet = phoneinternet
  ) %>% 
  
  mutate(ownhouse = case_when(housing_rent == 1 ~ 0,
                              housing_rent == 0 ~ 1,
                              TRUE ~ NA_real_)) %>% 
  mutate(housetype = poly_asset_num(housetype,w = c(1,7),m = c(4,5,6),b = c(2,3)),
         water2groups = case_when(water2groups %in% c(0,1) ~ water2groups,
                                  TRUE ~ NA_real_),
         toilet2groups = case_when(toilet2groups %in% c(0,1) ~ toilet2groups,
                                  TRUE ~ NA_real_)
         ) %>% 
  

  mutate_at(vars(tv,car,
                 refrigerator,washingmachine,telephone,cellphone,
                 microwave,computer,internet,cable),function(x) case_when(x %in% c(0,1) ~ x,
                                                                          TRUE ~ NA_real_)) %>% 
  
  
  rename_at(vars(housetype),~paste0("p_",.)) %>% 
  rename_at(vars(water2groups,toilet2groups),~paste0("r_",.)) %>% 
  
  # radio : not available
  # dvd: not available
  rename_at(vars(ownhouse, tv,car,
                 refrigerator,washingmachine,telephone,cellphone,
                 microwave,computer,internet,cable),~paste0("d_",.)) %>% 
  mutate(
    f_housetype = poly_asset_cat(p_housetype,w=0,m=1,b=2),
  ) %>% 
  mutate(numsleepingrooms = case_when(numsleepingrooms>=15 ~ NA_real_,
                                      TRUE ~ numsleepingrooms),
         numpeople = case_when(numpeople >= 50 ~ NA_real_,
                               TRUE ~ numpeople)
         ) %>% 
  mutate(c_crowding = case_when(!is.na(numsleepingrooms) & numpeople > 0 ~ numsleepingrooms/numpeople,
                                TRUE ~ NA_real_))

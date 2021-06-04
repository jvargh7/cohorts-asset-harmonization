
# Early life ---------
source(paste0(path_harmonization_repo,"/brazil 1993/b9_early_life.R"))

# adultoutcomes sourced in b9ses02_merge.R >> b9_adult_outcomes.R using pcall
# In the case of Brazil 1993, we combine adult outcomes from different waves (Age 18 and Age 22) into one dataset

# AGE 4 --------------
b9_age4 <- haven::read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::select(nquest,starts_with("f")) %>% 
  rename(n_radio = fradio,
         n_television = ftv,
         n_car = fcarro,
         n_housekeeper = femp,
         refrigerator  = fgelad,
         washingmachine = froupa,
         vaccumcleaner = faspir,
         motorcycle = fmoto,
         dvd = fvideo,
         # Categorical
         
         housingmaterial = fmor,
         n_bathroom = fbanh,
         n_bedroom = fquarto,
         # n_residents = ?
         toilet = fsanit,
         pipedwater = faguaen,
         
         income = frfreal4a
         ) %>% 
  mutate(radio = count2bin_asset(n_radio),
         television = count2bin_asset(n_television),
         car = count2bin_asset(n_car),
         housekeeper = count2bin_asset(n_housekeeper)) %>% 
  rename_at(vars(refrigerator,washingmachine,
                 vaccumcleaner,motorcycle,dvd,
                 
                 radio,television,car,housekeeper
                 ),~paste0("d_",.)) %>% 
  mutate_at(vars(starts_with("d_")),~binary_asset(.)) %>% 
  
  rename_at(vars(housingmaterial,pipedwater,toilet),~paste0("p_",.)) %>% 
  mutate(f_housingmaterial = poly_asset_cat(p_housingmaterial,w = c(4,5),m = c(2,3), b = c(1)),
         f_pipedwater = poly_asset_cat(p_pipedwater,w = c(3),m = c(2), b = c(1)),
         f_toilet = poly_asset_cat(p_toilet, w= c(4),m=c(2,3),b=c(1))
         ) %>% 
  mutate(p_housingmaterial = poly_asset_num(p_housingmaterial,w = c(4,5),m = c(2,3), b = c(1)),
         p_pipedwater = poly_asset_num(p_pipedwater,w = c(3),m = c(2), b = c(1)),
         p_toilet = poly_asset_num(p_toilet, w= c(4),m=c(2,3),b=c(1))
         )

# AGE 11 --------------
b9_age11 = haven::read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::select(nquest,starts_with("h"),LIVING_INDEX_ITEMS_11YRS) %>%
  rename(n_radio = hm061,
         n_television = hm062,
         n_car = hm063,
         n_housekeeper = hm065,
         refrigerator  = hm071,
         washingmachine = hm066,
         vaccumcleaner = hm064,
         # motorcycle = ,
         dvd = hm067,
         computer = hm069,
         internet = hm070,
         duplexfridge = hm072,
         stereo = hm068,
         
         income = hrenfa,
         
         # Categorical
         housingmaterial = hm081,
         houseownership = hm077,
         n_bathroom = hm073,
         n_bedroom = hm078,
         toilet = hm076,
         n_bathshower = hm074,
         pipedwater = hm075,
         n_residents = hm016
  ) %>% 
  mutate(radio = count2bin_asset(n_radio),
         television = count2bin_asset(n_television),
         car = count2bin_asset(n_car),
         housekeeper = count2bin_asset(n_housekeeper)) %>% 
  rename_at(vars(refrigerator,washingmachine,
                 vaccumcleaner,dvd,
                 computer,internet,
                 duplexfridge,stereo,
                 
                 radio,television,car,housekeeper
  ),~paste0("d_",.)) %>% 
  mutate_at(vars(starts_with("d_")),~binary_asset(.)) %>% 
  
  # House ownership
  mutate(houseownership = poly_asset_bin(houseownership,y=c(1),n=c(2:5))) %>% 
  rename(d_houseownership = houseownership) %>% 
  
  rename_at(vars(housingmaterial,pipedwater,toilet),~paste0("p_",.)) %>% 
  mutate(f_housingmaterial = poly_asset_cat(p_housingmaterial,w = c(6:8,12),m = c(2,4,5,10,11), b = c(1,3)),
         f_pipedwater = poly_asset_cat(p_pipedwater,w = c(0),m = c(2), b = c(1)),
         f_toilet = poly_asset_cat(p_toilet, w= c(0),m=c(2,3),b=c(1))
  ) %>% 
  mutate(p_housingmaterial = poly_asset_num(p_housingmaterial,w = c(6:8,12),m = c(2,4,5,10,11), b = c(1,3)),
         p_pipedwater = poly_asset_num(p_pipedwater,w = c(0),m = c(2), b = c(1)),
         p_toilet = poly_asset_num(p_toilet, w= c(0),m=c(2,3),b=c(1))
  ) %>% 
  rename(c_sli = LIVING_INDEX_ITEMS_11YRS) %>% 
  mutate(c_crowding = (n_bedroom/n_residents))

# AGE 15 --------------
b9_age15 = haven::read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::select(nquest,starts_with("j"),LIVING_INDEX_ITEMS_15YRS) %>%
  dplyr::select(-jcorpel5) %>% 
  rename(n_radio = jm061b,
         radio = jm061a,
         television = jm062a,
         n_television = jm062b,
         car = jm063a,
         n_car = jm063b,
         housekeeper = jm065a,
         n_housekeeper = jm065b,
         
         income = jrenfam,
         
         refrigerator  = jm072,
         washingmachine = jm066,
         vaccumcleaner = jm064,
         # motorcycle = ,
         dvd = jm067,
         computer = jm070,
         internet = jm071,
         duplexfridge = jm073,
         stereo = jm068,
         videogame = jm069,
         
         # Categorical
         # housingmaterial = ,
         houseownership = jm078,
         n_bathroom = jm074,
         n_bedroom = jm079,
         toilet = jm077,
         n_bathshower = jm075,
         pipedwater = jm076,
         n_residents = jm012
  ) %>% 
  mutate_at(vars(starts_with("n")),~count2count_asset(.,na_vals=88)) %>% 
  rename_at(vars(radio,television,car,housekeeper,
                 refrigerator,washingmachine,
                 vaccumcleaner,dvd,
                 computer,internet,
                 duplexfridge,stereo,
                 videogame), ~paste0("d_",.)) %>% 
  mutate_at(vars(starts_with("d_")),~binary_asset(.)) %>% 

  # House ownership
  mutate(houseownership = poly_asset_bin(houseownership,y=c(1),n=c(2:12))) %>% 
  rename(d_houseownership = houseownership) %>% 
  
  rename_at(vars(pipedwater,toilet
                 # housingmaterial,
                 ),~paste0("p_",.)) %>% 
  mutate(
         # f_housingmaterial = poly_asset_cat(p_housingmaterial,w = c(),m = c(), b = c()),
         f_pipedwater = poly_asset_cat(p_pipedwater,w = c(0),m = c(2), b = c(1)),
         f_toilet = poly_asset_cat(p_toilet, w= c(0),m=c(2,3),b=c(1))
  ) %>% 
  mutate(
         # p_housingmaterial = poly_asset_num(p_housingmaterial,w = c(),m = c(), b = c()),
         p_pipedwater = poly_asset_num(p_pipedwater,w = c(3),m = c(2), b = c(1)),
         p_toilet = poly_asset_num(p_toilet, w= c(0),m=c(2,3),b=c(1))
  ) %>% 
  rename(c_sli = LIVING_INDEX_ITEMS_15YRS) %>% 
  mutate(c_crowding = (n_bedroom/n_residents))


  


# AGE 18 --------------
b9_age18 = haven::read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::select(nquest,starts_with("k"),LIVING_INDEX_ITEMS_18YRS) %>% 
  rename(
    age = kidadeanos,
    iq = kqitotbr,
    children = kfilhos,
    employed_lastyear = kattrabtodos,
    occupation = koccupation,
    
    # n_radio = ,
    # radio = ,
    television = ktvcol,
    n_television = ktvqtas,
    car = kcarro,
    n_car = kcarqtos,
    housekeeper = kempdom,
    n_housekeeper = kempqtas,
    
    income = krenfam,
    
    refrigerator  = kgelad,
    washingmachine = kmaqlav,
    vaccumcleaner = kasppo,
    # motorcycle = ,
    dvd = kdvd,
    # computer = kcomput, knoteb,
    desktop = kcomput,
    notebook = knoteb,
    # n_computer = kcomput1, knoteb1,
    n_desktop = kcomput1,
    n_notebook = knoteb1,
    
    
    aircon = karcond,
    n_aircon = karcond1,
    
    internet = kint24h,
    duplexfridge = kfreezer,
    # stereo = ,
    videogame = kvgame,
    microwave = kmiconda,
    
    # Categorical
    # housingmaterial = ,
    houseownership = kcasmora,
    n_bathroom = kbanhei,
    n_bedroom = kdormit,
    # toilet = ,
    n_bathshower = kbanchuv,
    # pipedwater = ,
    n_residents = kqtpes
  ) %>% 
  mutate(inrelationship = case_when(knamoro == 1 ~ 1,
                                    kmoraconj == 1 ~ 1,
                                    knamoro == 0 & kmoraconj == 0 ~ 0,
                                    TRUE ~ NA_real_),
         livewithpartner = case_when(kmoraconj == 1 | kmoranam == 1 ~ 1,
                                     kmoraconj == 0 & kmoranam == 0~ 0,
                                     TRUE ~ NA_real_,
         )
  ) %>% 
  dplyr::select(-knamoro,-kmoraconj,-kmoranam) %>% 
  mutate(computer = case_when(desktop == 1 | notebook == 1 ~ 1,
                              desktop == 0 & notebook == 0 ~ 0,
                              TRUE ~ NA_real_)) %>% 
  mutate_at(vars(one_of("n_desktop","n_notebook")),~count2count_asset(.,na_vals = 88)) %>% 
  mutate_at(vars(starts_with("n_")),~count2count_asset(.,na_vals=88)) %>% 
  mutate(n_computer = case_when(is.na(computer) ~ NA_real_,
                               TRUE ~ rowSums(.[,c("n_desktop","n_notebook")],na.rm = TRUE))) %>% 
  
  rename_at(vars(television,car,housekeeper,
                 refrigerator,washingmachine,
                 vaccumcleaner,dvd,
                 aircon,
                 computer,
                 desktop,notebook,
                 internet,
                 duplexfridge,
                 videogame,microwave),~paste0("d_",.)) %>% 
  
  mutate_at(vars(starts_with("d_")),~binary_asset(.)) %>% 
  # House ownership
  # Check what to do with students living in dormitories
  mutate(houseownership = poly_asset_bin(houseownership,y=c(1),n=c(2:5))) %>% 
  rename(d_houseownership = houseownership) %>% 
  rename_at(vars(occupation
                 # housingmaterial,pipedwater,toilet
  ),~paste0("p_",.)) %>% 
  mutate(
    f_occupation = poly_asset_cat(p_occupation,w = c(0,1),m = c(2,5), b = c(3,4))
  ) %>% 
  mutate(
    p_occupation = poly_asset_num(p_occupation,w = c(0,1),m = c(2,5), b = c(3,4))
    ) %>% 
  rename(c_sli = LIVING_INDEX_ITEMS_18YRS) %>% 
  mutate(c_crowding = (n_bedroom/n_residents))

# AGE 22 --------------- 
b9_age22 = haven::read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::select(nquest,starts_with("l"),LIVING_INDEX_ITEMS_22YRS) %>% 
  # dplyr::select(-one_of(outcome_vars)) %>% 
  dplyr::select(-LIVING_INDEX_ITEMS_11YRS,-LIVING_INDEX_ITEMS_15YRS,
                -LIVING_INDEX_ITEMS_18YRS) %>% 
  rename(bmi = limcbod,
         bmi_category = lestnutbod,
         schooling = lescoljovemano,
         schooling_category = lescoljovemcat,
         occupation = loccupation,
         employed_ever = ld006,
         employed_lastyear = ld008, # Conditional on ld006
         employed_lastmonth = ld009,
         employed_currently = ld011,
         children = lfilhos,
         srq = lsrqtot,
         srq_category = lsrqdic,
         
         wellbeing = lbestarcont,
         wellbeing_category = lbestarq) %>% 
  
  rename(
    
    age = lidadeanos,
   
    
    lifestatus = LIFE_STATUS,
    
    # n_radio = ,
    radio = ld030,
    television = ld029,
    n_television = ld029a,
    car = ld031,
    n_car = ld031a,
    housekeeper = ld033,
    n_housekeeper = ld033a,
    cleaninglady = ld034,
    n_cleaninglady = ld034a,
    
    income = lrenfam,
    
    refrigerator  = ld042,
    n_refrigerator = ld042a,
    washingmachine = ld035,
    n_washingmachine = ld035a,
    # vaccumcleaner = ,
    motorcycle = ld032,
    n_motorcycle = ld032a,
    dvd = ld036,
    ndvd = ld036a,
    computer = ld038,
    n_computer = ld038a,
    
    aircon = ld037,
    n_aircon = ld037a,
    
    internet = ld038b,
    duplexfridge = ld043,
    n_duplexfridge = ld043a,
    # stereo = ,
    # videogame = ,
    microwave = ld039,
    n_microwave = ld039a,
    dishwasher = ld040,
    n_dishwasher = ld040a,
    clothesdryer = ld041,
    # Categorical
    # housingmaterial = ,
    # houseownership = ,
    n_bathroom = ld044,
    n_bedroom = ld045,
    # toilet = ,
    n_bathshower = ld044a,
    pipedwater = ld046,
    streetpaved = ld047,
    n_residents = ld024
  )  %>% 
  mutate(inrelationship = case_when(ld023 == 1 ~ 1,
                                    ld023b == 1 ~ 1,
                                    ld023b == 0 ~ 0,
                                    TRUE ~ NA_real_),
         
         livewithpartner = case_when(ld023 == 1 ~ 1,
                                     ld023 == 0 ~ 0,
                                     TRUE ~ NA_real_)
         
  ) %>% 
  dplyr::select(-ld023,-ld023b) %>% 
  rename_at(vars(radio,television,car,housekeeper,cleaninglady,
                 refrigerator,washingmachine,
                 motorcycle,
                 dvd,
                 aircon,
                 computer,internet,
                 duplexfridge,
                 microwave,dishwasher,
                 clothesdryer,streetpaved),~paste0("d_",.)) %>% 
  mutate_at(vars(starts_with("d_")),~binary_asset(.)) %>% 
  rename_at(vars(occupation,pipedwater
                 # housingmaterial,,toilet
  ),~paste0("p_",.)) %>% 
  mutate(
    f_occupation = poly_asset_cat(p_occupation,w = c(0,5,8),m = c(3,4,6), b = c(1,2)),
    f_pipedwater = poly_asset_cat(p_pipedwater,w = c(3),m = c(2), b = c(1))
  ) %>% 
  mutate(
    p_occupation = poly_asset_num(p_occupation,w = c(0,5,8),m = c(3,4,6), b = c(1,2)),
    p_pipedwater = poly_asset_num(p_pipedwater,w = c(3),m = c(2), b = c(1))
  ) %>% 
  rename(c_sli = LIVING_INDEX_ITEMS_22YRS) %>% 
  mutate(c_crowding = (n_bedroom/n_residents))












# 1969-72 -------------
in_1969 <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/NDBC 1969-1972 variables Jithin.sav")) %>% 
  dplyr::select(-religion,-c3tylat) %>% 
  rename(toilettype = c3toilet,
         drinkingwatershared = c3water,
         housetype = c3housty) %>% 
  rename_at(vars(toilettype,drinkingwatershared,housetype),~paste0("p_",.)) %>% 
  mutate(p_toilettype = poly_asset_num(p_toilettype,w = 0,m=1,b=2),
         p_drinkingwatershared = poly_asset_num(p_drinkingwatershared, w=0,m=1,b=2),
         p_housetype = poly_asset_num(p_housetype,w=1,m=c(2),b=c(3,4))
         ) %>% 
  mutate(f_toilettype = poly_asset_cat(p_toilettype,w = 0,m=1,b=2),
         f_drinkingwatershared = poly_asset_cat(p_drinkingwatershared, w = 0,m=1,b=2),
         f_housetype = poly_asset_cat(p_housetype,w = 0,m=1,b=2))
  
saveRDS(in_1969,paste0(path_harmonization_folder,"/india/working/in_1969.RDS"))  
# 1999-02 -----------

in_1999_assets <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/APH1 asset variables.sav"))

in_1999 <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/Adult Phase 1 (1999-2002).sav")) %>%
  left_join(in_1999_assets,
            by=c("id"="ORIGINALST")) %>% 
  dplyr::select(-omatposs) %>% 
  rename(sharedtoilet = otoilet,
         drinkingwater = odrwatso,
         generalwater = ognwatso,
         drinkingwatershared = odrwatsu,
         generalwatershared = ognwatsu,
         
         housetype = otypehou,
         numroom = onoroom,
         numpeople = onopeop,
         
         electricity = AELECTRICI,
         fan = BFAN,
         bicycle = CCYCLE,
         radio = DRADIO,
         twowheeler = E2WHEELER,
         stove = FGASSTOVE,
         television = GTELEVISIO,
         cable = HCABLETV,
         # IELECTRICM,
         # JELECRICGR,
         cooler = KAIRCOOLER,
         washingmachine = LWASHINGMA,
         car = MCAR,
         airconditioner = NAIRCONDIT,
         computer = OHOMECOMPU,
         dishtv = PDISHANTEN,
         telephone = OLDQTELEPHONE
         
         
         ) %>% 
    mutate(mixergrinder = case_when(IELECTRICM == 1 | JELECRICGR == 1 ~ 1,
                                    IELECTRICM == 0 & JELECRICGR == 0 ~ 0,
                                    TRUE ~ NA_real_)) %>% 
  dplyr::select(-IELECTRICM,-JELECRICGR) %>% 
  rename_at(vars(sharedtoilet,drinkingwater,
                 generalwater,drinkingwatershared,
                 generalwatershared),~paste0("p_",.)) %>% 
  rename_at(vars(housetype,
                 electricity,fan,bicycle,
                 radio,twowheeler,stove,television,
                 cable,mixergrinder,cooler,washingmachine,
                 car,airconditioner,computer,dishtv,
                 telephone
                 # ,cellphone
                 ),~paste0("d_",.)) %>% 
  mutate(d_housetype = case_when(d_housetype %in% c(1,2) ~ 1,
                                 TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("d_")),~as.numeric(.)) %>% 
  
  
  mutate_at(vars(starts_with("d_")),~as.numeric(.)) %>% 
  mutate(p_sharedtoilet = poly_asset_num(p_sharedtoilet,w=3,m=2,b=1),
         p_drinkingwater = poly_asset_num(p_drinkingwater, w = 3,m=2,b=1),
         p_generalwater = poly_asset_num(p_generalwater, w=3,m=2,b=1),
         p_drinkingwatershared = poly_asset_num(p_drinkingwatershared, w=3,m=2,b=1),
         p_generalwatershared = poly_asset_num(p_generalwatershared, w=3,m=2,b=1)
         ) %>% 
  mutate(f_sharedtoilet = poly_asset_cat(p_sharedtoilet,w=0,m=1,b=2),
         f_drinkingwater = poly_asset_cat(p_drinkingwater, w=0,m=1,b=2),
         f_generalwater = poly_asset_cat(p_generalwater, w=0,m=1,b=2),
         f_drinkingwatershared = poly_asset_cat(p_drinkingwatershared, w=0,m=1,b=2),
         f_generalwatershared = poly_asset_cat(p_generalwatershared, w=0,m=1,b=2)
  ) %>% 
  mutate(c_crowding = case_when(!is.na(numroom) & numpeople >0 ~ numroom/numpeople,
                                TRUE ~ NA_real_))

# 2006-09 ---------------

in_2006 <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/Adult Phase 2 (2006-2009).sav")) %>% 
  
  rename(sharedtoilet = o2toilet,
         drinkingwater = o2drwaty,
         generalwater = o2gnwaty,
         drinkingwatershared = o2drwats,
         generalwatershared = o2gnwats,
         housetype = o2typeho,
         numroom = o2noroom,
         numpeople = o2nopeop,
         
         electricity = o2electr,
         fan = o2fan,
         bicycle = o2cycle,
         radio = o2radio,
         twowheeler = o2wheel2,
         stove = o2stove,
         television = o2tv,
         cable = o2cable,
         mixergrinder = o2mixer,
         cooler = o2cooler,
         washingmachine = o2wmachi,
         car = o2car,
         airconditioner = o2ac,
         computer = o2pc,
         dishtv = o2dish,
         telephone = o2lphone,
         cellphone = o2mobile
         
         ) %>% 
  rename_at(vars(sharedtoilet,drinkingwater,
                 generalwater,drinkingwatershared,
                 generalwatershared),~paste0("p_",.)) %>% 
  rename_at(vars(housetype,electricity,fan,bicycle,
                 radio,twowheeler,stove,television,
                 cable,mixergrinder,cooler,washingmachine,
                 car,airconditioner,computer,dishtv,
                 telephone,cellphone),~paste0("d_",.)) %>% 
  mutate(d_housetype = case_when(d_housetype %in% c(1,2) ~ 1,
                                 TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("d_")),~as.numeric(.)) %>% 
  mutate(p_sharedtoilet = poly_asset_num(p_sharedtoilet,w=3,m=2,b=1),
         p_drinkingwater = poly_asset_num(p_drinkingwater, w = 3,m=2,b=c(1,4,5)),
         p_generalwater = poly_asset_num(p_generalwater, w=3,m=2,b=c(1)),
         p_drinkingwatershared = poly_asset_num(p_drinkingwatershared, w=3,m=2,b=c(1,4,5)),
         p_generalwatershared = poly_asset_num(p_generalwatershared, w=3,m=2,b=c(1))
  ) %>% 
  mutate(f_sharedtoilet = poly_asset_cat(p_sharedtoilet,w=0,m=1,b=2),
         f_drinkingwater = poly_asset_cat(p_drinkingwater, w=0,m=1,b=2),
         f_generalwater = poly_asset_cat(p_generalwater, w=0,m=1,b=2),
         f_drinkingwatershared = poly_asset_cat(p_drinkingwatershared, w=0,m=1,b=2),
         f_generalwatershared = poly_asset_cat(p_generalwatershared, w=0,m=1,b=2)
  ) %>% 
  mutate(c_crowding = case_when(!is.na(numroom) & numpeople >0 ~ numroom/numpeople,
                                TRUE ~ NA_real_))

# 2012-16 -------------

in_2012 <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/NDBC APH 3 & 4 variables.sav")) %>% 
  dplyr::select(Id,starts_with("O3")) %>% 
  
  dplyr::select(-one_of("O3doe","O3marita","O3numchild","O3child",
                "O3educat","O3occupa")) %>% 
  rename(id = Id,
         
         sharedtoilet = O3toilet,
         drinkingwater = O3drwats,
         generalwater = O3gnwats,
         drinkingwatershared = O3drwaty,
         generalwatershared = O3gnwaty,
         housetype = O3typeho,
         numroom = O3noroom,
         numpeople = O3nopeop,
         
         electricity = O3electr,
         fan = O3fan,
         bicycle = O3cycle,
         radio = O3radio,
         twowheeler = O3wheel2,
         stove = O3stove,
         television = O3tv,
         cable = O3cable,
         mixergrinder = O3mixer,
         cooler = O3cooler,
         washingmachine = O3wmachi,
         car = O3car,
         airconditioner = O3ac,
         computer = O3pc,
         dishtv = O3dish,
         telephone = O3lphone,
         cellphone = O3mobile
         ) %>% 
  rename_at(vars(sharedtoilet,drinkingwater,
                 generalwater,drinkingwatershared,
                 generalwatershared),~paste0("p_",.)) %>% 
  rename_at(vars(housetype,electricity,fan,bicycle,
                 radio,twowheeler,stove,television,
                 cable,mixergrinder,cooler,washingmachine,
                 car,airconditioner,computer,dishtv,
                 telephone,cellphone),~paste0("d_",.)) %>% 
  mutate(d_housetype = case_when(d_housetype %in% c(1,2) ~ 1,
                                 d_housetype == 0 ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  mutate_at(vars(starts_with("d_")),~as.numeric(.)) %>% 
  mutate(p_sharedtoilet = poly_asset_num(p_sharedtoilet,w=3,m=2,b=1),
         p_drinkingwater = poly_asset_num(p_drinkingwater, w = 3,m=2,b=c(1,4,5)),
         p_generalwater = poly_asset_num(p_generalwater, w=3,m=2,b=c(1)),
         p_drinkingwatershared = poly_asset_num(p_drinkingwatershared, w=3,m=2,b=c(1,4,5)),
         p_generalwatershared = poly_asset_num(p_generalwatershared, w=3,m=2,b=c(1))
  ) %>% 
  mutate(f_sharedtoilet = poly_asset_cat(p_sharedtoilet,w=0,m=1,b=2),
         f_drinkingwater = poly_asset_cat(p_drinkingwater, w=0,m=1,b=2),
         f_generalwater = poly_asset_cat(p_generalwater, w=0,m=1,b=2),
         f_drinkingwatershared = poly_asset_cat(p_drinkingwatershared, w=0,m=1,b=2),
         f_generalwatershared = poly_asset_cat(p_generalwatershared, w=0,m=1,b=2)
  )  %>% 
  mutate(c_crowding = case_when(!is.na(numroom) & numpeople >0 ~ numroom/numpeople,
                                TRUE ~ NA_real_))

# 2016-19 ----------------

in_2016 <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/NDBC APH 3 & 4 variables.sav")) %>% 
  dplyr::select(Id,doe:marital) %>% 
  dplyr::select(-one_of("doe","numchild","child_total",
                        "edu","occ_cohort","fam_type",
                        "marital","toiletother","lightother","drinkwatother")) %>% 
  
  rename(id = Id,
         
         toilettype = toilet,
         drinkingwater = drinkwat,
         numroom = rooms,
         numpeople = noofpers,
         lighttype = light,
         separatekitchen = kitchen,
         ownhouse = owner,
         
         fan = fan,
         bicycle = Bicycle,
         radio = radioown,
         twowheeler = Moped,
         
         cooler = cooler,
         car = Car,
         airconditioner = Aircon,
         computer = comp,
         telephone = phone,
         cellphone = mobile,
         mattress = mattress,
         pressurecooker = cooker,
         chair = Chair,
         bed = Cot,
         table = Tableown,
         clock = Clock,
         televisionbw = TV_BW,
         televisioncolor = TV_colour,
         televisionplasma = TV_ps,
         waterpump = pump,
         bullockcart = Bullcart,
         thresher = Thresher,
         tractor = Tractor,
         refrigerator = refrig,
         sewingmachine = sewmac,
         internet = intnet
         ) %>% 
  mutate(sharedtoilet = toilettype,
         # stove = case_when(cookfuel %in% c(1,4,5,7) ~ 1,
         #                   cookfuel %in% c(2,3,6) ~ 0,
         #                   TRUE ~ NA_real_),
         electricity = case_when(lighttype == 1 ~ 1,
                                 # cookfuel == 1 ~ 1,
                                 !is.na(lighttype) ~ 0,
                                 # !is.na(cookfuel) ~ 0,
                                 TRUE ~ NA_real_)
         
         ) %>% 
  rename_at(vars(toilettype,
                 sharedtoilet,drinkingwater,
                 # cookfuel,
                 lighttype),~paste0("p_",.)) %>%
  
  rename_at(vars(electricity,fan,bicycle,
                 radio,twowheeler,
                 # stove,: to be created -------
                 televisionbw,televisioncolor,televisionplasma,
                 # cable,mixergrinder,: unavailable
                 cooler,
                 # washingmachine: unavailable
                 car,airconditioner,computer,
                 # dishtv,
                 telephone,cellphone,
                 mattress,pressurecooker,chair,
                 bed,table,clock,waterpump,
                 bullockcart,thresher,tractor,
                 refrigerator, sewingmachine,
                 internet, 
                 separatekitchen,ownhouse
                 ),~paste0("d_",.)) %>% 
  
  # PENDING d_housetype: to be created from roof, wall, floor -----------
# mutate(d_housetype = case_when(d_housetype %in% c(1,2) ~ 1,
#                                TRUE ~ 0)) %>% 
  mutate(d_television = case_when(d_televisionbw == 1 | d_televisioncolor == 1 | d_televisionplasma == 1 ~ 1,
                                  d_televisionbw == 0 & d_televisioncolor == 0 & d_televisionplasma == 0 ~ 0,
                                  is.na(d_televisionbw) & is.na(d_televisioncolor) & is.na(d_televisionplasma) ~ NA_real_,
                                  TRUE ~ 0)) %>%
  mutate_at(vars(starts_with("d_")),~as.numeric(.)) %>% 
  mutate(p_sharedtoilet = poly_asset_num(p_sharedtoilet,w=c(3,6,7),m=c(2,5),b=c(1,4)),
         p_toilettype = poly_asset_num(p_toilettype,w=c(6,7),m=c(3:5),b=c(1:2)),
         p_drinkingwater = poly_asset_num(p_drinkingwater, w = c(4,5),m=c(2,3),b=c(1,6,7)),
         # p_cookfuel = poly_asset_num(p_cookfuel,w=c(2,3),m=c(6,7),b=c(1,4,5)),
         p_lighttype = poly_asset_num(p_lighttype, w=3,m=c(2,4),b=1)
  ) %>% 
  mutate(f_sharedtoilet = poly_asset_cat(p_sharedtoilet,w=0,m=1,b=2),
         f_toilettype = poly_asset_cat(p_toilettype,w=0,m=1,b=2),
         f_drinkingwater = poly_asset_cat(p_drinkingwater, w=0,m=1,b=2),
         # f_cookfuel = poly_asset_cat(p_cookfuel, w=0,m=1,b=2),
         f_lighttype = poly_asset_cat(p_lighttype, w=0,m=1,b=2)
  ) %>% 
  mutate(c_crowding = case_when(!is.na(numroom) & numpeople >0 ~ numroom/numpeople,
                                TRUE ~ NA_real_))


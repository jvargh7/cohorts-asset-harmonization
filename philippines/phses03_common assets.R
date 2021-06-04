
housing_vars <- c(
                # Not there in 2007....  
                "p_sourcedw",
                "p_toilet",
                "p_garbage",
                "p_litetype",
                "p_cookfuel",
                "p_gencond",
                "p_areafood",
                "p_material",
                "p_neighcon", # Not there in 1983
                "p_neighgar", # Not there in 1983
                "p_neighmat", # Not there in 1983, 1991, 2007
                "p_foodstor", # Not there 1983-1994
                "p_settlemn", # Not there 1983-1994
                "p_houseneat", # Not there 1983-1994
                
                "d_electric", # Not there in 1983
                "d_elecserv", # Not there in 1983
                
                "c_crowding", # Not there in 2007, 2009
                # "roomnumb", # Not there in 2007, 2009
                # "numbper", # Present in all years
                # "tabsent", # Not there in 1983
                
                # Available in 2007...
                "d_ownhouse",
                "d_ownlot" # Not there 1991-1998
                )

asset_vars <- c(
                # Electrical appliances
                "d_aircon",
                "d_elecfan",
                "d_otherappl",
                "d_sewingmachine", # Not there 1991-1998
                "d_vacuum", # Not there 1991-1998
                
                # Entertainment devices
                "d_cdplayer", # Not there 1983-1998
                
                "r_dvcamera", # Not there 1983-1991
                # "d_dvcamera","d_videocasrec"
                
                "d_karaoke", # Not there 1983-1998
                
                "d_record", # Tape recorder/Stereo # Not there 1994-2002
                "d_vcr", # Not there 1983-1998
                
                "r_tv",
                # d_owntv, "d_blackwtv","d_colortv",
                
                "r_phone", # Not there 1983, 1991
                # "d_cellphone", "d_telephone",
                
                "r_computer", # Not there 1983-1998
                # d_computer, d_internet,
                
                # Household furniture and items
                #"d_beds", "d_bedwmatt","d_bedwomat",
                "r_beds", 
                "d_drawers", # Not there from 2002-2018
                "d_cabinet",
                "d_livinset",
                "d_dining", # Not there 1994, 1998
                "d_otherfurni",
                
                # Vehicles
                
                
                
                "d_bicycle", 
                
                "r_otherveh",
                # "d_bikecar", # Not there 1983-1998
                # "d_motorcar", # Motorcycle with sidecar # Not there 1983-1998
                # "d_otherveh",
                # "r_banca", # Not there 1983-1998; added to r_otherveh
                # d_banca, d_bancab
                
                "r_boat",
                # "d_boat", "d_marinegn",
                # "d_bus", "d_truckbus","d_trucks",
                "r_truckbus",
                "d_car",
                "d_jepny",
                "d_motobike", # Not there 1983, 1991
                
                 
                "d_tractor",
                
                # Animals
                "d_chicken",
                "r_cattle",
                # "d_cows", "d_carabaos", 
                "r_farm",
                # "d_goats", "d_horses", "d_pigs","d_otheranimals",
                
                # Kitchen appliances
                "r_gastove", # Not there 1991-1998
                # "d_gastove", "d_gasrange"
                
                "d_kerstove", # Not there 1991-1998
                "d_micwave", # Not there 1983-1998
                "d_oven", # Not there 1983-1998
                "d_prcooker", # Not there 1983-1998
                "d_refrigerator",
                "d_rcooker" # Not there 1983-1998
                ) 


availability_pcall <- pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_"),starts_with("n_")) %>% 
  group_by(year) %>% 
  summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("year"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)

# Selection ----------

years = c(1983, 1991, 1994, 1998, 2002, 2005, 2009, 2018)


# exclude_vars = c("r_cattle","r_farm","d_chicken")
exclude_vars = c(
                 "d_dvcamera","d_videocasrec",
                 "d_owntv","d_blackwtv","d_colortv",
                 "d_cellphone","d_telephone",
                 "d_computer","d_internet",
                 "d_beds","d_bedwmatt","d_bedwomat",
                 "d_banca","d_bancab",
                 "d_boat","d_marinegn",
                 "d_bus","d_truckbus","d_trucks",
                 "d_bikecar","d_banca","d_bancab","d_motorcar","d_otherveh",
                 "d_cows","d_carabaos",
                 "d_goats","d_horses","d_pigs","d_otheranimals",
                 "d_gastove","d_gasrange"
                 
                 )
pca_vars <-availability_pcall %>%
  dplyr::filter(year %in% years ) %>% 
  dplyr::select(year,var_name) %>%
  group_by(var_name) %>%
  tally() %>% 
  dplyr::filter(n >=length(years)-1,!var_name %in% exclude_vars) %>% 
  dplyr::select(var_name) %>% 
  pull()



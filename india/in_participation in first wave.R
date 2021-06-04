

var_order <- c('c_crowding', 
               'd_airconditioner', 
               'd_bed', 
               'd_bicycle', 
               'd_bullockcart', 
               'd_cable', 
               'd_car', 
               'd_cellphone', 
               'd_chair', 
               'd_clock', 
               'd_computer', 
               'd_cooler', 
               'd_dishtv', 
               'd_electricity', 
               'd_fan', 
               'd_housetype', 
               'd_internet', 
               'd_mattress', 
               'd_mixergrinder', 
               'd_ownhouse', 
               'd_pressurecooker', 
               'd_radio', 
               'd_refrigerator', 
               'd_separatekitchen', 
               'd_sewingmachine', 
               'd_stove', 
               'd_table', 
               'd_telephone', 
               'd_television', 
               'd_televisionbw', 
               'd_televisioncolor', 
               'd_televisionplasma', 
               'd_thresher', 
               'd_tractor', 
               'd_twowheeler', 
               'd_washingmachine', 
               'd_waterpump', 
               'f_drinkingwater', 
               'f_drinkingwatershared', 
               'f_generalwater', 
               'f_generalwatershared', 
               'f_housetype', 
               'f_lighttype', 
               'f_sharedtoilet', 
               'f_toilettype'
               
               
)

india_pcall <- readRDS(paste0(path_harmonization_folder,"/india/working/pcall.RDS"))  %>% 
  dplyr::select(year,id,one_of(var_order)) %>% 
  mutate(non_na_assets = apply(.[,var_order],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
  dplyr::filter(non_na_assets > 0)

assets1969_ids <- india_pcall %>% 
  dplyr::filter(year == 1969) %>% 
  dplyr::select(id) %>% 
  pull()

table(india_pcall$year)/8181

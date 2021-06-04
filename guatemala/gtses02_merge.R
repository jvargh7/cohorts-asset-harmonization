
source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))

non_gtml2018 <- ur %>% 
  dplyr::filter(urbano_rural2018 == 2) %>% 
  dplyr::select(iduni) %>% 
  pull()

non_gtml2016 <- ur %>% 
  dplyr::filter(urbano_rural2015 == 2) %>% 
  dplyr::select(iduni) %>% 
  pull()

pcall_bmc <- readRDS(paste0(path_harmonization_folder,"/guatemala/pcall for BMC paper.RDS"))  %>% 
  dplyr::filter(!(census == 2018 & id_uni %in% non_gtml2018)&!(census == 2016 & id_uni %in% non_gtml2016))
  

# Check ------
pcall_sas7bdat <- haven::read_sas(paste0(path_incap_ses_raw_hc,"/data_Apr06/SAS/ses/pcall.sas7bdat")) 
names(pcall_sas7bdat)[!names(pcall_sas7bdat) %in% names(pcall_bmc)]


pcall <- pcall_bmc %>% 

# Recoding from INCAP SES Harmonization paper ------------------

  rename(numpersonshouse = v06,
         numpersonsfamily = v07,
         
         ownsite = sitio90,
         ownhouse = tenen_7,
         electricity = luz,
         # floor = piso,
         # roof = techo,
         # wall = pared,
         # kitchen = cocin,
         # stove = poele,
         # toilet = letrin,
         # water = abasag,
         # sewage = desag,
         # garbage = garbage,
         
         telephone = telefono,
         cellphone = cellphone,
         radio = radio,
         cassetteplayer = cassette,
         turntable = tocad,
         soundequipment = equipo,
         ipod = ipod,
         television = tv,
         videoplayer = video,
         cable = cable,
         internet = inter,
         directtv = directtv,
         bicycle = bike,
         motorcycle = moto,
         car = auto,
         sewingmachine = coser,
         refrigerator = frig,
         washingmachine = washingmachine,
         microwave = micro,
         handgrinder = molino,
         blender = licua,
         electriciron = plancha,
         birds = aves,
         pigs = cerdos,
         n_birds = qaves,
         n_pigs = qcerdos,
         typewriter = maqescr,
         computer = compu,
         c_crowding = cuartv07
         ) %>% 
  rename_at(vars(ownsite,ownhouse,electricity,
                 telephone,cellphone,radio,
                 cassetteplayer,turntable,
                 soundequipment,ipod,
                 television, videoplayer,
                 cable, internet,
                 directtv,bicycle,
                 motorcycle,car,
                 sewingmachine, refrigerator,
                 washingmachine,microwave,
                 handgrinder,blender,
                 electriciron,birds,
                 pigs, typewriter,computer), ~paste0("d_",.)) %>% 
  
  
  mutate(p_floor = case_when(piso_1 == 1 ~ 0,
                              piso_2 == 1 | piso_3 == 1 ~ 1,
                              piso_4 == 1 | piso_5 == 1 ~ 2,
                              TRUE ~ 0),
         p_roof = case_when(techo_1 == 1 ~ 0,
                               techo_2 == 1 | techo_3 == 1  ~ 1,
                               techo_4 == 1 | techo_5 == 1 ~ 2,
                               TRUE ~ 0),
         p_wall = case_when(pared_1 == 1 | pared_2 == 1 | pared_3 == 1  ~ 0,
                               pared_8 == 1 ~ 1, # Paul suggested 0 but earlier census waves code it high. Hence consensus at Medium
                               pared_4 == 1 | pared_5 == 1 | pared_6 == 1 ~ 1,
                               pared_7 == 1 ~ 2,
                               TRUE ~ 0),
         p_kitchen = case_when(cocin_1 ==1 | cocin_2 == 1 ~ 0,
                               cocin_3 == 1 ~ 1,
                               cocin_4 == 1 ~ 2,
                               TRUE ~ 0),
         p_stove = case_when(poele_1 == 1 | poele_2 == 1 ~ 0,
                               poele_3 == 1 | poele_4 | poele_5 == 1 | poele_6 == 1 ~ 1,
                               poele_78 == 1 | poele_9 == 1 | poele_10 == 1 ~ 2,
                               TRUE ~ 0),
         p_toilet = case_when(letrin_1 == 1 ~ 0,
                                letrin_2 == 1 | letrin_5 == 1 ~ 1,
                                letrin_3 == 1 | letrin_4 == 1 ~ 2,
                                TRUE ~ 0),
         p_water = case_when(abasag_4 == 1 ~ 2,
                                (abasag_2 == 1 | abasag_3 == 1) & abasag_4 == 0 ~ 1,
                                abasag_1 == 1 ~ 0,
                                TRUE ~ 0),
         p_sewage = case_when(desag_3 == 1 | desag_4 == 1 ~ 2,
                               desag_2  == 1 & (desag_3 == 0 & desag_4 == 0) ~ 1,
                               desag_1  == 1 & (desag_2 ==0 & desag_3 == 0 & desag_4 == 0)   ~ 0,
                               TRUE ~ 0),
         p_garbage = case_when(garbage_5 == 1 ~ 2,
                                 garbage_2 == 1 | garbage_3 == 1 | garbage_4 == 1 ~ 1,
                                 garbage_1 == 1 ~ 0,
                                 TRUE ~ NA_real_)
  ) %>% 
  # mutate_at(vars(starts_with("p_")), function(x) factor(x,levels=c(0,1,2), labels=c("low","med","high"))) %>% 
  mutate(
    
    f_floor = poly_asset_cat(p_floor,w=0,m=1,b=2),
    f_roof = poly_asset_cat(p_roof,w=0,m=1,b=2),
    f_wall = poly_asset_cat(p_wall,w=0,m=1,b=2),
    f_kitchen = poly_asset_cat(p_kitchen,w=0,m=1,b=2),
    f_stove = poly_asset_cat(p_stove,w=0,m=1,b=2),
    f_toilet = poly_asset_cat(p_toilet,w=0,m=1,b=2),
    f_water = poly_asset_cat(p_water,w=0,m=1,b=2),
    f_sewage = poly_asset_cat(p_sewage,w=0,m=1,b=2),
    f_garbage = poly_asset_cat(p_garbage,w=0,m=1,b=2)
    
  ) %>% 
  
  dplyr::select(familia,comuni,census,id_uni, starts_with("c_"),starts_with("d_"),starts_with("p_"),starts_with("f_"))

write.csv(head(pcall),paste0(path_harmonization_folder,"/guatemala/gt_head_pcall.csv"))

# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
gtpcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/guatemala/GT Variable List.xlsx"),sheet="pcall")

pcall <- label_variables(pcall,cohorts_labels = gtpcall_labels,overwrite_var_label = TRUE)

# SAVE ---------------
saveRDS(pcall,paste0(path_harmonization_folder,"/guatemala/working/pcall.RDS"))
write_dta(pcall,paste0(path_harmonization_folder,"/guatemala/working/pcall.dta"),version=12)

# pcall <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pcall.RDS"))

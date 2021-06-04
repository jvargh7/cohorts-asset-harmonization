

sesairic <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/sesairic/sesairic.dta")) %>% 
  dplyr::filter(!is.na(icsex))

merged_check <- data.frame(
  year = c(1983,1991,1994,1998,2002,2005,2007,2009,2018),
  ije2011 = c(3080,2264,2186,2089,2023,1888,1817,NA,NA),
  sesairic = c(
    sum(!is.na(sesairic$basebrgy)),
    sum(!is.na(sesairic$currbrgy91)),
    sum(!is.na(sesairic$currbrgy94)),
    sum(!is.na(sesairic$currbrgy98)),
    sum(!is.na(sesairic$currbrgy02)),
    sum(!is.na(sesairic$currbrgy05)),
    NA,
    NA,
    NA),
  
  twin_recs = c(sum(assets83$uncmomid %in% uncmomid_twins),
                sum(assets91$uncmomid %in% uncmomid_twins),
                sum(assets94$uncmomid %in% uncmomid_twins),
                sum(assets98$uncmomid %in% uncmomid_twins),
                sum(assets02m$uncmomid %in% uncmomid_twins),
                sum(assets05m$uncmomid %in% uncmomid_twins),
                sum(assets07m$uncmomid %in% uncmomid_twins),
                NA,NA
                ),
  
  unique_moms = c(length(unique(assets83$uncmomid)),
             length(unique(assets91$uncmomid)),
             length(unique(assets94$uncmomid)),
             length(unique(assets98$uncmomid)),
             length(unique(c(assets02c$uncmomid,assets02m$uncmomid))),
             length(unique(c(assets05c$uncmomid,assets05m$uncmomid))),
             length(unique(c(assets07c$uncmomid,assets07m$uncmomid))),
             length(unique(assets09c$uncmomid)),
             length(unique(assets18c$uncmomid))
             ),
  nobs = c(nrow(assets83),
           nrow(assets91),
           nrow(assets94),
           nrow(assets98),
           nrow(assets02c) + nrow(assets02m),
           nrow(assets05c) + nrow(assets05m),
           nrow(assets07c) + nrow(assets07m),
           nrow(assets09c),
           nrow(assets18c)
           )
  
)


pcall <- plyr::rbind.fill(
  
  assets83 %>% mutate(year = 1983,source = "mother"),
  assets91 %>% mutate(year = 1991,source = "mother"),
  assets94 %>% mutate(year = 1994,source = "mother"),
  assets98 %>% mutate(year = 1998,source = "mother"),
  assets02c %>% mutate(year = 2002,source = "index"),
  assets02m %>% mutate(year = 2002,source = "mother"),
  assets05c %>% mutate(year = 2005,source = "index"),
  assets05m %>% mutate(year = 2005,source = "mother"),
  assets07c %>% mutate(year = 2007,source = "index"),
  assets07m %>% mutate(year = 2007,source = "mother"),
  assets09c %>% mutate(year = 2009,source = "index"),
  assets18c %>% mutate(year = 2018,source = "index")
  
)

# View(pcall %>% group_by(uncchdid,year) %>% tally())
# View(pcall %>% group_by(year) %>% tally())

# Recoding---------
pcall <- pcall %>% 
  mutate(
    r_dvcamera = multi_asset_binary(.,c("d_dvcamera","d_videocasrec")),
    r_tv = multi_asset_binary(.,c("d_owntv","d_blackwtv","d_colortv")),
    r_phone = multi_asset_binary(.,c("d_cellphone","d_telephone")),
    r_computer = multi_asset_binary(.,c("d_computer","d_internet")),
    r_beds = multi_asset_binary(.,c("d_beds","d_bedwmatt","d_bedwomat")),
    r_banca = multi_asset_binary(.,c("d_banca","d_bancab")),
    r_boat = multi_asset_binary(.,c("d_boat","d_marinegn")),
    r_truckbus = multi_asset_binary(.,c("d_bus","d_truckbus","d_trucks")),
    r_otherveh = multi_asset_binary(.,c("d_bikecar","d_banca","d_bancab","d_motorcar","d_otherveh")),
    r_cattle = multi_asset_binary(.,c("d_cows","d_carabaos")),
    r_farm = multi_asset_binary(.,c("d_goats","d_horses","d_pigs","d_otheranimals")),
    r_gastove = multi_asset_binary(.,c("d_gastove","d_gasrange"))
    
  )


structural_exclusion = c("brgy1991","currbrgy","wmanbrgy1991","chldbrgy1991",
                         "brgy1994","wmanbrgy1994","chldbrgy1994",
                         "brgy1998","brgy2002","brgy2005",
                         "currstra",
                         
                         "currwman","wmanid1991","chldwmanid1991",
                         "wmanid1994","chldwmanid1994",
                         "wmanid1998","wmanid2002",
                         "uncmomid1998","uncmomid2002","wmanid2005",
                         "uncmomid2005","uncmomid2007",
                         "moint1991",
                         "chint1991",
                         "mochint1994",
                         "status1994",
                         
                         "wmanhhid1991","chldhhid1991",
                         "wmanhhid1994","chldhhid1994",
                         
                         "hhid1994","hhid1998","hhid2002",
                         "hhid2005","currhhno","hhldid1991",
                         "status1991",
                         
                         "momint1994","chint1994",
                         "mochint1998","status1998",
                         "mochint2002","mochint2005",
                         "mochint2007"
)

# Exclude structural vars -------------

pcall <- pcall %>% 
  # Both cannot be easily used in analysis
  dplyr::select(-p_settlement,-p_foodpurchase) %>% 
  dplyr::select(-one_of(structural_exclusion))

# Restricting to singletons ---------------

pcall <- pcall %>% 
  dplyr::filter(uncchdid %in% uncchdid_singletons)

pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_"),starts_with("n_")) %>% 
  compareGroups::compareGroups(year ~ .,data=.,max.ylev = 10,include.label = FALSE) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()




write.csv(head(pcall),paste0(path_harmonization_folder,"/philippines/ph_head_pcall.csv"))




# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
phpcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="pcall")

pcall <- label_variables(pcall,cohorts_labels = phpcall_labels,overwrite_var_label = TRUE)

# SAVE ---------------
saveRDS(pcall,paste0(path_harmonization_folder,"/philippines/working/pcall.RDS"))
write_dta(pcall,paste0(path_harmonization_folder,"/philippines/working/pcall.dta"),version=12)

# CHECK --------
p_vars = names(pcall)[regexpr("^p_",names(pcall))>0] %>% str_replace(.,"p_","")
f_vars = names(pcall)[regexpr("^f_",names(pcall))>0] %>% str_replace(.,"f_","")
p_vars[!p_vars %in% f_vars]
f_vars[!f_vars %in% p_vars]

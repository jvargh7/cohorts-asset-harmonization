# source(paste0(path_harmonization_repo,"/guatemala/gtses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses02_merge.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses03_common assets.R"))
source(paste0(path_harmonization_repo,"/guatemala/gtses04_harmonized index.R"))

# Harmonized loadings ----------------
ses_pca_obj = data.frame(
  year = numeric(),
  iteration = character(),
  item = character(),
  PC1 = numeric()
) %>% 
  bind_rows(polychoric_out$weights[,] %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(year = 0,
                     iteration = "all") %>% 
              dplyr::select(year, iteration, row,PC1) %>% 
              rename(item = row))

ses_pca_imp = data.frame(
  year = numeric(),
  iteration = character(),
  importance = numeric()
) %>% 
  bind_rows(data.frame(importance = polychoric_out$eigen_values/sum(polychoric_out$eigen_values),
                       component = c(1:length(polychoric_out$eigen_values))
  ) %>% 
    pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
    mutate(year = 0,
           iteration = "all") %>% 
    rename(importance = importance1) %>% 
    dplyr::select(year, iteration, importance))

ses_spearman_cs <- expand.grid(years, "all") %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))

# Reading pcall (orig) -------------
pcall <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pcall.RDS"))

availability_pcall <- pcall %>% 
  dplyr::select(census,starts_with("p_"),starts_with("d_")) %>% 
  group_by(census) %>% 
  # summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  summarize_all(.funs=function(x)sum(x==0)) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("census"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)

# Selection ----------

years = c(1967,1975,1987,1996,2002,2016,2018)

include_vars <- c("c_crowding")

# exclude_vars = c("r_cattle","r_farm","d_chicken")
# exclude_vars = c(
#   "d_dvcamera","d_videocasrec",
#   "d_owntv","d_blackwtv","d_colortv",
#   "d_cellphone","d_telephone",
#   "d_computer","d_internet",
#   "d_beds","d_bedwmatt","d_bedwomat",
#   "d_banca","d_bancab",
#   "d_boat","d_marinegn",
#   "d_bus","d_truckbus","d_trucks",
#   "d_bikecar","d_banca","d_bancab","d_motorcar","d_otherveh",
#   "d_cows","d_carabaos",
#   "d_goats","d_horses","d_pigs","d_otheranimals",
#   "d_gastove","d_gasrange",
#   
#   # 2002
#   "d_ownlot","d_carriage"
#   
# )


pca_cs <- data.frame()


for (row in 1:nrow(ses_spearman_cs)){
  
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  
  temp_pca_vars <-availability_pcall %>%
    # dplyr::filter(census == y,!var_name %in% exclude_vars) %>% 
    dplyr::filter(census == y) %>% 
    dplyr::select(var_name) %>% 
    pull()
  
  temp_pca_vars <- c(include_vars,temp_pca_vars)
  
  temp_pca_df <- pcall %>% 
    dplyr::filter(census == y) %>% 
    dplyr::select(census,familia,comuni,id_uni, one_of(temp_pca_vars)) %>% 
    arrange(census,familia,comuni,id_uni)
  
  # if(y==1994){
  #   temp_pca_df <- temp_pca_df %>% dplyr::select(-d_electric,-p_litetype)
  #   temp_pca_vars <- temp_pca_vars[temp_pca_vars %in% names(temp_pca_df)]
  #   
  # }
  
  
  
  harmonized_index <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS")) %>% 
    dplyr::filter(census == y) %>% 
    dplyr::select(census,familia,comuni,id_uni, pc,pc2,pc3)  %>% 
    arrange(census,familia,comuni,id_uni)
  
  # if(y==1994){
  #   temp_pca_df <- temp_pca_df %>% dplyr::select(-d_electric,-p_litetype)
  # }
  # 
  
  temp_pca_df_unimputed <- temp_pca_df
  
  # source(paste0(path_harmonization_repo,"/guatemala/gt_pca_df_imputation.R"))
  
  # temp_pca_df[is.na(temp_pca_df)] <- 0
  temp_pca_df <- mode_impute(temp_pca_df,
                        ignore_vars = c("familia","comuni","id_uni"),
                        grouping_var = "census")
  
  # Removing zero-variance variables
  zero_var_indices <- caret::nearZeroVar(temp_pca_df)
  names_zero_var <- names(temp_pca_df)[zero_var_indices]
  
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-census,-familia,-comuni,-id_uni,
                  # -pc,-pc2,-pc3 - Removing these earlier
                  
    ) %>% 
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  
  if(y == 1967){
    temp_x = temp_pca_df %>% 
      dplyr::select(-census,-familia,-comuni,-id_uni) %>% 
      dplyr::select(-one_of(names_zero_var)) %>% 
      rename(d_kitchen = p_kitchen,
             d_roof = p_roof,
             d_toilet = p_toilet,
             d_wall = p_wall,
             d_water = p_water) %>% 
      as.matrix()
    
    temp_pca_vars = colnames(temp_x)
  }
  
  
  temp_pca_vars <- temp_pca_vars[!temp_pca_vars %in% names_zero_var]
  
  polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)
  
  temp_pca_df_unimputed[,c("pc", paste0("pc",c(2,3)))] <- polychoric_out$scores[,1:3]
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df_unimputed$pc,harmonized_index$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_spearman_cs[row,]$r = r
  
  if(i == "all"){
    print(y)
    warnings()
  }
  
  # Loadings --------
  ses_pca_obj = bind_rows(ses_pca_obj,
                          polychoric_out$weights[,] %>% 
                            data.frame() %>% 
                            mutate(row = rownames(.)) %>% 
                            mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                            mutate(year = y,
                                   iteration = i) %>% 
                            dplyr::select(year, iteration, row,PC1) %>% 
                            rename(item = row))
  
  ses_pca_imp = bind_rows(ses_pca_imp,
                          data.frame(importance = polychoric_out$eigen_values/sum(polychoric_out$eigen_values),
                                     component = c(1:length(polychoric_out$eigen_values))
                          ) %>% 
                            pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
                            mutate(year = y,
                                   iteration = i) %>% 
                            rename(importance = importance1) %>% 
                            dplyr::select(year, iteration, importance)
  )
  
  pca_cs <- bind_rows(
    pca_cs,
    temp_pca_df_unimputed
  )
  
}

# SAVE --------------



saveRDS(pca_cs,paste0(path_harmonization_folder,"/guatemala/working/pca_cs.RDS"))
write_dta(pca_cs,paste0(path_harmonization_folder,"/guatemala/working/pca_cs.dta"),version=12)

ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/correlation with cs all items indices.csv"),row.names = FALSE)

# Each row is a PC1 loading
ses_pca_obj %>%
  distinct(year,iteration,item,.keep_all = TRUE) %>% 
  pivot_wider(names_from="item",values_from="PC1") %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/cs all items indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/cs all items indices proportion of variance.csv"),row.names = FALSE)


source(paste0(path_harmonization_repo,"/guatemala/gtses08a_census link.R"))
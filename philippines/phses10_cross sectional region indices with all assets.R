# source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/philippines/phses02_merge.R"))
# source(paste0(path_harmonization_repo,"/philippines/phses03_common assets.R"))
source(paste0(path_harmonization_repo,"/philippines/phses04_harmonized index.R"))

# Cross-sectional ----------------
ses_pca_obj = data.frame(
  year = numeric(),
  cstratum = numeric(),
  iteration = character(),
  item = character(),
  PC1 = numeric()
) %>% 
  bind_rows(polychoric_out$weights[,] %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(year = 0,
                     cstratum = 0,
                     iteration = "all") %>% 
              dplyr::select(year, cstratum, iteration, row,PC1) %>% 
              rename(item = row))

ses_pca_imp = data.frame(
  year = numeric(),
  cstratum = numeric(),
  iteration = character(),
  importance = numeric()
) %>% 
  bind_rows(data.frame(importance = polychoric_out$eigen_values/sum(polychoric_out$eigen_values),
                       component = c(1:length(polychoric_out$eigen_values))
  ) %>% 
    pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
    mutate(year = 0,
           cstratum = 0,
           iteration = "all") %>% 
    rename(importance = importance1) %>% 
    dplyr::select(year, cstratum, iteration, importance))

# 1 = Urban
# 2 = Rural
ses_spearman_cs <- expand.grid(years, "all",c(1,2)) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))

# Reading pcall (orig) -------------
pcall <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pcall.RDS"))

# Merging with ph_region ------------
source(paste0(path_harmonization_repo,"/philippines/ph_region.R"))

pcall <- pcall %>% 
  left_join(ph_region,
            by = c("year" = "survey","uncchdid"))

availability_pcall <- pcall %>% 
  dplyr::select(year,cstratum,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_")) %>% 
  group_by(year,cstratum) %>% 
  summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of(c("year","cstratum")),names_to="var_name",values_to="obs_count") %>% 
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
  "d_gastove","d_gasrange",
  
  # 2002
  "d_ownlot","d_carriage"
  
)


pca_region <- data.frame()


for (row in 1:nrow(ses_spearman_cs)){
  
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  s = ses_spearman_cs[row,]$Var3
  
  temp_pca_vars <-availability_pcall %>%
    dplyr::filter(year == y,cstratum == s, !var_name %in% exclude_vars) %>% 
    dplyr::select(var_name) %>% 
    pull()
  
  temp_pca_df <- pcall %>% 
    dplyr::filter(year == y,cstratum == s) %>% 
    dplyr::select(uncchdid,year,cstratum, one_of(temp_pca_vars)) %>% 
    arrange(uncchdid)
  
  if(y==1994  & s == 2){
    temp_pca_df <- temp_pca_df %>% dplyr::select(-d_electric,-p_litetype)
    temp_pca_vars <- temp_pca_vars[temp_pca_vars %in% names(temp_pca_df)]
    
  }
  
  
  
  harmonized_index <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS")) %>% 
    dplyr::filter(year == y) %>% 
    left_join(ph_region,
              by = c("year" = "survey","uncchdid")) %>% 
    dplyr::select(uncchdid,cstratum,year, pc,pc2,pc3)  %>% 
    dplyr::filter(cstratum == s) %>% 
    arrange(uncchdid)
  
  temp_pca_df_unimputed <- temp_pca_df
  
  # source(paste0(path_harmonization_repo,"/philippines/ph_pca_df_imputation.R"))
  
  # temp_pca_df[is.na(temp_pca_df)] <- 0
  
  temp_pca_df <- mode_impute(temp_pca_df,
                             ignore_vars = c("uncchdid"),
                             grouping_var = "year")
  
  # Removing zero-variance variables
  zero_var_indices <- caret::nearZeroVar(temp_pca_df)
  names_zero_var <- names(temp_pca_df)[zero_var_indices]
  
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-year,-uncchdid 
                  # -pc,-pc2,-pc3 - Removing these earlier
                  
    ) %>% 
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  
  temp_pca_vars <- temp_pca_vars[!temp_pca_vars %in% names_zero_var]
  
  temp_polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)   
  
  temp_pca_df_unimputed[,c("pc", paste0("pc",c(2,3)))] <- temp_polychoric_out$scores[,1:3]
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df_unimputed$pc,harmonized_index$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_spearman_cs[row,]$r = r
  
  # if(i == "all"){
  #   print(y)
  #   summary(temp_pca_obj) %>% print()
  # }
  
  # Loadings --------
  ses_pca_obj = bind_rows(ses_pca_obj,
                          temp_polychoric_out$weights[,] %>% 
                            data.frame() %>% 
                            mutate(row = rownames(.)) %>% 
                            mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                            mutate(year = y,
                                   cstratum = s,
                                   iteration = i) %>% 
                            dplyr::select(year, cstratum, iteration, row,PC1) %>% 
                            rename(item = row))
  
  ses_pca_imp = bind_rows(ses_pca_imp,
                          data.frame(importance = temp_polychoric_out$eigen_values/sum(temp_polychoric_out$eigen_values),
                                     component = c(1:length(temp_polychoric_out$eigen_values))
                          ) %>% 
                            pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
                            mutate(year = y,
                                   cstratum = s,
                                   iteration = i) %>% 
                            rename(importance = importance1) %>% 
                            dplyr::select(year, cstratum,iteration, importance)
  )
  
  pca_region <- bind_rows(
    pca_region,
    temp_pca_df_unimputed
  )
  
}

# SAVE --------------


saveRDS(pca_region,paste0(path_harmonization_folder,"/philippines/working/pca_region.RDS"))
write_dta(pca_region,paste0(path_harmonization_folder,"/philippines/working/pca_region.dta"),version=12)

ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/philippines/working/correlation with cs all items region indices.csv"),row.names = FALSE)

# Each row is a PC1 loading

ses_pca_obj %>%
  distinct(year,iteration,cstratum,item,.keep_all = TRUE) %>% 
  dplyr::filter(year == 1983) %>% 
  pivot_wider(names_from="cstratum",values_from="PC1") %>% 
  View()

ses_pca_obj %>%
  distinct(year,iteration,cstratum,item,.keep_all = TRUE) %>% 
  pivot_wider(names_from="item",values_from="PC1") %>%
  write.csv(.,paste0(path_harmonization_folder,"/philippines/working/cs all items region indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/philippines/working/cs all items region indices proportion of variance.csv"),row.names = FALSE)

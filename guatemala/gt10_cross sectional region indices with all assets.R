# source(paste0(path_harmonization_repo,"/guatemala/gtses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses02_merge.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses03_common assets.R"))
source(paste0(path_harmonization_repo,"/guatemala/gtses04_harmonized index.R"))

years = c(2016,2018)
items = c("all")

# Imputation ------------
source(paste0(path_harmonization_repo,"/guatemala/gt_region.R"))
rm(ur_df) # This is based on pca_df which is only using pca_vars 

# with(pca_df,table(year,cstratum))

# Cross-sectional ----------------
ses_pca_obj = data.frame(
  year = numeric(),
  ur = numeric(),
  iteration = character(),
  item = character(),
  PC1 = numeric()
) %>% 
  bind_rows(polychoric_out$weights[,] %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(year = 0,
                     ur = 0,
                     iteration = "all") %>% 
              dplyr::select(year, ur, iteration, row,PC1) %>% 
              rename(item = row))

ses_pca_imp = data.frame(
  year = numeric(),
  ur = numeric(),
  iteration = character(),
  importance = numeric()
) %>% 
  bind_rows(data.frame(importance = polychoric_out$eigen_values/sum(polychoric_out$eigen_values),
                       component = c(1:length(polychoric_out$eigen_values))
  ) %>% 
    pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
    mutate(year = 0,
           ur = 0,
           iteration = "all") %>% 
    rename(importance = importance1) %>% 
    dplyr::select(year, ur, iteration, importance))

# 1 = Urban
# 2 = Rural
ses_spearman_cs <- expand.grid(years, "all",c(1,2)) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))


# Reading pcall (orig) -------------
pcall <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pcall.RDS"))
ur_df <- bind_rows(pcall %>% 
            dplyr::filter(census == 2016) %>% 
            left_join(gt_region %>% 
                        dplyr::select(iduni,urbano_rural2015) %>% 
                        rename(ur = urbano_rural2015),
                      by = c("id_uni" = "iduni")),
            pcall %>% 
            dplyr::filter(census == 2018) %>% 
            full_join(gt_region %>% 
                        
                        dplyr::select(iduni,urbano_rural2018) %>% 
                        mutate(urbano_rural2018 = as.numeric(urbano_rural2018)) %>% 
                        rename(ur = urbano_rural2018),
                      by = c("id_uni" = "iduni"))) %>% 
  dplyr::filter(!is.na(census))


availability_pcall <- ur_df %>% 
  dplyr::select(census,ur,starts_with("p_"),starts_with("d_")) %>% 
  group_by(census,ur) %>% 
  # summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  summarize_all(.funs=function(x)sum(x==0)) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("census","ur"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)

# Selection ----------

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



pca_region <- data.frame()

for (row in 1:nrow(ses_spearman_cs)){
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  s = ses_spearman_cs[row,]$Var3
  
  temp_pca_vars <-availability_pcall %>%
    # dplyr::filter(census == y,!var_name %in% exclude_vars) %>% 
    dplyr::filter(census == y,ur==s) %>% 
    dplyr::select(var_name) %>% 
    pull()
  
  temp_pca_vars <- c(include_vars,temp_pca_vars)
  
  temp_pca_df <- ur_df %>% 
    dplyr::filter(census == y,ur==s) %>% 
    dplyr::select(census,familia,comuni,id_uni,ur, one_of(temp_pca_vars)) %>% 
    arrange(census,familia,comuni,id_uni)
  
  harmonized_index <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS")) %>% 
    dplyr::filter(census == y,id_uni %in% temp_pca_df$id_uni) %>% 
    dplyr::select(census,familia,comuni,id_uni, pc,pc2,pc3)  %>% 
    arrange(census,familia,comuni,id_uni)
  
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
    dplyr::select(-census,-familia,-comuni,-id_uni,-ur,
                  # -pc,-pc2,-pc3 - Removing these earlier
                  
    ) %>% 
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  
  # Equivalent
  # temp_pca_vars <- temp_pca_vars[!temp_pca_vars %in% names_zero_var]
  temp_pca_vars = colnames(temp_x)
  
  polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)
  
  temp_pca_df_unimputed[,c("pcr", paste0("pcr",c(2,3)))] <- polychoric_out$scores[,1:3]
  
  
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df_unimputed$pcr,harmonized_index$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_spearman_cs[row,]$r = r
  
  ses_pca_obj = bind_rows(ses_pca_obj,
                          polychoric_out$weights[,] %>% 
                            data.frame() %>% 
                            mutate(row = rownames(.)) %>% 
                            mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                            mutate(year = y,
                                   ur = s,
                                   iteration = i) %>% 
                            dplyr::select(year, ur, iteration, row,PC1) %>% 
                            rename(item = row))
  
  ses_pca_imp = bind_rows(ses_pca_imp,
                          data.frame(importance = polychoric_out$eigen_values/sum(polychoric_out$eigen_values),
                                     component = c(1:length(polychoric_out$eigen_values))
                          ) %>% 
                            pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
                            mutate(year = y,
                                   ur = s,
                                   iteration = i) %>% 
                            rename(importance = importance1) %>% 
                            dplyr::select(year, ur, iteration, importance)
  )
  
  pca_region <- bind_rows(
    pca_region,
    temp_pca_df_unimputed %>% 
      dplyr::select(census,familia,comuni,id_uni,ur,pcr,pcr2,pcr3)
  )
  
  
}

saveRDS(pca_region,paste0(path_harmonization_folder,"/guatemala/working/pca_region.RDS"))
write_dta(pca_region,paste0(path_harmonization_folder,"/guatemala/working/pca_region.dta"),version=12)


ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/correlation with cs region all assets indices.csv"),row.names = FALSE)

# Each row is a PC1 loading
ses_pca_obj %>%
  distinct(year,iteration,ur,item,.keep_all = TRUE) %>% 
  pivot_wider(names_from="item",values_from="PC1") %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/cs region all assets indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/cs region all assets indices proportion of variance.csv"),row.names = FALSE)

source(paste0(path_harmonization_repo,"/guatemala/gtses10a_region link.R"))

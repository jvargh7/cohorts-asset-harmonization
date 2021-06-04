# source(paste0(path_harmonization_repo,"/guatemala/gtses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses02_merge.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses03_common assets.R"))
source(paste0(path_harmonization_repo,"/guatemala/gtses04_harmonized index.R"))

years = c(2016,2018)
items = c("all")

pca_df <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS")) %>% 
  dplyr::filter(census %in% years) %>% 
  dplyr::select(census,familia,comuni,id_uni,one_of(pca_vars),pc,pc2,pc3)

# pca_df_unimputed <- pca_df

# Imputation ------------

source(paste0(path_harmonization_repo,"/guatemala/gt_pca_df_imputation.R"))
source(paste0(path_harmonization_repo,"/guatemala/gt_region.R"))


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

pca_region <- data.frame()

for (row in 1:nrow(ses_spearman_cs)){
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  s = ses_spearman_cs[row,]$Var3
  
  
  temp_pca_df = ur_df %>% 
    dplyr::filter(census == y,ur == s) %>% 
    
    dplyr::select(-one_of(i))
  
  # if(y==1994 & s == 2){
  #   temp_pca_df <- temp_pca_df %>% dplyr::select(-d_electric,-p_litetype)
  # }
  
  # Removing zero-variance variables
  zero_var_indices <- caret::nearZeroVar(temp_pca_df)
  names_zero_var <- names(temp_pca_df)[zero_var_indices]
  
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-census,-familia,-comuni,-id_uni,-ur,-pc,-pc2,-pc3) %>%
    
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  

  temp_pca_vars = colnames(temp_x)
 
  polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)
  
  temp_pca_df[,c("pcr", paste0("pcr",c(2,3)))] <- polychoric_out$scores[,1:3]
  
  
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df$pcr,temp_pca_df$pc,method = "spearman")),fmt = "%0.2f")
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
    temp_pca_df %>% 
      dplyr::select(census,familia,comuni,id_uni,ur,pcr,pcr2,pcr3,pc,pc2,pc3)
  )
  
  
}

# saveRDS(pca_region,paste0(path_harmonization_folder,"/guatemala/working/pca_region.RDS"))
# write_dta(pca_region,paste0(path_harmonization_folder,"/guatemala/working/pca_region.dta"),version=12)


ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/correlation with cs region indices.csv"),row.names = FALSE)

# Each row is a PC1 loading
ses_pca_obj %>%
  distinct(year,iteration,ur,item,.keep_all = TRUE) %>% 
  pivot_wider(names_from="item",values_from="PC1") %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/cs region indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/cs region indices proportion of variance.csv"),row.names = FALSE)

# source(paste0(path_harmonization_repo,"/guatemala/gtses07a_region link.R"))

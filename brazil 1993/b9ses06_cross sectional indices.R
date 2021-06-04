# source(paste0(path_harmonization_repo,"/brazil 1993/b9ses01_recoding variables.R"))
# source(paste0(path_harmonization_repo,"/brazil 1993/b9ses02_merge.R"))
# source(paste0(path_harmonization_repo,"/brazil 1993/b9ses03_common assets.R"))
source(paste0(path_harmonization_repo,"/brazil 1993/b9ses04_harmonized index.R"))

years = years
items = c("all")

pca_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS")) %>% 
  dplyr::filter(year %in% years) %>% 
  dplyr::select(year,nquest,one_of(pca_vars),pc,pc2,pc3)


# Imputation ------------

source(paste0(path_harmonization_repo,"/brazil 1993/b9_pca_df_imputation.R"))


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

ses_spearman_cs <- expand.grid(years,items) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))

for (row in 1:nrow(ses_spearman_cs)){
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  
  temp_pca_df = pca_df %>% 
    dplyr::filter(year == y) %>% 
    dplyr::select(-one_of(i))
  
  # Removing zero-variance variables
  zero_var_indices <- caret::nearZeroVar(temp_pca_df)
  names_zero_var <- names(temp_pca_df)[zero_var_indices]
  
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-year,-nquest,-pc,-pc2,-pc3) %>% 
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  temp_pca_vars = names(temp_x)
  
  temp_polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)   
  
  temp_pca_df[,c("temp_pc", paste0("temp_pc",c(2,3)))] <- temp_polychoric_out$scores[,1:3]
  
  
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df$temp_pc,temp_pca_df$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_spearman_cs[row,]$r = r
  
  ses_pca_obj = bind_rows(ses_pca_obj,
                          temp_polychoric_out$weights[,] %>% 
                            data.frame() %>% 
                            mutate(row = rownames(.)) %>% 
                            mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                            mutate(year = y,
                                   iteration = i) %>% 
                            dplyr::select(year, iteration, row,PC1) %>% 
                            rename(item = row))
  
  ses_pca_imp = bind_rows(ses_pca_imp,
                          data.frame(importance = temp_polychoric_out$eigen_values/sum(temp_polychoric_out$eigen_values),
                                     component = c(1:length(temp_polychoric_out$eigen_values))
                          ) %>% 
                            pivot_wider(names_from = "component",names_prefix="importance",values_from="importance") %>% 
                            mutate(year = y,
                                   iteration = i) %>% 
                            rename(importance = importance1) %>% 
                            dplyr::select(year, iteration, importance)
  )
  
}

ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/brazil 1993/working/correlation with cs indices.csv"),row.names = FALSE)

# Each row is a PC1 loading
ses_pca_obj %>%
  pivot_wider(names_from="item",values_from=PC1) %>%
  write.csv(.,paste0(path_harmonization_folder,"/brazil 1993/working/cs indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/brazil 1993/working/cs indices proportion of variance.csv"),row.names = FALSE)

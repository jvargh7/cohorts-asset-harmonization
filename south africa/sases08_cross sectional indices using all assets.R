# source(paste0(path_harmonization_repo,"/south africa/sases01_recoding variables.R"))
# source(paste0(path_harmonization_repo,"/south africa/sases02_merge.R"))
# source(paste0(path_harmonization_repo,"/south africa/sases03_common assets.R"))
source(paste0(path_harmonization_repo,"/south africa/sases04_harmonized index.R"))


# Harmonized loadings ----------

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

years = years
items = c("all")

ses_spearman_cs <- expand.grid(years,items) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))


# Reading pcall (orig) -------------
pcall <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pcall.RDS"))

availability_pcall <- pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_")) %>% 
  group_by(year) %>% 
  summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("year"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)





pca_cs <- data.frame()


for (row in 1:nrow(ses_spearman_cs)){
  
  exclude_vars = c("p_watertype","p_toilettype","d_cable")
  # exclude_vars = c(NULL)
  
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  
  if(y == 2012){
    exclude_vars = c(exclude_vars,"r_toilet2groups","r_water2groups")
  }
  if(y == 2018){
    exclude_vars = c(exclude_vars,"d_radio","d_electricity")
  }
  
  temp_pca_vars <-availability_pcall %>%
    dplyr::filter(year == y,!var_name %in% exclude_vars) %>% 
    dplyr::select(var_name) %>% 
    pull()
  
  temp_pca_df <- pcall %>% 
    dplyr::filter(year == y) %>% 
    mutate(non_na_assets = apply(.[,temp_pca_vars],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
    dplyr::filter(non_na_assets > 0) %>% 
    dplyr::select(bttid,year, one_of(temp_pca_vars)) %>% 
    arrange(bttid) 
  
  harmonized_index <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS")) %>% 
    dplyr::filter(year == y) %>% 
    dplyr::select(bttid,year, pc,pc2,pc3)  %>% 
    arrange(bttid)
  
  if(y == 1990){
    temp_pca_df <- temp_pca_df %>% 
      dplyr::filter(bttid %in% harmonized_index$bttid)
    
  }
  
  if(y == 2018){
    temp_pca_df <- temp_pca_df %>% 
      dplyr::filter(bttid %in% harmonized_index$bttid)
    
  }
  
  temp_pca_df_unimputed <- temp_pca_df
  
  # source(paste0(path_harmonization_repo,"/south africa/ph_pca_df_imputation.R"))
  
  # Mode impute -----------
  # temp_pca_df[is.na(temp_pca_df)] <- 0
  temp_pca_df <- mode_impute(temp_pca_df,
                        ignore_vars = c("bttid"),
                        grouping_var = "year")
  
  
  
  # Removing zero-variance variables
  zero_var_indices <- caret::nearZeroVar(temp_pca_df)
  names_zero_var <- names(temp_pca_df)[zero_var_indices]
  
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-year,-bttid 
                  # -pc,-pc2,-pc3 - Removing these earlier
                  
    ) %>% 
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  
  temp_pca_vars <- temp_pca_vars[!temp_pca_vars %in% names_zero_var]
  
  temp_pca_vars = names(temp_x)
  
  temp_polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)   
  
  temp_pca_df_unimputed[,c("pc", paste0("pc",c(2,3)))] <- temp_polychoric_out$scores[,1:3]
  
  # colnames(temp_x)[!colnames(temp_x) %in% rownames(temp_polychoric_out$pca_loadings[,])]
  
  # psych::biplot.psych(psych_pca)
  
  # Correlation -------

  r = paste0(
    sprintf(abs(cor(temp_pca_df_unimputed$pc,harmonized_index$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_spearman_cs[row,]$r = r

  
  # Loadings --------
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
  
  pca_cs <- bind_rows(
    pca_cs,
    temp_pca_df_unimputed
  )
  
}



saveRDS(pca_cs,paste0(path_harmonization_folder,"/south africa/working/pca_cs.RDS"))
write_dta(pca_cs,paste0(path_harmonization_folder,"/south africa/working/pca_cs.dta"),version=12)

ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/south africa/working/correlation with cs all items indices.csv"),row.names = FALSE)

# Each row is a PC1 loading
ses_pca_obj %>%
  distinct(year,iteration,item,.keep_all = TRUE) %>% 
  pivot_wider(names_from="item",values_from="PC1") %>%
  write.csv(.,paste0(path_harmonization_folder,"/south africa/working/cs all items indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/south africa/working/cs all items indices proportion of variance.csv"),row.names = FALSE)

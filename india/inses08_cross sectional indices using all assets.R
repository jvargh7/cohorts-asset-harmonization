# source(paste0(path_harmonization_repo,"/india/inses01_recoding variables.R"))
# source(paste0(path_harmonization_repo,"/india/inses02_merge.R"))
# source(paste0(path_harmonization_repo,"/india/inses03_common assets.R"))
source(paste0(path_harmonization_repo,"/india/inses04_harmonized index.R"))


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
pcall <- readRDS(paste0(path_harmonization_folder,"/india/working/pcall.RDS"))

availability_pcall <- pcall %>% 
  dplyr::select(year,starts_with("p_"),starts_with("d_"),c_crowding,starts_with("r_")) %>% 
  group_by(year) %>% 
  summarize_all(.funs=function(x)sum(!is.na(x))) %>% 
  ungroup() %>% 
  pivot_longer(cols=-one_of("year"),names_to="var_name",values_to="obs_count") %>% 
  dplyr::filter(obs_count!=0)





pca_cs <- data.frame()


for (row in 1:nrow(ses_spearman_cs)){
  
  # exclude_vars = c("d_cable","d_dishtv","d_housetype",
  #                  "d_mixergrinder","d_stove","d_washingmachine",
  #                  "p_drinkingwater","p_drinkingwatershared","p_generalwater",
  #                  "p_generalwatershared")
  exclude_vars = c(NULL)
  
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  
  if(y == 1999){
    exclude_vars = c(exclude_vars,"d_cellphone")
  }
  
  if(y == 2016){
    exclude_vars = c(exclude_vars,
                     "d_television",
                     "d_cable","d_dishtv","d_washingmachine",
                     "d_mixergrinder",
                     "p_drinkingwater","p_drinkingwatershared",
                     "p_generalwater","p_generalwatershared"
                     )
  }
  
  temp_pca_vars <-availability_pcall %>%
    dplyr::filter(year == y,!var_name %in% exclude_vars) %>% 
    dplyr::select(var_name) %>% 
    pull()
  
  temp_pca_df <- pcall %>% 
    dplyr::filter(year == y) %>% 
    mutate(non_na_assets = apply(.[,temp_pca_vars],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
    dplyr::filter(non_na_assets > 0) %>% 
    dplyr::select(id,year, one_of(temp_pca_vars)) %>% 
    arrange(id) 
  
  harmonized_index <- readRDS(paste0(path_harmonization_folder,"/india/working/pca_df_unimputed.RDS")) %>% 
    dplyr::filter(year == y) %>% 
    dplyr::select(id,year, pc,pc2,pc3)  %>% 
    arrange(id)
  
  if(y %in% c(2006,2012)){
    # There are 2 individuals for whom variables forcibly excluded from pca_df are available in 2006
    temp_pca_df <- temp_pca_df %>% 
      dplyr::filter(id %in% harmonized_index$id)
    
  }
  
  temp_pca_df_unimputed <- temp_pca_df
  
  # source(paste0(path_harmonization_repo,"/brazil 1993/b9_pca_df_imputation.R"))
  
  # Mode impute -----------
  # temp_pca_df[is.na(temp_pca_df)] <- 0
  temp_pca_df <- mode_impute(temp_pca_df,
                             ignore_vars = c("id"),
                             grouping_var = "year")
  
  # temp_pca_df[is.na(temp_pca_df)] <- 0
  
  # Removing zero-variance variables
  zero_var_indices <- caret::nearZeroVar(temp_pca_df)
  names_zero_var <- names(temp_pca_df)[zero_var_indices]
  
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-year,-id 
                  # -pc,-pc2,-pc3 - Removing these earlier
                  
    ) %>% 
    dplyr::select(-one_of(names_zero_var)) %>% 
    as.matrix()
  
  temp_pca_vars <- temp_pca_vars[!temp_pca_vars %in% names_zero_var]
  
  temp_polychoric_out <- polychoric_pca(temp_x,temp_pca_vars) 
  
  # colnames(temp_x)[!colnames(temp_x) %in% rownames(temp_pca_obj$loadings[,])]
  
  # Correlation -------
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



saveRDS(pca_cs,paste0(path_harmonization_folder,"/india/working/pca_cs.RDS"))
write_dta(pca_cs,paste0(path_harmonization_folder,"/india/working/pca_cs.dta"),version=12)

ses_spearman_cs %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/india/working/correlation with cs all items indices.csv"),row.names = FALSE)

# Each row is a PC1 loading

View(ses_pca_obj %>%
       distinct(year,iteration,item,.keep_all = TRUE) %>% 
       pivot_wider(names_from="year",values_from="PC1"))

ses_pca_obj %>%
  distinct(year,iteration,item,.keep_all = TRUE) %>% 
  pivot_wider(names_from="item",values_from="PC1") %>%
  write.csv(.,paste0(path_harmonization_folder,"/india/working/cs all items indices loadings.csv"),row.names = FALSE)

# Each value is variable importance from dropping none (=all) or one of the variables
ses_pca_imp %>%
  write.csv(.,paste0(path_harmonization_folder,"/india/working/cs all items indices proportion of variance.csv"),row.names = FALSE)

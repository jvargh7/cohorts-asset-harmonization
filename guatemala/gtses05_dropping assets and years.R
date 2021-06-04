source(paste0(path_harmonization_repo,"/guatemala/gtses02_merge.R"))
source(paste0(path_harmonization_repo,"/guatemala/gtses03_common assets.R"))

pca_df <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS")) %>% 
  dplyr::filter(census %in% years) %>% 
  dplyr::select(census,familia,comuni,id_uni,one_of(pca_vars),pc,pc2,pc3)

pca_df_unimputed <- pca_df

# Imputation ------------

source(paste0(path_harmonization_repo,"/guatemala/gt_pca_df_imputation.R"))


# Asset x Year --------

items = c("all",pca_vars)
years <- c(0, years)

ses_pearson <- expand.grid(years,items) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))

for (row in 1:nrow(ses_pearson)){
  y = ses_pearson[row,]$Var1
  i = ses_pearson[row,]$Var2
  
  temp_pca_df = pca_df %>% 
    dplyr::filter(census != y) %>% 
    dplyr::select(-one_of(i))
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-census,-familia,-comuni,-id_uni,-pc,-pc2,-pc3) %>% as.matrix()
  
  temp_pca_vars = colnames(temp_x)
  
  polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)   
  
  temp_pca_df[,c("temp_pc", paste0("temp_pc",c(2,3)))] <- polychoric_out$scores[,1:3]
  
  
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df$temp_pc,temp_pca_df$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_pearson[row,]$r = r
  
}

ses_pearson %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/drop asset x year.csv"),row.names = FALSE)

# Asset x Assets --------

items = c("all",pca_vars)

ses_pearson2 <- expand.grid(items,items) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var1 = as.character(Var1),
         Var2 = as.character(Var2))

for (row in 1:nrow(ses_pearson2)){
  i1 = ses_pearson2[row,]$Var1
  i2 = ses_pearson2[row,]$Var2
  
  temp_pca_df = pca_df %>% 
    # dplyr::filter(year != y) %>% 
    dplyr::select(-one_of(i1),-one_of(i2))
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-census,-familia,-comuni,-id_uni,-pc,-pc2,-pc3) %>% as.matrix()
  
  temp_pca_vars = colnames(temp_x)
  
  polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)      
  
  temp_pca_df[,c("temp_pc", paste0("temp_pc",c(2,3)))] <- polychoric_out$scores[,1:3]
  
  
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df$temp_pc,temp_pca_df$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_pearson2[row,]$r = r
  
}

ses_pearson2 %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/drop asset x asset.csv"),row.names = FALSE)


# Year x Year --------

items = c("all",pca_vars)

ses_pearson3 <- expand.grid(years,years) %>% 
  data.frame() %>% 
  mutate(r = NA)

for (row in 1:nrow(ses_pearson3)){
  y1 = ses_pearson3[row,]$Var1
  y2 = ses_pearson3[row,]$Var2
  
  temp_pca_df = pca_df %>% 
    dplyr::filter(!census %in% c(y1,y2))
  # dplyr::select(-one_of(i1),-one_of(i2))
  
  temp_x = temp_pca_df %>% 
    dplyr::select(-census,-familia,-comuni,-id_uni,-pc,-pc2,-pc3) %>% as.matrix()
  
  temp_pca_vars = colnames(temp_x)
  
  polychoric_out <- polychoric_pca(temp_x,temp_pca_vars)       
  
  temp_pca_df[,c("temp_pc", paste0("temp_pc",c(2,3)))] <- polychoric_out$scores[,1:3]
  
  
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df$temp_pc,temp_pca_df$pc,method = "spearman")),fmt = "%0.2f")
  )
  
  ses_pearson3[row,]$r = r
  
}

ses_pearson3 %>%
  distinct(Var1,Var2,.keep_all = TRUE) %>% 
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_harmonization_folder,"/guatemala/working/drop year x year.csv"),row.names = FALSE)
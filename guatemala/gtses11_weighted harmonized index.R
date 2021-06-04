# source(paste0(path_harmonization_repo,"/guatemala/gtses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses02_merge.R"))
# source(paste0(path_harmonization_repo,"/guatemala/gtses03_common assets.R"))
source(paste0(path_harmonization_repo,"/guatemala/gtses04_harmonized index.R"))

pca_df <- pca_df %>% 
  group_by(census) %>% 
  mutate(w = n()) %>% 
  ungroup() %>% 
  mutate(w = nrow(.)/(length(unique(census))*w))  

x = pca_df %>% 
  dplyr::select(-census,-familia,-comuni,-id_uni,-w) %>% as.matrix()

polychoric_out <- polychoric_pca(x,pca_vars,w = pca_df$w)   
saveRDS(polychoric_out,paste0(path_harmonization_folder,"/guatemala/working/polychoric_out weighted.RDS"))

r = paste0(
  sprintf(abs(cor(polychoric_out$scores[,1],pca_df_unimputed$pc,method = "spearman")),fmt = "%0.2f")
)

# source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/philippines/phses02_merge.R"))
# source(paste0(path_harmonization_repo,"/philippines/phses03_common assets.R"))
source(paste0(path_harmonization_repo,"/philippines/phses04_harmonized index.R"))

pca_df <- pca_df %>% 
  group_by(year) %>% 
  mutate(w = n()) %>% 
  ungroup() %>% 
  mutate(w = nrow(.)/(length(unique(years))*w))

x = pca_df %>% 
  dplyr::select(-year,-uncchdid,-w) %>% as.matrix()

polychoric_out <- polychoric_pca(x,pca_vars,w=pca_df$w)  
saveRDS(polychoric_out,paste0(path_harmonization_folder,"/philippines/working/polychoric_out weighted.RDS"))


r = paste0(
  sprintf(abs(cor(polychoric_out$scores[,1],pca_df_unimputed$pc,method = "spearman")),fmt = "%0.2f")
)


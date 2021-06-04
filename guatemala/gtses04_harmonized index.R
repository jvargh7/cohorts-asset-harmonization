source(paste0(path_harmonization_repo,"/guatemala/gtses02_merge.R"))
source(paste0(path_harmonization_repo,"/guatemala/gtses03_common assets.R"))

pcall %>% 
  dplyr::filter(census %in% years) %>% 
  dplyr::select(census,one_of(pca_vars)) %>% 
  compareGroups::compareGroups(census ~ .,data=.,max.ylev = 10) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()

pca_df <- pcall %>% 
  dplyr::filter(census %in% years) %>% 
  dplyr::select(census,familia,comuni,id_uni,one_of(pca_vars))

pca_df_unimputed <- pca_df

# Imputation ------------

source(paste0(path_harmonization_repo,"/guatemala/gt_pca_df_imputation.R"))


x = pca_df %>% 
  dplyr::select(-census,-familia,-comuni,-id_uni) %>% as.matrix()

polychoric_out <- polychoric_pca(x,pca_vars)   
saveRDS(polychoric_out,paste0(path_harmonization_folder,"/guatemala/working/polychoric_out.RDS"))

pca_df_unimputed[,c("pc", paste0("pc",c(2,3)))] <- polychoric_out$scores[,1:3]

# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
gtpcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/guatemala/GT Variable List.xlsx"),sheet="pcall")

pca_df_unimputed <- label_variables(pca_df_unimputed,cohorts_labels = gtpcall_labels,overwrite_var_label = FALSE)






# SAVE ---------------
saveRDS(pca_df_unimputed,paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS"))
write_dta(pca_df_unimputed,paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.dta"),version=12)

source(paste0(path_harmonization_repo,"/guatemala/gtses04a_census link.R"))







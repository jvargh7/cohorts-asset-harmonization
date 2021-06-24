source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
source(paste0(path_harmonization_repo,"/philippines/phses02_merge.R"))
source(paste0(path_harmonization_repo,"/philippines/phses03_common assets.R"))

pcall %>% 
  dplyr::filter(year %in% years) %>% 
  dplyr::select(year,one_of(pca_vars)) %>% 
  compareGroups::compareGroups(year ~ .,data=.,max.ylev = 10) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()

pca_df <- pcall %>% 
  dplyr::filter(year %in% years) %>% 
  dplyr::select(year,uncchdid,one_of(pca_vars))

pca_df_unimputed <- pca_df  %>% arrange(year,uncchdid)

# Imputation ------------

source(paste0(path_harmonization_repo,"/philippines/ph_pca_df_imputation.R"))

x = pca_df %>% 
  dplyr::select(-year,-uncchdid) %>% as.matrix()

polychoric_out <- polychoric_pca(x,pca_vars)  
saveRDS(polychoric_out,paste0(path_harmonization_folder,"/philippines/working/polychoric_out.RDS"))
# psych::biplot.psych(psych_pca)

# Warning message:
#   In polydi(data[, p, drop = FALSE], data[, d, drop = FALSE], global = global,  :
#               The items do not have an equal number of response alternatives, I am setting global to FALSE


# https://stat.ethz.ch/pipermail/r-help/2014-April/373365.html
# https://stats.stackexchange.com/questions/144841/compute-component-scores-from-principalloadings-directly-in-r/144866#144866



pca_df_unimputed[,c("pc", paste0("pc",c(2,3)))] <- polychoric_out$scores[,1:3]




# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
phpcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/philippines/Philippines dictionary.xlsx"),sheet="pcall")

pca_df_unimputed <- label_variables(pca_df_unimputed,cohorts_labels = phpcall_labels,overwrite_var_label = FALSE)






# SAVE ---------------
saveRDS(pca_df_unimputed,paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS"))
write_dta(pca_df_unimputed,paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.dta"),version=12)



# NOTES -------------
# psych::print.psych(psych_pca)



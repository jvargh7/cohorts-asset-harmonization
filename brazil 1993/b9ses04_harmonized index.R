source(paste0(path_harmonization_repo,"/brazil 1993/b9ses01_recoding variables.R"))
source(paste0(path_harmonization_repo,"/brazil 1993/b9ses02_merge.R"))
source(paste0(path_harmonization_repo,"/brazil 1993/b9ses03_common assets.R"))


pcall %>% 
  dplyr::filter(year %in% years) %>% 
  dplyr::select(year,one_of(pca_vars)) %>% 
  compareGroups::compareGroups(year ~ .,data=.,max.ylev = 10) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()

pca_df <- pcall %>% 
  dplyr::filter(year %in% years) %>% 
  dplyr::select(year,nquest,one_of(pca_vars)) %>% 
  mutate(non_na_assets = apply(.[,pca_vars],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
  dplyr::filter(non_na_assets > 0)

pca_df_unimputed <- pca_df

# Imputation ------------

source(paste0(path_harmonization_repo,"/brazil 1993/b9_pca_df_imputation.R"))


x = pca_df %>% 
  dplyr::select(-year,-nquest,-non_na_assets) %>% as.matrix()

polychoric_out <- polychoric_pca(x,pca_vars)   
# psych::biplot.psych(psych_pca)

saveRDS(polychoric_out,paste0(path_harmonization_folder,"/brazil 1993/working/polychoric_out.RDS"))

# Warning message:
#   In polydi(data[, p, drop = FALSE], data[, d, drop = FALSE], global = global,  :
#               The items do not have an equal number of response alternatives, I am setting global to FALSE


# https://stat.ethz.ch/pipermail/r-help/2014-April/373365.html
# https://stats.stackexchange.com/questions/144841/compute-component-scores-from-principalloadings-directly-in-r/144866#144866







pca_df_unimputed[,c("pc", paste0("pc",c(2,3)))] <- polychoric_out$scores[,1:3]




# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
b9pcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/brazil 1993/B9 Variable List.xlsx"),sheet="pcall")

pca_df_unimputed <- label_variables(pca_df_unimputed,cohorts_labels = b9pcall_labels,overwrite_var_label = FALSE)






# SAVE ---------------
saveRDS(pca_df_unimputed,paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS"))
write_dta(pca_df_unimputed,paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.dta"),version=12)



# NOTES -------------
# psych::print.psych(psych_pca) # Refer  ?principal
# Uniqueness. Uniqueness represents the variance that is 'unique' to the variable and not shared with other variables. It is equal to 1 – communality (variance that is shared with other variables)
# Communalities – This is the proportion of each variable's variance that can be explained by the factors (e.g., the underlying latent continua). It is also noted as h2 and can be defined as the sum of squared factor loadings for the variables.

# In cor.smooth(r) : Matrix was not positive definite, smoothing was done
# https://stackoverflow.com/questions/36867565/interpreting-the-psychcor-smoother-function
# http://personality-project.org/r/psych/help/cor.smooth.html
# "Correlation matrices are said to be improper (or more accurately, not positive semi-definite) 
# when at least one of the eigen values of the matrix is less than 0. 
# This can happen if you have some missing data and are using pair-wise complete correlations. 
# It is particularly likely to happen if you are doing tetrachoric or polychoric correlations based 
# upon data sets with some or even a lot of missing data."
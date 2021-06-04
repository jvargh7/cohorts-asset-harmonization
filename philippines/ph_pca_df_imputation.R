


# Instead of imputing with zero, impute with cross-sectional mode
# pca_df[is.na(pca_df)] <- 0
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

pca_df <- mode_impute(pca_df,
                      ignore_vars = c("uncchdid"),
                      grouping_var = "year")

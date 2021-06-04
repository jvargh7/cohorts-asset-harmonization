
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))
pca_df <- mode_impute(pca_df,
                      ignore_vars = c("familia","comuni","id_uni"),
                      grouping_var = "census")


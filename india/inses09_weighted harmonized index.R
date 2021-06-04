# source(paste0(path_harmonization_repo,"/india/inses01_recoding variables.R"))
# source(paste0(path_harmonization_repo,"/india/inses02_merge.R"))
# source(paste0(path_harmonization_repo,"/india/inses03_common assets.R"))
source(paste0(path_harmonization_repo,"/india/inses04_harmonized index.R"))

pca_df <- pca_df %>% 
  group_by(year) %>% 
  mutate(w = n()) %>% 
  ungroup() %>% 
  mutate(w = nrow(.)/(length(unique(years))*w))

# Imputation ------------


pca_df %>% 
  dplyr::select(year,one_of(pca_vars)) %>% 
  compareGroups::compareGroups(year ~ .,data=.,max.ylev = 10) %>% 
  compareGroups::createTable(show.n = TRUE,show.all=TRUE,digits=1) %>% 
  compareGroups::export2md()


x = pca_df %>% 
  dplyr::select(-year,-id,-non_na_assets,-w) %>% 
  as.matrix()

polychoric_out <- polychoric_pca(x,pca_vars,w = pca_df$w)   

saveRDS(polychoric_out,paste0(path_harmonization_folder,"/india/working/polychoric_out weighted.RDS"))

# Warning messages:
#   1: In cor.smooth(mat) :
#   Matrix was not positive definite, smoothing was done
# 2: In cor.smooth(model) :
#   Matrix was not positive definite, smoothing was done
# 3: In cor.smooth(r) : Matrix was not positive definite, smoothing was done
# 4: In sqrt(1/diag(V)) : NaNs produced
# 5: In cov2cor(t(w) %*% r %*% w) :
#   diag(.) had 0 or NA entries; non-finite result is doubtful

View(polychoric_out$scores)
View(pca_df_unimputed)
r = paste0(
  sprintf(abs(cor(polychoric_out$scores[,1],pca_df_unimputed$pc,method = "spearman")),fmt = "%0.2f")
)

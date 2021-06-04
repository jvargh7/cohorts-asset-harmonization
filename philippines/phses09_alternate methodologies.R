# 
# source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
# source(paste0(path_harmonization_repo,"/philippines/phses02_merge.R"))
# source(paste0(path_harmonization_repo,"/philippines/phses03_common assets.R"))
source(paste0(path_harmonization_repo,"/philippines/phses04_harmonized index.R"))


pca_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS")) %>% 
  dplyr::filter(year %in% years) %>% 
  dplyr::select(year,uncchdid,one_of(pca_vars),pc,pc2,pc3)

# pca_df_unimputed <- pca_df

# Imputation ------------

source(paste0(path_harmonization_repo,"/philippines/ph_pca_df_imputation.R"))

# 1. Factor Analysis + Polychoric --------------
rotation_method = "varimax"
fm_method = "minres"
nfactors = 1

x = pca_df %>% 
  dplyr::select(-year,-uncchdid,-pc,-pc2,-pc3) %>% as.matrix()

mixedcor_obj <- psych::mixedCor(data=x,
                                d = c(pca_vars[regexpr("(d_|r_)",pca_vars)>0]),
                                c = c(pca_vars[regexpr("(c_)",pca_vars)>0]),
                                p = c(pca_vars[regexpr("(p_)",pca_vars)>0]),
                                use="pairwise",
                                method="pearson",
                                correct = 0)

output_wealth_efa <- psych::fa(mixedcor_obj$rho,
                               n.obs = nrow(pca_df),
                               nfactors = nfactors,fm=fm_method,rotate=rotation_method)

output_wealth_efa$scores <- psych::factor.scores(x=apply(x,2,function(y) scale(y)),output_wealth_efa,method="components")   #find the scores from the response data set with the p3 pca solution
# psych::biplot.psych(psych_pca)
r1 <- paste0(
  sprintf(abs(cor(output_wealth_efa$scores$scores[,1],pca_df$pc,method = "spearman")),fmt = "%0.2f")
)

# 2. Factor Analysis + Pearson  ------------
rotation_method = "varimax"
fm_method = "minres"
nfactors = 1


pearson_x <- pca_df %>% 
  dplyr::select(-year,-uncchdid,-pc,-pc2,-pc3) %>% 
  mutate_at(vars(starts_with("p_")),function(x){case_when(x == 2 ~ 1,
                                  x %in% c(0,1) ~ 0)})

output_wealth_efa2 <- psych::fa(pearson_x,
                                n.obs = nrow(pca_df),
                                nfactors = nfactors,fm=fm_method,rotate=rotation_method)



output_wealth_efa2$scores <- psych::factor.scores(x=apply(x,2,function(y) scale(y)),output_wealth_efa2,method="components")   #find the scores from the response data set with the p3 pca solution
# psych::biplot.psych(psych_pca)
r2 <- paste0(
  sprintf(abs(cor(output_wealth_efa2$scores$scores[,1],pca_df$pc,method = "spearman")),fmt = "%0.2f")
)

# 3. PCA + Pearson ---------------
source(paste0(path_incap_repo,"/ses/temp_pca.R"))

output_wealth_pca3 <- pca_df %>% 
  dplyr::select(-year,-uncchdid,-pc,-pc2,-pc3) %>% 
  mutate_at(vars(starts_with("p_")),function(x){case_when(x == 2 ~ 1,
                                                          x %in% c(0,1) ~ 0)}) %>%  
  temp_pca(.,scale_term=TRUE)

r3 <- paste0(
  sprintf(abs(cor(output_wealth_pca3$x[,1],pca_df$pc,method = "spearman")),fmt = "%0.2f")
)

# 4. Multiple Correspondence Analysis --------------
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
library(MASS)
output_wealth_mca4 <- pca_df %>% 
  dplyr::select(-year,-uncchdid,-pc,-pc2,-pc3) %>% 
  mutate(c_crowding = case_when(c_crowding > 0.75 ~ 1,
                                TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("p_")),function(x){case_when(x == 2 ~ 1,
                                                          x %in% c(0,1) ~ 0)}) %>% 
  mutate_all(~as.factor(.)) %>% 
  mca(.)

r4 = paste0(
  # sprintf(abs(cor.test(temp_pca_df$mds,temp_pca_df$pcall)$estimate),fmt = "%0.2f")
  sprintf(abs(cor(output_wealth_mca4$rs[,1],pca_df$pc,method = "spearman")),fmt = "%0.2f")
)

# Fit statistics
# Some items ( d_chicken r_boat r_cattle r_farm ) were negatively correlated with the total scale and 
# probably should be reversed.  
# To do this, run the function again with the 'check.keys=TRUE'
alpha_out <- psych::alpha(check.keys=TRUE,pca_df %>% 
                            dplyr::select(-year,-uncchdid,-pc,-pc2,-pc3))
kmo_out <- psych::KMO(pca_df %>% 
                        dplyr::select(-year,-uncchdid,-pc,-pc2,-pc3))

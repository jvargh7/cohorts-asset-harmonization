polychoric_pca <- function(x_matrix,vars_pca,n_factors=NA,scale_x = TRUE,w=NULL){
  
  mixedcor_obj <- psych::mixedCor(data=x_matrix,
                                  d = c(vars_pca[regexpr("^(d_|r_)",vars_pca)>0]),
                                  c = c(vars_pca[regexpr("^(c_|n_)",vars_pca)>0]),
                                  p = c(vars_pca[regexpr("^(p_)",vars_pca)>0]),
                                  use="pairwise",
                                  method="pearson",
                                  correct = 0,weight = w)
  
  mixedcor_rho <- mixedcor_obj$rho
  pca_obj <- psych::principal(r = mixedcor_rho, 
                                   # n.obs=nrow(temp_pca_df),
                                   nfactors = ifelse(!is.na(n_factors),n_factors,length(vars_pca)),
                                   rotate = "none",scores = TRUE)
  

  
  # factor.scores uses four different ways of estimate factor scores. 
  # In all cases, the factor score estimates are based upon 
  # the data matrix, X, times a weighting matrix, W, which weights the observed variables.
  
  # method="Thurstone" finds the regression based weights: 
  # W = R^{-1} F where R is the correlation matrix and F is the factor loading matrix.
  
  if(scale_x == TRUE){
    pca_obj$scores <- psych::factor.scores(x=apply(x_matrix,2,function(y) scale(y)),rho = mixedcor_rho,f=pca_obj,method = "components")  
  }
  if(scale_x == FALSE){
    pca_obj$scores <- psych::factor.scores(x=x_matrix,rho = mixedcor_rho,f=pca_obj,method = "components")  
  }
  
  pca_obj$scores$eigen_values <- pca_obj$values
  pca_obj$scores$pca_loadings <- pca_obj$loadings
  pca_obj$scores$polychoric_mat <- mixedcor_rho
  pca_obj$scores$fa.scores <- pca_obj$scores$scores
  pca_obj$scores$scores <- apply(pca_obj$scores$scores,2,FUN=function(x) scale(x))
  
  return(pca_obj$scores)
  

  
  
}



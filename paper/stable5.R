
congruence_mat <- function(df,use="complete"){
  years = df$year
  
  loadings_df = df %>% 
    dplyr::select(-iteration)
  
  phi_out <- expand.grid(years,years) %>% 
    mutate(phi = NA,
           na_x = 0,
           na_y = 0) %>% 
    dplyr::filter(Var1 == 0 & Var2!=0)
  
  for(i in 1:nrow(phi_out)){
    
    year1 = phi_out[i,]$Var1
    year2 = phi_out[i,]$Var2
    
    x = loadings_df %>% 
      dplyr::filter(year == year1) %>% 
      dplyr::select(-year) %>% 
      as.matrix(ncol=1) %>% 
      t()
    
    y = loadings_df %>% 
      dplyr::filter(year == year2) %>% 
      dplyr::select(-year) %>% 
      as.matrix(ncol=1)%>% 
      t()
    
    # phi_out[i,"phi"] = psych::factor.congruence(f1,f2,use = "complete")
    
    na_x = 0
    na_y = 0
    
    if(use == "complete"){
      na_x = is.na(x)
      na_y = is.na(y)
      
      x_new = x[!is.na(x)&!is.na(y)]
      y_new = y[!is.na(x)&!is.na(y)]
      
      x = x_new
      y = y_new
    }
    
    nx <- 1
    ny <- 1
    # nx <- dim(x)[2]
    # ny <- dim(y)[2]
    cross <- t(y) %*% x
    sumsx <- sqrt(1/diag(t(x) %*% x))
    sumsy <- sqrt(1/diag(t(y) %*% y))
    result <- matrix(rep(0, nx * ny), ncol = nx)
    result <- round(sumsy * (cross * rep(sumsx, each = ny)), 
                    digits = 2)
    # return(t(result))
    
    phi_out[i,"phi"] = result[[1]]
    phi_out[i,"na_x"] = sum(na_x)
    phi_out[i,"na_y"] = sum(na_y)
    
  }
  
  return(phi_out)
  
  
}


b9_df <- read_csv(paste0(path_harmonization_folder,"/brazil 1993/working/cs indices loadings.csv"))
gt_df <- read_csv(paste0(path_harmonization_folder,"/guatemala/working/cs indices loadings.csv"))
in_df <- read_csv(paste0(path_harmonization_folder,"/india/working/cs indices loadings.csv"))
ph_df <- read_csv(paste0(path_harmonization_folder,"/philippines/working/cs indices loadings.csv"))
sa_df <- read_csv(paste0(path_harmonization_folder,"/south africa/working/cs indices loadings.csv"))

library(psych)
b9_out = congruence_mat(b9_df)
gt_out = congruence_mat(gt_df)
in_out = congruence_mat(in_df)
ph_out = congruence_mat(ph_df)
sa_out = congruence_mat(sa_df)


stable5 <- bind_rows(b9_out %>% mutate(cohort = "B9"),
                    gt_out %>% mutate(cohort = "GT"),
                    in_out %>% mutate(cohort = "IN"),
                    ph_out %>% mutate(cohort = "PH"),
                    sa_out %>% mutate(cohort = "SA"),
)


write.csv(stable5,paste0(path_dissertation,"/aim 1/working/cohorts/stable5.csv"))


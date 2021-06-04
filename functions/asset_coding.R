

binary_asset <- function(var) {
  var_new <- case_when(var == 1 ~ 1, 
            var == 0 ~ 0, 
            TRUE ~ NA_real_)
  
  var_label(var_new) = var_label(var)
  
  return(var_new)
}



count2bin_asset <- function(var,na_vals = NA) {
  var_new <- case_when(var %in% na_vals ~ NA_real_,
                       var >= 1 ~ 1, 
                       var == 0 ~ 0, 
                       TRUE ~ NA_real_)
  
  var_label(var_new) = paste0("Binary - ",var_label(var))
  
  return(var_new)
}

count2count_asset <- function(var,na_vals = NA) {
  
  var_numeric = as.numeric(var)
  var_new <- case_when(var_numeric %in% na_vals ~ NA_real_,
                       var_numeric >= 1 ~ var_numeric, 
                       var_numeric == 0 ~ var_numeric, 
                       TRUE ~ NA_real_)
  
  var_label(var_new) = paste0("Count - ",var_label(var))
  
  return(var_new)
}


poly_asset_num <- function(var,w,m,b){
  
  
  
  var_new <- case_when(var %in% w ~ 0,
                   var %in% m ~ 1,
                   var %in% b ~ 2,
                   TRUE ~ NA_real_)
  var_label(var_new) = var_label(var)
  
   return(var_new)
  
}

poly_asset_bin <- function(var,y,n){
  
  
  
  var_new <- case_when(var %in% y ~ 1,
                       var %in% n ~ 0,
                       TRUE ~ NA_real_)
  
  var_label(var_new) = paste0("Binary - ",var_label(var))
  
  return(var_new)
  
}

poly_asset_cat <- function(var,w,m,b){
  
  var_new <- case_when(var %in% w ~ 0,
                   var %in% m ~ 1,
                   var %in% b ~ 2,
                   TRUE ~ NA_real_)
  var_new <- label_factor(var_new,
                      levels_vec = c(0,1,2),
                      labels_vec = c("0, Worst",
                                     "1, Medium",
                                     "2, Best"))
  
  var_label(var_new) = var_label(var)
  
  return(var_new)
}


multi_asset_binary <- function(df,var_names){
  n_vars = length(var_names)
  
  
  
  var_new = case_when(rowSums(df[,c(var_names)],na.rm=TRUE) >0 ~ 1,
                          rowSums(is.na(df[,c(var_names)])) == n_vars ~ NA_real_,
                          rowSums(df[,c(var_names)],na.rm=TRUE) == 0 ~ 0,
                          TRUE ~ NA_real_)
  var_label(var_new) = paste0(var_names,collapse=" | ")
  
  return(var_new)
  
}



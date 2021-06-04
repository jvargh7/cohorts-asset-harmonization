# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode?page=1&tab=votes#tab-top
Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  
  mode_x = ux[which.max(tabulate(match(x, ux)))]
  mode_x <- case_when(is.na(mode_x)|is.null(mode_x) ~ 0,
                      TRUE ~ mode_x)
  
  return(mode_x)
}

mode_impute <- function(df,ignore_vars=c(""),grouping_var = "year"){
  
  df_ignored <- df %>% 
    dplyr::select(one_of(ignore_vars),starts_with("pc"))
  
  df_imputed <- df %>% 
    dplyr::select(-one_of(ignore_vars),-starts_with("pc")) %>% 
    group_by_at(grouping_var) %>% 
    mutate_at(vars(-group_cols()),
              function(x) {case_when(is.na(x) ~ Mode(x),
                                         TRUE ~ x)}
              ) %>% 
    ungroup()
  
  df_out <- bind_cols(df_ignored,df_imputed)
  
  return(df_out)
  
}
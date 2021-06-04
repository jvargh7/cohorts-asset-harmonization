
pca_df <- pca_df %>% arrange(year,bttid)

if(c(2018) %in% unique(pca_df$year)){
  pca_df2012 <- pca_df %>% 
    dplyr::filter(year == 2012) %>% 
    mutate(radio2012 = d_radio,
           electricity2012 = d_electricity)
  
  pca_df2018 <- pca_df %>% 
    dplyr::filter(year == 2018) %>% 
    left_join(pca_df2012 %>% 
                dplyr::select(bttid,radio2012,electricity2012),
              by=c("bttid")) %>% 
    mutate(d_radio = radio2012,
           d_electricity = electricity2012)
  
  pca_dfREST <- pca_df %>% 
    dplyr::filter(!year %in% c(2012,2018))
  
  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2012 %>% 
      dplyr::select(-radio2012,-electricity2012),
    pca_df2018 %>% 
      dplyr::select(-radio2012,-electricity2012))
  
  
}

rm(pca_dfREST,pca_df2012,pca_df2018)

if(c(2012) %in% unique(pca_df$year)){
  pca_df2006 <- pca_df %>%
    dplyr::filter(year == 2006) %>%
    mutate(toilet2groups2006 = r_toilet2groups,
           water2groups2006 = r_water2groups
           )

  pca_df2012 <- pca_df %>%
    dplyr::filter(year == 2012) %>%
    left_join(pca_df2006 %>%
                dplyr::select(bttid,toilet2groups2006,water2groups2006),
              by=c("bttid")) %>%
    mutate(r_toilet2groups = toilet2groups2006,
           r_water2groups = water2groups2006)

  pca_dfREST <- pca_df %>%
    dplyr::filter(!year %in% c(2006,2012))

  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2012 %>%
      dplyr::select(-toilet2groups2006,-water2groups2006),
    pca_df2006 %>%
      dplyr::select(-toilet2groups2006,-water2groups2006))


}


# if(c(2012) %in% unique(pca_df$year)){
  # pca_df2018 <- pca_df %>%
  #   dplyr::filter(year == 2018) %>%
  #   mutate(toilet2groups2018 = r_toilet2groups,
  #          water2groups2018 = r_water2groups
  #   )
# 
#   pca_df2012 <- pca_df %>%
#     dplyr::filter(year == 2012) %>%
#     left_join(pca_df2018 %>%
#                 dplyr::select(bttid,toilet2groups2018,water2groups2018),
#               by=c("bttid")) %>%
#     mutate(r_toilet2groups = toilet2groups2018,
#            r_water2groups = water2groups2018)
# 
#   pca_dfREST <- pca_df %>%
#     dplyr::filter(!year %in% c(2012,2018))
# 
#   pca_df <- plyr::rbind.fill(
#     pca_dfREST,
#     pca_df2012 %>%
#       dplyr::select(-toilet2groups2018,-water2groups2018),
#     pca_df2018 %>%
#       dplyr::select(-toilet2groups2018,-water2groups2018))
# 
# 
# }



# Instead of imputing with zero, impute with cross-sectional mode
# pca_df[is.na(pca_df)] <- 0
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

pca_df <- mode_impute(pca_df,
                      ignore_vars = c("bttid"),
                      grouping_var = "year")

pca_df <- pca_df %>% arrange(year,bttid)


# ids2006 <- pca_df[pca_df$year==2006,]$bttid
# ids2012 <- pca_df[pca_df$year==2012,]$bttid
# ids2018 <- pca_df[pca_df$year==2018,]$bttid
# table(ids2006 %in% ids2012)
#  
# table(ids2006 %in% ids2012,useNA="always")
# 
# # TRUE <NA> 
# #   1511    0 
# table(ids2012 %in% ids2006,useNA="always")
# # 
# # FALSE  TRUE  <NA> 
# #   125  1511     0 
# table(ids2012 %in% ids2018,useNA="always")
# # 
# # FALSE  TRUE  <NA> 
# #   362  1274     0 
# table(pca_df_unimputed$r_toilet2groups,pca_df_unimputed$year,useNA="always")
# # 
# # 1990 1997 2002 2006 2012 2018 <NA>
# #   0     887  827  657  700    0  366    0
# # 1     351  518  632  810    0 1014    0
# # <NA>  220    0  153    1 1636   14    0

# # Under imputation with 2018
# table(pca_df$r_toilet2groups,pca_df$year,useNA="always")
# # 
# # 1990 1997 2002 2006 2012 2018 <NA>
# #   0    1107  827  810  700  340  366    0
# # 1     351  518  632  811 1296 1028    0
# # <NA>    0    0    0    0    0    0    0

# # Under imputation with 2006
# table(pca_df$r_toilet2groups,pca_df$year,useNA="always")
# 1990 1997 2002 2006 2012 2018 <NA>
#   0    1107  827  810  700  700  366    0
# 1     351  518  632  811  936 1028    0
# <NA>    0    0    0    0    0    0    0











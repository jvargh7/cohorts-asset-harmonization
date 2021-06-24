



if(c(2009) %in% unique(pca_df$year)){
  pca_df2005 <- pca_df %>%
    dplyr::filter(year == 2005) %>%
    mutate(chicken2005 = d_chicken,
           crowding2005 = c_crowding,
           livinset2005 = d_livinset,
           beds2005 = r_beds,
           cattle2005 = r_cattle,
           farm2005 = r_farm
           )

  pca_df2009 <- pca_df %>%
    dplyr::filter(year == 2009) %>%
    left_join(pca_df2005 %>%
                dplyr::select(uncchdid,
                              chicken2005,crowding2005,
                              livinset2005,beds2005,
                              cattle2005,farm2005),
              by=c("uncchdid")) %>%
    mutate(d_chicken = chicken2005,
           c_crowding = crowding2005,
           d_livinset = livinset2005,
           r_beds = beds2005,
           r_cattle = cattle2005,
           r_farm = farm2005)

  pca_dfREST <- pca_df %>%
    dplyr::filter(!year %in% c(2005,2009))

  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2005 %>%
      dplyr::select(-chicken2005,-crowding2005,
                    -livinset2005,-beds2005,
                    -cattle2005,-farm2005),
    pca_df2009 %>%
      dplyr::select(-chicken2005,-crowding2005,
                    -livinset2005,-beds2005,
                    -cattle2005,-farm2005))


}


if(c(2018) %in% unique(pca_df$year)){
  pca_df2009 <- pca_df %>%
    dplyr::filter(year == 2009) %>%
    mutate(otherappl2009 = d_otherappl
    )

  pca_df2018 <- pca_df %>%
    dplyr::filter(year == 2018) %>%
    left_join(pca_df2009 %>%
                dplyr::select(uncchdid,
                              otherappl2009),
              by=c("uncchdid")) %>%
    mutate(d_otherappl = otherappl2009)

  pca_dfREST <- pca_df %>%
    dplyr::filter(!year %in% c(2009,2018))

  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2009 %>%
      dplyr::select(-otherappl2009),
    pca_df2018 %>%
      dplyr::select(-otherappl2009))


}


# Instead of imputing with zero, impute with cross-sectional mode
# pca_df[is.na(pca_df)] <- 0
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

pca_df <- mode_impute(pca_df,
                      ignore_vars = c("uncchdid"),
                      grouping_var = "year")

pca_df <- pca_df %>% arrange(year,uncchdid)
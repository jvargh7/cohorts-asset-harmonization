


# pca_df[is.na(pca_df)] <- 0

if(c(1999) %in% unique(pca_df$year)){
  
  pca_df <- pca_df %>% 
    mutate(d_cellphone = case_when(year == 1999 ~ 0,
                                   TRUE ~ d_cellphone))
  
}


if(c(2016) %in% unique(pca_df$year)){
  pca_df2012 <- pca_df %>%
    dplyr::filter(year == 2012) %>%
    mutate(cable2012 = d_cable,
           dishtv2012 = d_dishtv,
            washingmachine2012 = d_washingmachine
            )

  pca_df2016 <- pca_df %>%
    dplyr::filter(year == 2016) %>%
    left_join(pca_df2012 %>%
                dplyr::select(id,
                              cable2012,dishtv2012,
                               washingmachine2012
                               ),
              by=c("id")) %>%
    mutate(d_cable = cable2012,
           d_dishtv = dishtv2012,
           d_washingmachine = washingmachine2012
     )

  pca_dfREST <- pca_df %>%
    dplyr::filter(!year %in% c(2012,2016))

  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2012 %>%
      dplyr::select(-cable2012,-dishtv2012,
                    -washingmachine2012
                     ),
    pca_df2016 %>%
      dplyr::select(-cable2012,-dishtv2012,
                    -washingmachine2012
                     ))


}


if(c(2016) %in% unique(pca_df$year)){
  pca_df2012 <- pca_df %>%
    dplyr::filter(year == 2012) %>%
    mutate(
           # housetype2012 = d_housetype,
           mixergrinder2012 = d_mixergrinder,
           # stove2012 = d_stove,
           drinkingwater2012 = p_drinkingwater,
           drinkingwatershared2012 = p_drinkingwatershared,
           generalwater2012 = p_generalwater,
           generalwatershared2012 = p_generalwatershared
    )
  
  pca_df2016 <- pca_df %>%
    dplyr::filter(year == 2016) %>%
    left_join(pca_df2012 %>%
                dplyr::select(id,
                              # housetype2012,
                              mixergrinder2012,
                              # stove2012,
                              drinkingwater2012,drinkingwatershared2012,
                              generalwater2012,generalwatershared2012
                ),
              by=c("id")) %>%
    mutate(
           # d_housetype = housetype2012,
           d_mixergrinder = mixergrinder2012,
           # d_stove = stove2012,
           p_drinkingwater = drinkingwater2012,
           p_drinkingwatershared = drinkingwatershared2012,
           p_generalwater = generalwater2012,
           p_generalwatershared = generalwatershared2012
    )
  
  pca_dfREST <- pca_df %>%
    dplyr::filter(!year %in% c(2012,2016))
  
  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2012 %>%
      dplyr::select(
                    # -housetype2012,
                    -mixergrinder2012,
                    # -stove2012,
                    -drinkingwater2012,-drinkingwatershared2012,
                    -generalwater2012,-generalwatershared2012
      ),
    pca_df2016 %>%
      dplyr::select(
                    # -housetype2012,
                    -mixergrinder2012,
                    # -stove2012,
                    -drinkingwater2012,-drinkingwatershared2012,
                    -generalwater2012,-generalwatershared2012
      ))
  
  
}



source(paste0(path_harmonization_repo,"/package/mode_impute.R"))
pca_df <- mode_impute(pca_df,
                      ignore_vars = c("id"),
                      grouping_var = "year")

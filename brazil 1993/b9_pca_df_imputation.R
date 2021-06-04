

pca_df <- pca_df %>% arrange(year,nquest)



if(c(2011) %in% unique(pca_df$year)){
  pca_df2008 <- pca_df %>% 
    dplyr::filter(year == 2008) %>% 
    mutate(pipedwater2008 = p_pipedwater)
  
  pca_df2011 <- pca_df %>% 
    dplyr::filter(year == 2011) %>% 
    left_join(pca_df2008 %>% 
                dplyr::select(nquest,pipedwater2008),
              by=c("nquest")) %>% 
    mutate(p_pipedwater = pipedwater2008)
  
  pca_dfREST <- pca_df %>% 
    dplyr::filter(!year %in% c(2008,2011))
  
  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2008 %>% 
      dplyr::select(-pipedwater2008),
    pca_df2011 %>% 
      dplyr::select(-pipedwater2008))

  
}

if(c(2015) %in% unique(pca_df$year)){
  
  pca_df2011 <- pca_df %>% 
    dplyr::filter(year == 2011) %>% 
    mutate(vaccumcleaner2011 = d_vaccumcleaner)
  
  pca_df2015 <- pca_df %>% 
    dplyr::filter(year == 2015) %>% 
    # mutate(vaccumcleaner2015 = d_vaccumcleaner) %>% 
    left_join(pca_df2011 %>% 
                dplyr::select(nquest,vaccumcleaner2011),
              by=c("nquest")) %>% 
    mutate(d_vaccumcleaner = vaccumcleaner2011)
  
  pca_dfREST <- pca_df %>% 
    dplyr::filter(!year %in% c(2011,2015))
  
  pca_df <- plyr::rbind.fill(
    pca_dfREST,
    pca_df2011 %>% 
      dplyr::select(-vaccumcleaner2011),
    pca_df2015 %>% 
      dplyr::select(-vaccumcleaner2011) )
    
 
  
}

# Instead of imputing with zero, impute with cross-sectional mode
# pca_df[is.na(pca_df)] <- 0
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

pca_df <- mode_impute(pca_df,
                      ignore_vars = c("nquest"),
                      grouping_var = "year")

pca_df <- pca_df %>% arrange(year,nquest)
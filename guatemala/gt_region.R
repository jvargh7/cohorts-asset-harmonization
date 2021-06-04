
# incap/structural/classify_urban_rural.R ------------
# 0: Urban, 1: Rural, 2: Other country --> 1: Urban, 2: Rural, NA: Otherwise


gt_region <- readRDS(paste0(path_incap_ses_dfa,"/urbano_rural/urbano_rural2015-18 before request.RDS")) %>% 
   
  mutate(urbano_rural2018 = case_when(
                                      urbano_rural2018 == 1 ~ 2,
                                      urbano_rural2018 == 0 ~ 1,
                                      TRUE ~ NA_real_)) %>% 
  left_join(    
    bind_rows(read_csv(paste0(path_incap_ses_dfa,"/urbano_rural/requiring_urbano_rural2015_MARVIN.csv")) %>% 
                            dplyr::select(iduni,urbano_rural2018) %>% 
                            rename(urbano_rural2015 = urbano_rural2018),
                          readxl::read_excel(paste0(path_incap_ses_dfa,"/urbano_rural/requiring urbano_rural2015 request 2.xlsx")) %>% 
                            dplyr::select(iduni,urbano_rural2015)
  ),
            by = "iduni") %>% 
  
  
  mutate(urbano_rural2015 = case_when(lugarres2015 == 5 ~ NA_real_, 
                                      urbano_rural2015 == "Rural" ~ 2,
                                      urbano_rural2015 == "Urbano" ~ 1,
                                      lugarres2015 == 0 ~ 2,
                                      is.na(urbano_rural2015) & !is.na(lugarres2015) ~ urbano_rural2018 %>% as.numeric(),
                                      TRUE ~ NA_real_))


# with(gt_region,table(urbano_rural2015,urbano_rural2018,useNA="always"))
                    # urbano_rural2018
# urbano_rural2015    1   2 <NA>
            #   1    263   0   52
            #   2      5 771   70
            # <NA>    72 157    4


try({
  ur_df <- bind_rows(pca_df %>% 
                       dplyr::filter(census == 2016) %>% 
                       left_join(gt_region %>% 
                                   dplyr::select(iduni,urbano_rural2015) %>% 
                                   rename(ur = urbano_rural2015),
                                 by = c("id_uni" = "iduni")),
                     pca_df %>% 
                       dplyr::filter(census == 2018) %>% 
                       full_join(gt_region %>% 
                                   
                                   dplyr::select(iduni,urbano_rural2018) %>% 
                                   mutate(urbano_rural2018 = as.numeric(urbano_rural2018)) %>% 
                                   rename(ur = urbano_rural2018),
                                 by = c("id_uni" = "iduni"))) %>% 
    dplyr::filter(!is.na(census))
})


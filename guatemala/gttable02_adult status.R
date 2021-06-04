source(paste0(path_harmonization_repo,"/guatemala/gt_region.R"))


table2_df <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "guatemala") %>% 
  dplyr::mutate(id_uni = pin - 20000000) %>% 
  dplyr::select(id_uni,
                chsex,
                ademployment,
                # adschooling,
                gtadeduyr1618,
                adrelstat,
                
                gtadbmi2016,
                gtadravenstotscore2018,
                gtadsrq2018,
                gtadhappy2018) %>% 
  dplyr::filter(!is.na(gtadbmi2016)|!is.na(gtadravenstotscore2018)|!is.na(gtadsrq2018)|!is.na(gtadhappy2018)) %>% 
  left_join(gt_region %>% 
              dplyr::select(iduni,urbano_rural2015,urbano_rural2018),
            by = c("id_uni"="iduni")) %>% 
  dplyr::filter(!is.na(urbano_rural2018)|!is.na(urbano_rural2015)) %>% 
  mutate_at(vars(urbano_rural2015,urbano_rural2018),function(x) factor(x,labels=c("Urban","Rural")))

var_label(table2_df$urbano_rural2015) <- "Residence status in 2015-16"
var_label(table2_df$urbano_rural2018) <- "Residence status in 2017-18"
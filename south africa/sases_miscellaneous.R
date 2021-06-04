# using2018_pca_df_u <- pca_df_unimputed
# using2018_pca_df_i <- pca_df
using2006_pca_df_u <- pca_df_unimputed
using2006_pca_df_i <- pca_df


plot(using2018_pca_df_u$pc,using2006_pca_df_u$pc)

check_2006_vs_2018 <- bind_cols(using2006_pca_df_u %>% 
                                  arrange(bttid,year) %>% 
                                  dplyr::select(pc,year,bttid,d_radio,d_electricity,r_toilet2groups,r_water2groups) %>% 
                                  rename_all(~paste0(.,"_06")),
                                using2006_pca_df_i %>% 
                                  arrange(bttid,year) %>% 
                                  dplyr::select(d_radio,d_electricity,r_toilet2groups,r_water2groups) %>% 
                                  rename_all(~paste0(.,"_06i")),
                                using2018_pca_df_u %>% 
                                  arrange(bttid,year) %>% 
                                  dplyr::select(pc,year,bttid) %>% 
                                  rename_all(~paste0(.,"_18")),
                                using2018_pca_df_i %>% 
                                  arrange(bttid,year) %>% 
                                  dplyr::select(d_radio,d_electricity,r_toilet2groups,r_water2groups) %>% 
                                  rename_all(~paste0(.,"_18i")))
View(check_2006_vs_2018)

with(check_2006_vs_2018,table(r_toilet2groups_06,r_toilet2groups_06i,year_18,useNA = "always"))
with(check_2006_vs_2018,table(r_toilet2groups_06,r_toilet2groups_18i,year_18,useNA = "always"))
with(check_2006_vs_2018,table(r_toilet2groups_06i,r_toilet2groups_18i,year_18,useNA = "always"))
with(check_2006_vs_2018,table(r_water2groups_06i,r_water2groups_18i,year_18,useNA = "always"))


with(check_2006_vs_2018,table(d_radio_06,d_radio_06i,year_18,useNA = "always"))
with(check_2006_vs_2018,table(d_radio_06i,d_radio_18i,year_18,useNA = "always"))

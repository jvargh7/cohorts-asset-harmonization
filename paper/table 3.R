b9_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean_SD = paste0(round_d(mean(pc))," +/- ",round_d(sd(pc))),
            
            Median_IQR = paste0(round_d(median(pc))," [",round_d(quantile(pc,0.25)),", ",round_d(quantile(pc,0.75)),"]"),
            
            Min_Max = paste0("(",round_d(min(pc)),", ",round_d(max(pc)),")"),
            
            Mean = mean(pc)
            
  )

in_df <- readRDS(paste0(path_harmonization_folder,"/india/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean_SD = paste0(round_d(mean(pc))," +/- ",round_d(sd(pc))),
            
            Median_IQR = paste0(round_d(median(pc))," [",round_d(quantile(pc,0.25)),", ",round_d(quantile(pc,0.75)),"]"),
            
            Min_Max = paste0("(",round_d(min(pc)),", ",round_d(max(pc)),")"),
            
            Mean = mean(pc)
            
  )

gt_df <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/pca_df_unimputed.RDS")) %>% 
  rename(year = census) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean_SD = paste0(round_d(mean(pc))," +/- ",round_d(sd(pc))),
            
            Median_IQR = paste0(round_d(median(pc))," [",round_d(quantile(pc,0.25)),", ",round_d(quantile(pc,0.75)),"]"),
            
            Min_Max = paste0("(",round_d(min(pc)),", ",round_d(max(pc)),")"),
            
            Mean = mean(pc)
            
  )

ph_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean_SD = paste0(round_d(mean(pc))," +/- ",round_d(sd(pc))),
            
            Median_IQR = paste0(round_d(median(pc))," [",round_d(quantile(pc,0.25)),", ",round_d(quantile(pc,0.75)),"]"),
            
            Min_Max = paste0("(",round_d(min(pc)),", ",round_d(max(pc)),")"),
            
            Mean = mean(pc)
            
  )

sa_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean_SD = paste0(round_d(mean(pc))," +/- ",round_d(sd(pc))),
            
            Median_IQR = paste0(round_d(median(pc))," [",round_d(quantile(pc,0.25)),", ",round_d(quantile(pc,0.75)),"]"),
            
            Min_Max = paste0("(",round_d(min(pc)),", ",round_d(max(pc)),")"),
            
            Mean = mean(pc)
            
  )



table2 <- bind_rows(b9_df %>% mutate(cohort = "B9"),
                    gt_df %>% mutate(cohort = "GT"),
                    in_df %>% mutate(cohort = "IN"),
                    ph_df %>% mutate(cohort = "PH"),
                    sa_df %>% mutate(cohort = "SA"),
                    )


write.csv(table2,paste0(path_dissertation,"/aim 1/working/cohorts/table 3.csv"))



table2 %>% 
  group_by(cohort) %>% 
  do(coef = coef(lm(Mean ~ year,data=.))[2]) %>% 
  View()

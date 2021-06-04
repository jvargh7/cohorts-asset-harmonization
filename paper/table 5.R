

b9_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/brazil 1993/absolute_df.RDS"))  %>% 
  mutate(id = nquest,
         pc_child = pc1997,
         pc_adult = pc2015) %>% 
  dplyr::select(id, pc_child,pc_adult,moscho,eduyr)

gt_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/guatemala/absolute_df.RDS"))  %>% 
  mutate(id = id_uni,
         pc_child = pcall6775_1,
         pc_adult = pcall1618_1,
         moscho = moscho_sib) %>% 
  dplyr::select(id, pc_child,pc_adult,moscho,eduyr)

in_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/india/absolute_df.RDS"))  %>% 
  mutate(id = id,
         # pc_child = pc1997,
         pc_adult = case_when(!is.na(pc2012) & !is.na(pc2016) ~ pc2016,
                              is.na(pc2012) & !is.na(pc2016) ~ pc2016,
                              is.na(pc2016) & !is.na(pc2012) ~ pc2012,
                              TRUE ~ NA_real_)) %>% 
  dplyr::select(id, pc_adult,eduyr)

ph_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/philippines/absolute_df.RDS"))  %>% 
  mutate(id = uncchdid,
         pc_child = pc1983,
         pc_adult = pc2018) %>% 
  dplyr::select(id, pc_child,pc_adult,moscho,eduyr)

sa_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/south africa/absolute_df.RDS"))  %>% 
  mutate(id = bttid,
         pc_child = pc1990,
         pc_adult = pc2018) %>% 
  dplyr::select(id, pc_child,pc_adult,moscho,eduyr)


table4 <- bind_rows(b9_df %>% mutate(cohort = "B9"),
                     gt_df %>% mutate(cohort = "GT"),
                     in_df %>% mutate(cohort = "IN"),
                     ph_df %>% mutate(cohort = "PH"),
                     sa_df %>% mutate(cohort = "SA"),
) %>% 
  group_by(cohort) %>% 
  summarize(r_child = tryCatch({cor(pc_child,moscho,use="complete.obs")},error=function(e) NA),
            r_adult = tryCatch({cor(pc_adult,eduyr,use="complete.obs")},error=function(e) NA),
            nobs_child = sum(!is.na(pc_child)&!is.na(moscho)),
            nobs_adult = sum(!is.na(pc_adult)&!is.na(eduyr))
            )
write.csv(table4,paste0(path_dissertation,"/aim 1/working/cohorts/table5.csv"))

 
# Brazil 1993 --------
b9_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
          Mean = mean(pc)) %>% 
  mutate(age = year - 1993)

# Guatemala --------
gt_df <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/ses_masters for COHORTS.RDS")) %>% 
  dplyr::select(id_uni,pcall1967_1,pcall1975_1,pcall1987_1,pcall1996_1,pcall2002_1,pcall2016_1,pcall2018_1) %>% 
  left_join(early_life <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/early_life.RDS")) %>% 
              dplyr::select(id_uni,gtchbyear),
            by = "id_uni") %>% 
  mutate(pcall6775_1 = case_when(gtchbyear < 71 ~ pcall1967_1,
                                 gtchbyear >= 71 ~ pcall1975_1,
                                 TRUE ~ NA_real_)) %>% 
  dplyr::select(-pcall1967_1,-pcall1975_1) %>% 
  pivot_longer(cols=-one_of("id_uni","gtchbyear"),names_to="var",values_to="pc") %>% 
  mutate(year = case_when(var == "pcall6775_1" & gtchbyear < 71 ~ 1967,
                          var == "pcall6775_1" & gtchbyear >= 71 ~ 1975,
                          TRUE ~ str_replace(var,"pcall","") %>% str_replace(.,"_1","") %>% as.numeric(.)),
         gtchbyear = 1900 + gtchbyear) %>% 
  dplyr::select(-var) %>% 
  mutate(age = year - gtchbyear) %>% 
  group_by(age) %>% 
  summarize(n = n(),
            Mean = mean(pc,na.rm=TRUE))

# India -----------  
in_df <- haven::read_sav(paste0(path_harmonization_folder,"/india/data/APH1 asset variables.sav")) %>% 
  dplyr::select(ORIGINALST,DATEEXAMIN) %>%
  mutate(DATEEXAMIN = lubridate::ymd(DATEEXAMIN)) %>% 
  dplyr::rename(id = ORIGINALST,
                y1999 = DATEEXAMIN) %>% 
  mutate(y2006 = as.Date("2007-06-01")) %>% 
  left_join(haven::read_sav(paste0(path_harmonization_folder,"/india/data/NDBC APH 3 & 4 variables.sav")) %>% 
              dplyr::select(Id,O3doe,doe) %>% 
              mutate_at(vars(O3doe,doe),~lubridate::ymd(.)) %>% 
              rename(y2012 = O3doe,
                     y2016 = doe),
            by = c("id"="Id")) %>% 
  pivot_longer(cols=-one_of("id"),names_to = "year",values_to="doe") %>% 
  mutate(year = str_replace(year,"y","") %>% as.numeric(.)) %>% 
  
  left_join(readRDS(paste0(path_harmonization_folder,"/india/working/pca_df_unimputed.RDS")) %>% 
              dplyr::select(year,id,pc),
            by=c("year","id")) %>% 
  
  left_join(readRDS(paste0(path_harmonization_folder,"/india/working/early_life.RDS")),
            by = "id") %>% 
  mutate(doby = case_when(is.na(doby) ~ 1971,
                          TRUE ~ doby)) %>% 
  
  mutate(age = year(doe) - doby) %>% 
  
  group_by(age) %>% 
  summarize(n = n(),
            Mean = mean(pc,na.rm=TRUE)) %>% 
  dplyr::filter(!is.na(Mean),!is.na(age))

# Philippines ----------

ph_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean = mean(pc))  %>% 
  mutate(age = year - 1983)

# South Africa -------------
sa_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS")) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            Mean = mean(pc))  %>% 
  mutate(age = year - 1990)


# Fig 2 ------------

bind_rows(b9_df %>% 
            mutate(country = "Brazil"),
          gt_df %>% 
            mutate(country = "Guatemala"),
          in_df %>% 
            mutate(country = "India"),
          ph_df %>% 
            mutate(country = "Philippines"),
          sa_df %>% 
            mutate(country = "South Africa")) %>% 
  dplyr::select(age,n,Mean,country) %>% 
  dplyr::filter(n > 30) %>% 
  ggplot(data=.,aes(x = age,y=Mean,shape=country)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  ylab("Mean of harmonied asset index (z-scores)") +
  xlab("Age (in years)") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12)) +
  scale_shape_discrete("")

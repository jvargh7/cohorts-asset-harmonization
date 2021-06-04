# Reading data ------------
ses_masters <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/ses_masters for COHORTS.RDS"))
b9_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS"))
in_df <- readRDS(paste0(path_harmonization_folder,"/india/working/pca_df_unimputed.RDS"))
ph_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS"))

sa_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS"))


b9_fig1 <- b9_df %>% 
  ggplot(data=.,aes(x=year,y=pc)) +
  geom_line(aes(group=nquest), col="grey") +
  # geom_smooth(method="lm",col="darkblue") +
  theme_bw(base_size=12) +
  scale_x_continuous(limits=c(1967,2020)) + 
  scale_y_continuous(limits=c(-4,4)) +
  labs(x="Year",y="Asset index (z-scores)") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14)) +
  ggtitle("A. Pelotas 1993 (Brazil)") +
  geom_vline(aes(xintercept = 1993),col="black",linetype=2)

in_fig1 <- in_df %>% 
  ggplot(data=.,aes(x=year,y=pc)) +
  geom_line(aes(group=id), col="grey") +
  # geom_smooth(method="lm",col="darkblue") +
  theme_bw(base_size=12) +
  scale_x_continuous(limits=c(1967,2020)) + 
  scale_y_continuous(limits=c(-4,4)) +
  labs(x="Year",y="Asset index (z-scores)") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14)) +
  ggtitle("C. NDBC (India)")  +
  geom_vline(aes(xintercept = c(1969)),col="black",linetype=2) +
  geom_vline(aes(xintercept = c(1972)),col="black",linetype=2)


ph_fig1 <- ph_df %>% 
  ggplot(data=.,aes(x=year,y=pc)) +
  geom_line(aes(group=uncchdid), col="grey") +
  # geom_smooth(method="lm",col="darkblue") +
  theme_bw(base_size=12) +
  scale_x_continuous(limits=c(1967,2020)) +
  scale_y_continuous(limits=c(-4,4)) +
  labs(x="Year",y="Asset index (z-scores)") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14)) +
  ggtitle("D. CLHNS (Philippines)")  +
  geom_vline(aes(xintercept = c(1983)),col="black",linetype=2)


gt_fig1 <- ses_masters %>% 
  dplyr::select(id_uni,ends_with("_1"),-pcall6775_1) %>% 
  # dplyr::filter(id_uni %in% alive2018) %>% 
  rename_at(vars(ends_with("_1")),.funs=function(x) str_replace(string = x,"_1","")) %>% 
  pivot_longer(cols = starts_with("pcall"),names_to = "year",values_to = "pcall") %>% 
  dplyr::mutate(year = str_replace(year,"pcall","") %>% as.numeric()) %>% 
  dplyr::filter(!is.na(pcall)) %>%
  
  ggplot(data=.,aes(x=year,y=pcall)) +
  geom_path(aes(group=id_uni),col="grey",alpha=0.2) +
  geom_point(col="grey") +
  # geom_smooth(method="loess",col="blue") +
  theme_bw()  +
  scale_x_continuous(limits=c(1967,2020)) +
  scale_y_continuous(limits=c(-4,4)) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14)) +
  ylab("Asset index (z-scores)") +
  xlab("Year") +
  ggtitle("B. INCAP (Guatemala)")   +
  geom_vline(aes(xintercept = c(1969)),col="black",linetype=2) +
  geom_vline(aes(xintercept = c(1977)),col="black",linetype=2)

sa_fig1 <- sa_df %>% 
  ggplot(data=.,aes(x=year,y=pc)) +
  geom_line(aes(group=bttid), col="grey") +
  # geom_smooth(method="lm",col="darkblue") +
  theme_bw(base_size=12) +
  scale_x_continuous(limits=c(1967,2020)) +
  scale_y_continuous(limits=c(-4,4)) +
  labs(x="Year",y="Asset index (z-scores)") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))  +
  ggtitle("E. Bto20+ (South Africa)")    +
  geom_vline(aes(xintercept = c(1990)),col="black",linetype=2)

library(ggpubr)

ggarrange(
  b9_fig1,
  gt_fig1,
  in_fig1,
  ph_fig1,
  sa_fig1,
  nrow=2,ncol = 3)

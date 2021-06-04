
relative_change <- function(x,y,id=NULL){
  if(is.null(id)){
    beta = coef(lm(log(y)~x))[2]
    
    
  }
  
  if(!is.null(id)){
    beta = coef(gee::gee(y~x,id = id,corstr = "unstructured"))[2]
    
  }
  change = exp(beta)-1
  return(change)
  
  
}

# Asset index data -----------

b9_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS"))
gt_df <- readRDS(paste0(path_incap_ses_dfa,"/ses_masters w96.RDS")) %>% 
  dplyr::select(id_uni,pcall6775_1,pcall1987_1,pcall1996_1,pcall2002_1,pcall2016_1,pcall2018_1) %>%
  rename(y1971 = pcall6775_1,
         y1987 = pcall1987_1,
         y1996 = pcall1996_1,
         y2002 = pcall2002_1,
         y2016 = pcall2016_1,
         y2018 = pcall2018_1) %>% 
  pivot_longer(cols=starts_with("y"),names_to="year",values_to="pc") %>% 
  mutate(year = str_replace(year,"y","") %>% as.numeric(.))
# in_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS"))
ph_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS"))
sa_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS"))

# Relative change in asset index ----------
rc_asset <- list()

rc_asset["BRA"] <- relative_change(b9_df$year,b9_df$pc,b9_df$nquest)
rc_asset["GTM"] <- relative_change(gt_df$year,gt_df$pc,gt_df$id_uni)
rc_asset["IND"] <- NA
rc_asset["PHL"] <- relative_change(ph_df$year,ph_df$pc,ph_df$uncchdid)
rc_asset["ZAF"] <- relative_change(sa_df$year,sa_df$pc,sa_df$bttid)





# GDP -------------
wb_country_codes = c("BRA","GTM","IND","PHL","ZAF")

wb_wdi_gdppc <- read_csv(paste0(path_dissertation,"/aim 1/working/cohorts/5ae05ea9-fc3d-460c-a6b6-f562c1de7fa6_Data.csv")) %>% 
  janitor::clean_names(.) %>% 
  dplyr::filter(country_code %in% wb_country_codes) %>% 
  dplyr::filter(series_code == "NY.GDP.PCAP.KD") %>% 
  pivot_longer(cols=matches("x[0-9]+"),names_to="year",values_to="gdp") %>% 
  mutate(year = str_extract(year,"x([0-9])+")) %>% 
  mutate(year = str_replace(year,"x","") %>% as.numeric(.),
         gdp = as.numeric(gdp)) %>% 
  mutate(year = case_when(country_code == "BRA" & year < 1993 ~ NA_real_,
                          country_code == "GTM" & year < 1971 ~ NA_real_,
                          country_code == "IND" & year < 1998 ~ NA_real_,
                          country_code == "PHL" & year < 1983 ~ NA_real_,
                          country_code == "ZAF" & year < 1990 ~ NA_real_,
                          TRUE ~ year)) %>% 
  dplyr::filter(complete.cases(.))






# Relative change in GDP ---------
rc_gdppc = list()

for(c in wb_country_codes){
  
  x = wb_wdi_gdppc[wb_wdi_gdppc$country_code==c,]$year
  y = wb_wdi_gdppc[wb_wdi_gdppc$country_code==c,]$gdp
  
  rc_gdppc[c] <- relative_change(x,y)
  
}

# sfigure 2 ---------

sfigure2 <- data.frame(
  country_code = wb_country_codes,
  gdp = (rc_gdppc %>% unlist())*100,
  asset = (rc_asset %>% unlist())*100
  
) %>% 
  ggplot(data=.,aes(x=gdp,y=asset,label=country_code)) +
  geom_text()+
  theme_bw() +
  xlab("Relative change (%) in GDP per capita (2010$)") +
  ylab("Relative change (%) in Asset index (Birth to latest wave)") +
  scale_x_continuous(limits=c(0,5))+
  scale_y_continuous(limits=c(0,13))


sfigure2



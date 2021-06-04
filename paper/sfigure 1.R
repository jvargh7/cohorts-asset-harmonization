
wb_wdi_gdppc <- read_csv(paste0(path_dissertation,"/aim 1/working/cohorts/5ae05ea9-fc3d-460c-a6b6-f562c1de7fa6_Data.csv")) %>% 
  janitor::clean_names(.) %>% 
  dplyr::filter(country_code %in% c("BRA","GTM","IND","PHL","ZAF")) %>% 
  dplyr::filter(series_code == "NY.GDP.PCAP.KD") %>% 
  pivot_longer(cols=matches("x[0-9]+"),names_to="year",values_to="gdp") %>% 
  mutate(year = str_extract(year,"x([0-9])+")) %>% 
  mutate(year = str_replace(year,"x","") %>% as.numeric(.))

wb_wdi_gini <- read_csv(paste0(path_dissertation,"/aim 1/working/cohorts/5ae05ea9-fc3d-460c-a6b6-f562c1de7fa6_Data.csv")) %>% 
  janitor::clean_names(.) %>% 
  dplyr::filter(country_code %in% c("BRA","GTM","IND","PHL","ZAF")) %>% 
  dplyr::filter(series_code == "SI.POV.GINI") %>% 
  pivot_longer(cols=matches("x[0-9]+"),names_to="year",values_to="gini") %>% 
  mutate(year = str_extract(year,"x([0-9])+")) %>% 
  mutate(year = str_replace(year,"x","") %>% as.numeric(.),
         gini = as.numeric(gini))

wb_wdi <- left_join(wb_wdi_gdppc,
                    wb_wdi_gini %>% dplyr::select(year,country_code,gini),
                    by = c("year","country_code")) %>% 
  mutate_at(vars(gdp,gini),~as.numeric(.)) %>% 
  dplyr::filter(complete.cases(.))

sfigure1a <- ggplot(data=wb_wdi,
                   aes(x=year,y=gdp,col=country_code)) + 
  geom_point() + geom_line(aes(group=country_code))+
  xlab("Year") +ylab("GDP per capita (2010,$)") +
  theme_bw() +
  theme(legend.pos="bottom") +
  scale_color_discrete(name="") +
  ggtitle("A")

sfigure1b <- ggplot(data=wb_wdi,
                    aes(x=year,y=gini,col=country_code)) + 
  geom_point() + geom_line(aes(group=country_code))+
  xlab("Year") +ylab("Gini coefficient") +
  theme_bw() +
  theme(legend.pos="bottom") +
  scale_color_discrete(name="") +
  ggtitle("B")

library(ggpubr)
sfigure1 <- ggarrange(sfigure1a,
                      sfigure1b,
                      nrow = 2,ncol=1,
                      common.legend = TRUE,legend = "bottom")
sfigure1




pcall <- plyr::rbind.fill(
  # Need to check source
  b9_age4 %>% mutate(year = 1997,source = "mother"),
  b9_age11 %>% mutate(year = 2004,source = "mother"),
  b9_age15 %>% mutate(year = 2008,source = "index"),
  b9_age18 %>% mutate(year = 2011,source = "index"), # Need to check source
  b9_age22 %>% mutate(year = 2015,source = "index")
) 

# View(pcall %>% group_by(nquest,year) %>% tally())
# View(pcall %>% dplyr::filter(!is.na(d_television)) %>% group_by(year) %>% tally())
# 

write.csv(head(pcall),paste0(path_harmonization_folder,"/brazil 1993/b9_head_pcall.csv"))

# Separating and saving adult outcomes ---------

source(paste0(path_harmonization_repo,"/brazil 1993/b9_adult_outcomes.R"))


# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
b9pcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/brazil 1993/B9 Variable List.xlsx"),sheet="pcall")

pcall <- label_variables(pcall,cohorts_labels = b9pcall_labels,overwrite_var_label = TRUE)





# SAVE ---------------
saveRDS(pcall,paste0(path_harmonization_folder,"/brazil 1993/working/pcall.RDS"))
write_dta(pcall,paste0(path_harmonization_folder,"/brazil 1993/working/pcall.dta"),version=12)


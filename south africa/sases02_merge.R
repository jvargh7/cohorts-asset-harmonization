pcall <- plyr::rbind.fill(
  # Need to check source
  yr_0_to_2 %>% mutate(year = 1990,source = "mother"), # Need to check source
  yr_7_to_9 %>% mutate(year = 1997,source = "mother"), # Need to check source
  yr_12_to_13  %>% mutate(year = 2002,source = "mother"),# Need to check source
  yr_16 %>% mutate(year = 2006,source = "index"), # Need to check source
  yr_22  %>% mutate(year = 2012,source = "index"),
  yr_28  %>% mutate(year = 2018,source = "index")
)

write.csv(head(pcall),paste0(path_harmonization_folder,"/south africa/sa_head_pcall.csv"))

# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
sapcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/south africa/SA Variable List.xlsx"),sheet="pcall")

pcall <- label_variables(pcall,cohorts_labels = sapcall_labels,overwrite_var_label = TRUE)

# SAVE ---------------
saveRDS(pcall,paste0(path_harmonization_folder,"/south africa/working/pcall.RDS"))
write_dta(pcall,paste0(path_harmonization_folder,"/south africa/working/pcall.dta"),version=12)


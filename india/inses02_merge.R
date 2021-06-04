pcall <- plyr::rbind.fill(
  # Need to check source
  in_1969 %>% mutate(year = 1969,source = "mother"), # Need to check source
  in_1999 %>% mutate(year = 1999,source = "index"), # Need to check source
  in_2006  %>% mutate(year = 2006,source = "index"),# Need to check source
  in_2012 %>% mutate(year = 2012,source = "index"), # Need to check source
  in_2016  %>% mutate(year = 2016,source = "index")
)

write.csv(head(pcall),paste0(path_harmonization_folder,"/india/in_head_pcall.csv"))

# Labels ----------
source(paste0(path_harmonization_repo,"/package/label_variables.R"))
inpcall_labels = readxl::read_excel(paste0(path_harmonization_folder,"/india/IN Variable List.xlsx"),sheet="pcall")

pcall <- label_variables(pcall,cohorts_labels = inpcall_labels,overwrite_var_label = TRUE)





# SAVE ---------------
saveRDS(pcall,paste0(path_harmonization_folder,"/india/working/pcall.RDS"))
write_dta(pcall,paste0(path_harmonization_folder,"/india/working/pcall.dta"),version=12)
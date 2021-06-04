years = c(1990,1997,2002,2006,2012,2018)
# source("../.Rprofile")

source(paste0(path_harmonization_repo,"/south africa/satable01_early life and asset data availability.R"))



stable2E <- compareGroups::compareGroups(year ~ moage + moscho + fascho + ethnicity + chsex,data=table_df %>% 
                                          dplyr::filter(year %in% years,status == "Not Available or Died"),max.ylev=10,method=c(1,2,2,3,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2E,file=paste0(path_dissertation,"/aim 1/working/cohorts/sastable2E_nonparticipants.xlsx"))

stable2E_orig <- compareGroups::compareGroups(year ~ moage + moscho + fascho + ethnicity + chsex,data=table_df %>% 
                                               dplyr::filter(year == 1997),method=c(1,2,2,3,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2E_orig,file=paste0(path_dissertation,"/aim 1/working/cohorts/sastable2E_orig.xlsx"))

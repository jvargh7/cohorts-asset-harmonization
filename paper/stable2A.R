years = c(1997,2004,2008,2011,2015)
# source("../.Rprofile")

source(paste0(path_harmonization_repo,"/brazil 1993/b9table01_early life and asset data availability.R"))



stable2A <- compareGroups::compareGroups(year ~ moage + faage + moscho + fascho + moemployedpreg + moskincolor + sex + adskincolor,data=table_df %>% 
                                          dplyr::select(-nquest) %>% 
                                          dplyr::filter(year %in% years,status == "Not Available or Died")
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2A,file=paste0(path_dissertation,"/aim 1/working/cohorts/b9stable2A_nonparticipants.xlsx"))

stable2A_orig <- compareGroups::compareGroups(year ~ moage + faage + moscho + fascho + moemployedpreg + moskincolor + sex + adskincolor,data=table_df %>% 
                                          dplyr::select(-nquest) %>% 
                                          dplyr::filter(year == 1997)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2A_orig,file=paste0(path_dissertation,"/aim 1/working/cohorts/b9stable2A_orig.xlsx"))

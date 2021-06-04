# source("../.Rprofile")
years = c(1969,1999,2006,2012,2016)

source(paste0(path_harmonization_repo,"/india/intable01_early life and asset data availability.R"))

stable2C <- compareGroups::compareGroups(year ~ moage + moscho + fascho + doby + chsex + religion,include.miss = TRUE,data=table_df %>% 
                                           dplyr::filter(year %in% years,status %in% c("Not Available or Died")),method = c(1,2,2,2,3,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2C,file=paste0(path_dissertation,"/aim 1/working/cohorts/stable2C_nonparticipants.xlsx"))

stable2C_orig <- compareGroups::compareGroups(~ moage + moscho + fascho + doby + chsex + religion,
                                              include.miss = TRUE,data=early_life,method = c(1,2,2,2,3,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2C_orig,file=paste0(path_dissertation,"/aim 1/working/cohorts/stable2C_orig.xlsx"))

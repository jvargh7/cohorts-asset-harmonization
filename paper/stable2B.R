# source("../.Rprofile")
years = c(1967,1975,1987,1996,2002,2016,2018)

source(paste0(path_harmonization_repo,"/guatemala/gttable01_early life and asset data availability.R"))

stable2B <- compareGroups::compareGroups(year ~ moage + moht_sib + moscho_sib + gtchbyear + gtatole + chsex,include.miss = TRUE,data=table_df %>% 
                                          dplyr::filter(year %in% years,status %in% c("Data Not Available","Dead")),method = c(1,1,2,2,3,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2B,file=paste0(path_dissertation,"/aim 1/working/cohorts/stable2B_nonparticipants.xlsx"))

stable2B_orig <- compareGroups::compareGroups(~ moage + moht_sib + moscho_sib + gtchbyear + gtatole + chsex,
                                             include.miss = TRUE,data=early_life,method = c(1,1,2,2,3,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2B_orig,file=paste0(path_dissertation,"/aim 1/working/cohorts/stable2B_orig.xlsx"))

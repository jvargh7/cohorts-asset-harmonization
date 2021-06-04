years = c(1983, 1991, 1994, 1998, 2002, 2005, 2009, 2018)
# source("../.Rprofile")

source(paste0(path_harmonization_repo,"/philippines/phtable01_early life and asset data availability.R"))



stable2D <- compareGroups::compareGroups(year ~ moage + moscho + moht + chsex,data=table_df %>% 
                                          dplyr::filter(year %in% years,status == "Not Available or Died"),max.ylev=10,method=c(1,2,1,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n =
                               TRUE)

compareGroups::export2xls(stable2D,file=paste0(path_dissertation,"/aim 1/working/cohorts/phstable2D_nonparticipants.xlsx"))

stable2D_orig <- compareGroups::compareGroups(year ~ moage + moscho + moht + chsex,data=table_df %>% 
                                               dplyr::filter(year == 1983),method=c(1,2,1,3)
) %>% 
  compareGroups::createTable(.,sd.type = 2,digits=1,show.p.mul = FALSE,show.p.overall = FALSE,show.n = TRUE)

compareGroups::export2xls(stable2D_orig,file=paste0(path_dissertation,"/aim 1/working/cohorts/phstable2D_orig.xlsx"))

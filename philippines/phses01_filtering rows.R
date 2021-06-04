
# NOTES:
# 1. >=23310 is a twin (e.g. 4XXXX); Sibling would be same but with 2XXXX, Moms with twins are >=13310
# 2. Children moving out and moving back (use MOMCH or WHOSEHOUSEHOLD variable in some files; sesairic has 4 levels - m, c +m, c+s,c+caretaker)
# 3. Status variable is data at time of screening survey
# 4. There are edits to 'bathroom (1994, 1998, 2002, 2005, 2018)', 'watercon (1983)', resarea (1991, 1994, 1998, 2002, 2005, 2009, 2018) and area50m (1991, 1994, 1998, 2002, 2005, 2009, 2018)


# All mothers who were enrolled
idbaselink <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/idbaselink/idbaselink.dta"))

# Has 3327 uncchdid < 40000 ~ All mothers who were enrolled with a hypothetical child ID
idwithtwins <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/idbaselink/idbaselinkidwithtwins.dta"))

# Has 3354 unique uncchdid and 3328 uncmomid
ic_masterlist <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta"))

# Unique ID lists ---------
uncchdid_singletons <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/sesairic/sesairic.dta")) %>%
  # dplyr::select(basebrgy:currbrgy05,uncchdid,contains("icsex"),contains("icage")) %>%
  dplyr::filter(!is.na(icsex)) %>% 
  dplyr::select(uncchdid) %>% 
  pull()

uncmomid_singletons <- uncchdid_singletons - 10000

uncchdid_twins <- idwithtwins %>% 
  dplyr::filter(uncchdid >= 23310) %>% 
  dplyr::mutate(uncmomid = case_when(uncchdid >= 60000 ~ uncchdid - 50000,
                                     uncchdid >= 40000 ~ uncchdid - 30000,
                                     TRUE ~ uncchdid - 10000)) %>% 
  dplyr::select(uncchdid) %>% 
  pull()
uncmomid_twins <- idwithtwins %>% 
  dplyr::filter(uncchdid >= 23310) %>% 
  dplyr::mutate(uncmomid = case_when(uncchdid >= 60000 ~ uncchdid - 50000,
                                     uncchdid >= 40000 ~ uncchdid - 30000,
                                     
                                     TRUE ~ uncchdid - 10000)) %>% 
  dplyr::select(uncmomid) %>% 
  distinct(.) %>% 
  pull()

# ic_masterlist preprocessing -------------

# The numbers at the end of the variables represent survey as follows
# *91 - *05 are survey years 1991, 1994, 1998 2002 2005
# 
# Others were added from a dataset that numbered the surveys, so
# *0 is baseline (year of child’s birth)
# *12 is our 12th longitudinal survey when the child was 2 years of age
# *13 is 1991
# *14 is 1994
# *15 is 1998
# *16 is 2002
# *17 is 2005

ic_masterlist <- ic_masterlist %>% 
  dplyr::filter(!is.na(uncmomid)) %>% 
  dplyr::filter(uncchdid %in% c(uncchdid_singletons,uncchdid_twins))

# 1991 did not have data on twins
ic1991 <- ic_masterlist %>% 
  dplyr::filter(survey == 1991, whosedata %in% c(1,2,4,5))

# 1994 had data on 28 twins
ic1994 <- ic_masterlist %>% 
  dplyr::filter(survey == 1994, whosedata %in% c(1,2,4,5))

# 1998 had data on 28 twins
ic1998 <- ic_masterlist %>% 
  dplyr::filter(survey == 1998, whosedata %in% c(1,2,4,5))

# 2002 had data on 28 twins
ic2002c <- ic_masterlist %>% 
  dplyr::filter(survey == 2002, whosedata %in% c(1,2,4,5,10,11))

# 2005 had data on 23 twins
ic2005c <- ic_masterlist %>% 
  dplyr::filter(survey == 2005, whosedata %in% c(1,2,4,5,10,11))

# 2007 had data on 24 twins
ic2007c <- ic_masterlist %>% 
  dplyr::filter(survey == 2007, whosedata %in% c(1,2,4,5,10,11))

# 1983 (M = 3098, C = NA) ----------

assets83 <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets83.RDS")) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>% 
  dplyr::filter(uncmomid %in% c(uncmomid_singletons,uncmomid_twins)) %>% 
  left_join(idwithtwins,
            by = c("basewman","basebrgy"))


# 1991 Only index child : IJE-2264 ----------

assets91 <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets91.RDS")) %>% 
  mutate(childstatus = case_when(status1991 %in% c(1,5,6,7,11) ~ 1, # IC interviewed
                                 status1991 %in% c(2,10) ~ 0, # IC missing 
                                 status1991 %in% c(3) ~ 0, # IC dead
                                 TRUE ~ NA_real_)) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>% 
  dplyr::filter(uncmomid %in% c(uncmomid_twins,uncmomid_singletons)) %>% 
  dplyr::filter(childstatus == 1) %>% 
  # We use this filter here to retain only mothers who are present in ic1991
  # dplyr::filter(uncmomid %in% ic1991$uncmomid) %>%
  # The right join performs the same function as the filter but we are getting uncchdid also
  right_join(ic1991 %>% 
               dplyr::select(uncmomid,uncchdid),
             by=c("uncmomid"))

# # View(assets91 %>% group_by(uncmomid) %>% dplyr::filter(n()>1))
## There are 6 records with duplicates after filtering for child status and mother ID - pick one randomly

assets91 <- assets91 %>% 
  distinct(uncmomid,status1991,.keep_all=TRUE)



# 1994 Only index child : IJE-2186 -----------

assets94 <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets94.RDS")) %>% 
  mutate(childstatus = case_when(mochint1994 %in% c(1,3) ~ 1,
                                 mochint1994 %in% c(2) ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>% 
  dplyr::filter(uncmomid %in% c(uncmomid_twins,uncmomid_singletons)) %>% 
  dplyr::filter(childstatus == 1) %>% 
  # Filter and right join are interchangeable
  # dplyr::filter(uncmomid %in% ic1994$uncmomid) %>% 
  right_join(ic1994 %>% 
               dplyr::select(uncmomid,uncchdid),
             by=c("uncmomid"))

# 1998 Only index child : IJE-2089 -----------
assets98 <- bind_rows(readRDS(paste0(path_harmonization_folder,"/philippines/working/assets98b.RDS")),
                      readRDS(paste0(path_harmonization_folder,"/philippines/working/assets98g.RDS"))) %>% 
  mutate(childstatus = case_when(mochint1998 %in% c(1,3,4,5,6,7,8,9,-3,-4,-5,-6) ~ 1,
                                 mochint1998 %in% c(2,-7) ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  rename(uncmomid1998 = uncmomid) %>% 

  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>%
  
  # All filtering based on uncmomid at baseline
  dplyr::filter(uncmomid %in% c(uncmomid_twins,uncmomid_singletons)) %>% 
  
  # Did not work because birth mother is mapped in ic_masterlist
  # dplyr::filter(uncmomid %in% ic1998$uncmomid|uncmomid1998 %in% ic1998$uncmomid)
  
  dplyr::filter(childstatus == 1) %>% 
  left_join(ic1998 %>% 
               dplyr::select(uncmomid,uncchdid),
             by=c("uncmomid")) %>% 
  distinct(uncchdid,brgy1998,hhid1998,.keep_all=TRUE)

# How many mothers interviewed match their mother ID - 1991 out of 2202
with(assets98,table(uncmomid1998==uncmomid))
# Implies 211 children stay with non-mothers

# 2002 Only index child : IJE-2023 ----------

# uncchdid is available from this wave onwards
assets02c <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets02c.RDS")) %>% 
  mutate(childstatus = case_when(mochint2002 %in% c(3,13) ~ 1,
                                 mochint2002 %in% c(1) ~ 1, # This is the mother interview code for 'Mother and Index Child'
                                 mochint2002 %in% c(5,6,11,12,14,15) ~ 0, # Missing/Unable/Refused
                                 mochint2002 %in% c(4) ~ 0, # Dead
                                 TRUE ~ NA_real_)) %>% 
  mutate(child = 1) %>% 
  dplyr::filter(childstatus==1) %>% 
  mutate(is_twin = case_when(uncchdid %in% uncchdid_twins ~ 1,
                             TRUE ~ 0))  %>% 
  dplyr::filter(uncmomid %in% ic2002c$uncmomid)

# No need to merge with ic2002c because uncchdid is already there

# How many children in assets02c are twins - 4
table(assets02c$is_twin)

# Cross-check of above: How many mothers in assets02c are mothers of twins - 4
table(assets02c$uncmomid %in% uncmomid_twins)

# 1724 of 2102 remaining after below step
assets02m <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets02m.RDS")) %>% 
  mutate(childstatus = case_when(mochint2002 %in% c(1) ~ 1,
                                 mochint2002 %in% c(7,8,9,10) ~ 0, # Missing/Unable/Refused
                                 mochint2002 %in% c(2) ~ 0, # Mother only
                                 TRUE ~ NA_real_)) %>% 
  rename(uncmomid2002 = uncmomid) %>%
  # Remove all mother IDs where children don't live with mom
  dplyr::filter(childstatus == 1) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>%
  dplyr::filter(uncmomid %in% c(uncmomid_twins,uncmomid_singletons)) %>% 
  mutate(child = 0) %>% 
  # Remove all mother IDs who have children in assets02c
  dplyr::filter(!(uncmomid %in% assets02c$uncmomid)) %>% 
  mutate(mom_of_twins = case_when(uncmomid %in% uncmomid_twins ~ 1,
                                  TRUE ~ 0))  %>% 
  # dplyr::filter(uncmomid %in% ic2002c$uncmomid) %>%
  right_join(ic2002c %>% 
               dplyr::filter(!(uncchdid %in% assets02c$uncchdid)) %>% 
              dplyr::select(uncmomid,uncchdid),
            by=c("uncmomid")) 

# # View(assets02m %>% group_by(uncchdid) %>% tally())

#How many mothers in assets02m match with the mother ID - ALL
with(assets02m,table(uncmomid2002==uncmomid,is.na(uncmomid2002),useNA="always")) 
# All of them match - implies assets02m is for (a) all mothers whose children if eligible, live with them OR (b) children aren't eligible

# How many twins after getting mother dataset - 24
table(assets02m$mom_of_twins)

# How many children have mothers who also reported in assets02m - NOT VALID NOW since we filtered on it
table(assets02m$uncmomid %in% assets02c$uncmomid,assets02m$childstatus,useNA="always")
assets02m <- assets02m %>% 
  dplyr::filter(!is.na(childstatus))


# How many unique mother IDs
length(unique(c(assets02c$uncmomid,assets02m$uncmomid)))

# 2005 Only index child : 1888 ----------

assets05c <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets05c.RDS")) %>% 
  mutate(childstatus = case_when(mochint2005 %in% c(1,3,13,18) ~ 1,
                                 mochint2005 %in% c(2,12) ~ 0,
                                 TRUE ~ NA_real_
  )) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>% 
  mutate(child = 1) %>% 
  mutate(is_twin = case_when(uncchdid %in% uncchdid_twins ~ 1,
                             TRUE ~ 0))  %>% 
  dplyr::filter(uncmomid %in% ic2005c$uncmomid)

# How many children in assets05c are twins - 8
table(assets05c$is_twin)


assets05m <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets05m.RDS")) %>% 
  mutate(childstatus = case_when(mochint2005 %in% c(1,3,13,18) ~ 1,
                                 mochint2005 %in% c(2,12) ~ 0,
                                 TRUE ~ NA_real_
  )) %>% 
  dplyr::filter(childstatus == 1) %>% 
  rename(uncmomid2005 = uncmomid) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>%
  dplyr::filter(uncmomid %in% c(uncmomid_twins,uncmomid_singletons)) %>% 
  mutate(child = 0) %>%   
  # Remove all mother IDs who have children in assets05c
  dplyr::filter(!(uncmomid %in% assets05c$uncmomid)) %>% 
  mutate(mom_of_twins = case_when(uncmomid %in% uncmomid_twins ~ 1,
                                  TRUE ~ 0)) %>% 
  # dplyr::filter(uncmomid %in% ic2005c$uncmomid) %>% 
  right_join(ic2005c %>% 
               dplyr::filter(!(uncchdid %in% assets05c$uncchdid)) %>% 
               dplyr::select(uncmomid,uncchdid),
             by=c("uncmomid")) 

# How many twins in mother dataset - 10
with(assets05m,table(uncmomid %in% uncmomid_twins))

#How many records in assets05m match with the mother ID - ALL
with(assets05m,table(uncmomid2005==uncmomid))   


# How many children have mothers who also reported in assets05m-
table(assets05m$uncmomid %in% assets05c$uncmomid,assets05m$childstatus,useNA="always")
assets05m <- assets05m %>% 
  dplyr::filter(!is.na(childstatus))



# 2007 Only index child : 1817 ----------

assets07c <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets07c.RDS")) %>% 
  mutate(child = 1) %>% 
  mutate(childstatus = case_when(mochint2007 %in% c(1,3,13) ~ 1,
                                 mochint2007 %in% c(2,12,16) ~ 0,
                                 TRUE ~ NA_real_
  )) %>% 
  mutate(is_twin = case_when(uncchdid %in% uncchdid_twins ~ 1,
                             TRUE ~ 0))  %>% 
  dplyr::filter(uncmomid %in% ic2007c$uncmomid)

# How many children in assets07c are twins - 9
table(assets07c$is_twin)


assets07m <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets07m.RDS")) %>% 
  mutate(child = 0) %>% 
  mutate(childstatus = case_when(mochint2007 %in% c(1,3,13) ~ 1,
                                 mochint2007 %in% c(2,12,16) ~ 0,
                                 TRUE ~ NA_real_
  )) %>% 
  dplyr::filter(childstatus == 1) %>% 
  rename(uncmomid2007 = uncmomid) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy")) %>%
  dplyr::filter(uncmomid %in% c(uncmomid_twins,uncmomid_singletons)) %>% 
  mutate(child = 0) %>%   
  # Remove all mother IDs who have children in assets05c
  dplyr::filter(!(uncmomid %in% assets07c$uncmomid)) %>% 
  mutate(mom_of_twins = case_when(uncmomid %in% uncmomid_twins ~ 1,
                                  TRUE ~ 0))  %>% 
  # dplyr::filter(uncmomid %in% ic2007c$uncmomid)
  right_join(ic2007c %>%
               dplyr::filter(!(uncchdid %in% assets07c$uncchdid)) %>%
               dplyr::select(uncmomid),
             by=c("uncmomid"))

# How many are mothers of twins - 9
with(assets07m,table(uncmomid %in% uncmomid_twins))

# How many children have mothers who also reported in assets07m
table(assets07m$uncmomid %in% assets07c$uncmomid,assets07m$childstatus,useNA="always")
assets07m <- assets07m %>% 
  dplyr::filter(!is.na(childstatus))


# # View(assets07m %>% group_by(uncchdid) %>% dplyr::filter(n()>1))
# There are 6 records with duplicated uncchdid. 

assets07m <- assets07m %>% 
  distinct(uncchdid,uncmomid,.keep_all = TRUE)

# 2009 Only index child : ----------

assets09c <- bind_rows(readRDS(paste0(path_harmonization_folder,"/philippines/working/assets09fe.RDS")),
                      readRDS(paste0(path_harmonization_folder,"/philippines/working/assets09ma.RDS"))) %>% 
  mutate(is_twin = case_when(uncchdid %in% uncchdid_twins ~ 1,
                             TRUE ~ 0))

# How many children in assets09c are twins - 22
table(assets09c$is_twin)

# 2017-18 Only index child : 1343 ----------
assets18c <- readRDS(paste0(path_harmonization_folder,"/philippines/working/assets18.RDS")) %>% 
  mutate(is_twin = case_when(uncchdid %in% uncchdid_twins ~ 1,
                             TRUE ~ 0)) %>% 
  left_join(idbaselink,
            by = c("basewman","basebrgy"))

# How many children in assets18c are twins - 17
table(assets18c$is_twin)



# cps_clean.R
# Load and clean CPS data file


# Load CPS file (code provided by CPS) ####
ddi <- read_ipums_ddi(paste0(cps_path,"cps_00008.xml"))
cps_data <- read_ipums_micro(ddi)


# Basic cleaning ####
# Select and rename relevant variables
cps_data <- cps_data %>%
  select(id=CPSIDP,
         hh_id=CPSID,
         year=YEAR,
         month=MONTH,
         region=REGION,
         sample_weight=ASECWTH,
         cpi99=CPI99,
         age=AGE,
         sex=SEX,
         race=RACE,
         education_pre=EDUC,
         citizen=CITIZEN,
         class_worker=CLASSWKR,
         occupation=OCC1950,
         industry=IND,
         income=INCTOT,
         wage=INCWAGE,
         labour_force=LABFORCE,
         employment_status=EMPSTAT,
         work_stat=WKSTAT,
         work_hrs_wk=AHRSWORKT,
         usual_work_hrs_wk=UHRSWORKT,
         work_wk=WKSWORK2,
         pension=PENSION,
         child_support=INCCHILD,
         health_plan=HINSEMP,
         marriage=MARST,
         child=NCHILD,
         hh_serial=SERIAL,
         asec_flag=ASECFLAG,
         h_flag=HFLAG)

# Check some statistics before cleaning
count(cps_data) #9223447
cps_data %>% filter(!(race==100 | race==200)) %>% count() #520724, 5.65%
cps_data %>% filter(income==999999998 | income==999999999) %>% count() #2156032, 23.38%
cps_data %>% filter(wage==99999998 | wage==99999999) %>% count() #2156008, 23.38%

# Basic clean
cps_data <- cps_data %>%
  mutate(year=as.numeric(year),
         
         # Bin year
         bin_year=NA,
         bin_year=ifelse(year<1970, 1960, bin_year),
         bin_year=ifelse(1970<=year & year<1980, 1970, bin_year),
         bin_year=ifelse(1980<=year & year<1990, 1980, bin_year),
         bin_year=ifelse(1990<=year & year<2000, 1990, bin_year),
         bin_year=ifelse(2000<=year & year<2010, 2000, bin_year),
         bin_year=ifelse(2010<=year, 2010, bin_year),
         
         # Recode region
         region=as.numeric(region),
         region=recode(region,
                       `11`="new_england",
                       `12`="middle_atlantic",
                       `21`="east_north_central",
                       `22`="west_north_central",
                       `31`="south_atlantic",
                       `32`="east_south_central",
                       `33`="west_south_central",
                       `41`="mountain",
                       `42`="pacific",
                       .default=""),
         
         age=as.numeric(age),
         sex=ifelse(sex==1, "Male",
                    ifelse(sex==2, "Female", NA)),
         race=ifelse(race==100, "White",
                     ifelse(race==200, "Black", "Other")),
         
         # Recode education
         education=as.numeric(education_pre),
         education=recode(education,
                          `10`=2L,
                          `11`=1L,
                          `12`=2L,
                          `13`=3L,
                          `14`=4L,
                          `20`=5L,
                          `21`=5L,
                          `22`=6L,
                          `30`=7L,
                          `31`=7L,
                          `32`=8L,
                          `40`=9L,
                          `50`=10L,
                          `60`=11L,
                          `70`=11L,
                          `71`=11L,
                          `72`=12L,
                          `73`=12L,
                          `80`=13L,
                          `81`=13L,
                          `90`=14L,
                          `91`=14L,
                          `92`=14L,
                          `100`=15L,
                          `110`=16L,
                          `111`=16L,
                          `120`=17L,
                          `121`=17L,
                          `122`=18L,
                          `123`=18L,
                          `124`=18L,
                          `125`=22L,
                          .default=NA_integer_),
         
         # Indicator variables for schooling
         hschool=education==12,
         college=education==16,
         
         # Recode worker class
         class_worker=as.numeric(class_worker),
         class_worker=recode(class_worker,
                             `10`="Self-Employed",
                             `13`="Self-Employed",
                             `14`="Self-Employed",
                             `21`="Private",
                             `22`="Private",
                             `23`="Private",
                             `24`="Public-All",
                             `25`="Public-Federal",
                             `26`="Public-Military",
                             `27`="Public-State",
                             `28`="Public-Local",
                             .default=""),
         
         # Bin occupation
         bin_occupation="",
         bin_occupation=ifelse(occupation %in% 0:99,
                               "Professional, Technical",
                               bin_occupation),
         bin_occupation=ifelse(occupation %in% c(100:199, 600:699, 800:970),
                               "Labourers and Operatives",
                               bin_occupation),
         bin_occupation=ifelse(occupation %in% 200:299,
                               "Managers, Officials, Proprietors",
                               bin_occupation),
         bin_occupation=ifelse(occupation %in% 300:399,
                               "Clerical",
                               bin_occupation),
         bin_occupation=ifelse(occupation %in% 400:499,
                               "Sales",
                               bin_occupation),
         bin_occupation=ifelse(occupation %in% 500:599,
                               "Crafts",
                               bin_occupation),
         bin_occupation=ifelse(occupation %in% 700:799,
                               "Services",
                               bin_occupation),
         
         income=ifelse(income %in% c(999999998, 999999999), NA, income),
         wage=ifelse(wage %in% c(99999998, 99999999), NA, wage),
         labour_force=ifelse(labour_force==1, 0,
                             ifelse(labour_force==2, 1, NA)),
         
         # Recode weeks worked
         work_wk=as.numeric(work_wk),
         work_wk=recode(work_wk,
                        `1`=7L,
                        `2`=20L,
                        `3`=33L,
                        `4`=43L,
                        `5`=48L,
                        `6`=51L,
                        .default=NA_integer_),
         
         pension=ifelse(pension %in% c(2, 3), 1,
                        ifelse(pension %in% c(0, 1), 0, NA)),
         child_support=ifelse(child_support==999999, NA, child_support),
         health_plan=ifelse(health_plan==1, 0,
                            ifelse(health_plan==2, 1, NA)),
         
         # Indicator variable for marriage
         married=marriage==1)

# Remove unnecessary data
rm(ddi)

# cps_clean.R
# Load and clean CPS data file


# Load CPS file (code provided by CPS) ####
ddi <- read_ipums_ddi(paste0(cps_path,"cps_00007.xml"))
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
         occupation=OCC,
         industry=IND,
         income=INCTOT,
         wage=INCWAGE,
         labour_force=LABFORCE,
         employment_status=EMPSTAT,
         work_stat=WKSTAT,
         work_hrs_wk=AHRSWORKT,
         usual_work_hrs_wk=UHRSWORKT,
         work_wk=WKSWORK2,
         marriage=MARST,
         child=NCHILD,
         hh_serial=SERIAL,
         asec_flag=ASECFLAG,
         h_flag=HFLAG)

# Check some statistics before cleaning
count(cps_data) #9223447
cps_data %>% filter(!(race==100 | race==200)) %>% count() #520724, 5.65%
cps_data %>% filter(income==99999998 | income==99999999) %>% count() #2156032, 23.38%
cps_data %>% filter(wage==9999998 | wage==9999999) %>% count() #2156008, 23.38%

# Basic clean
cps_data <- cps_data %>%
  mutate(year=as.numeric(year),
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
         income=ifelse(income==99999998 | income==99999999, NA, income),
         wage=ifelse(wage==9999998 | wage==9999999, NA, wage),
         labour_force=ifelse(labour_force==1, 0,
                             ifelse(labour_force==2, 1, NA)),
         
         work_wk=as.numeric(work_wk),
         work_wk=recode(work_wk,
                        `1`=7L,
                        `2`=20L,
                        `3`=33L,
                        `4`=43L,
                        `5`=48L,
                        `6`=51L,
                        .default=NA_integer_),
         
         # Indicator variable for marriage
         married=marriage==1)

# Bin schooling and experience
cps_data <- cps_data %>%
  mutate(bin_education=recode(education,
                              `9`=10L,
                              `10`=10L,
                              `11`=10L,
                              `12`=12L,
                              `13`=14L,
                              `14`=14L,
                              `15`=14L,
                              `16`=16L,
                              `17`=18L,
                              `18`=18L,
                              `22`=18L,
                              .default=NA_integer_),
         bin_experience=pmax(0, age-bin_education-6))

# Remove unnecessary data
rm(ddi)

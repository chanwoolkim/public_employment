# cps_clean.R
# Load and clean CPS data file


# Load CPS file (code provided by CPS) ####
ddi <- read_ipums_ddi(paste0(cps_path,"cps_00001.xml"))
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
         income=INCTOT,
         wage=INCWAGE,
         hh_income=HHINCOME,
         family_income=FAMINC,
         labour_force=LABFORCE,
         employment_status=EMPSTAT,
         work_stat=WKSTAT,
         work_hrs_wk=AHRSWORKT,
         usual_work_hrs_wk=UHRSWORKT,
         work_wk=WKSWORK1,
         marriage=MARST,
         child=NCHILD,
         hh_serial=SERIAL,
         asec_flag=ASECFLAG,
         h_flag=HFLAG)

# Check some statistics before cleaning
count(cps_data) #7221527
cps_data %>% filter(is.na(id)) %>% count() #1634415, 22.63%
cps_data %>% filter(!(race==100 | race==200)) %>% count() #495543, 6.86%
cps_data %>% filter(income==99999998 | income==99999999) %>% count() #1706804, 23.63%
cps_data %>% filter(wage==9999998 | wage==9999999) %>% count() #1706804, 23.63%

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
         
         income=ifelse(income==99999998 | income==99999999, NA, income),
         wage=ifelse(wage==9999998 | wage==9999999, NA, wage),
         labour_force=ifelse(labour_force==1, 0,
                             ifelse(labour_force==2, 1, NA)),
         
         # Indicator variable for marriage
         married=marriage==1)

# Basic filtering
cps_data <- cps_data %>%
  filter(age>=18 & race %in% c("White", "Black"))

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

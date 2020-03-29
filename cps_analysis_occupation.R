# cps_analysis_occupation.R
# Analyse occupational distribution


# Occupational Distribution ####
cps_analysis_occupation <- cps_analysis %>%
  mutate(sector=ifelse(class_worker=="Private",
                       "Private",
                       ifelse(class_worker %in% c("Public-All",
                                                  "Public-Federal",
                                                  "Public-State",
                                                  "Public-Local"),
                              "Public",
                              NA)),
         all=bin_occupation!="",
         occupation_professional=bin_occupation=="Professional, Technical",
         occupation_labour=bin_occupation=="Labourers and Operatives",
         occupation_manager=bin_occupation=="Managers, Officials, Proprietors",
         occupation_clerical=bin_occupation=="Clerical",
         occupation_sales=bin_occupation=="Sales",
         occupation_craft=bin_occupation=="Crafts",
         occupation_services=bin_occupation=="Services") %>%
  filter(!is.na(sector))

cps_analysis_occupation_fraction <- cps_analysis_occupation %>%
  group_by(bin_year, sector) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            occupation_professional=sum(occupation_professional*sample_weight, na.rm=TRUE),
            occupation_labour=sum(occupation_labour*sample_weight, na.rm=TRUE),
            occupation_manager=sum(occupation_manager*sample_weight, na.rm=TRUE),
            occupation_clerical=sum(occupation_clerical*sample_weight, na.rm=TRUE),
            occupation_sales=sum(occupation_sales*sample_weight, na.rm=TRUE),
            occupation_craft=sum(occupation_craft*sample_weight, na.rm=TRUE),
            occupation_services=sum(occupation_services*sample_weight, na.rm=TRUE)) %>%
  mutate(`Professional, Technical`=occupation_professional/all,
         `Labourers and Operatives`=occupation_labour/all,
         `Managers, Officials, Proprietors`=occupation_manager/all,
         `Clerical`=occupation_clerical/all,
         `Sales`=occupation_sales/all,
         `Crafts`=occupation_craft/all,
         `Services`=occupation_services/all) %>%
  select(sector,
         bin_year,
         `Professional, Technical`,
         `Labourers and Operatives`,
         `Managers, Officials, Proprietors`,
         `Clerical`,
         `Sales`,
         `Crafts`,
         `Services`) %>%
  ungroup %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector)

print(xtable(cps_analysis_occupation_fraction,
             digits=c(0, 0, 3, 3, 3, 3, 3, 3, 3)),
      file="result/occupation_proportion.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

cps_analysis_occupation_fraction_sex <- cps_analysis_occupation %>%
  group_by(bin_year, sector, sex) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            occupation_professional=sum(occupation_professional*sample_weight, na.rm=TRUE),
            occupation_labour=sum(occupation_labour*sample_weight, na.rm=TRUE),
            occupation_manager=sum(occupation_manager*sample_weight, na.rm=TRUE),
            occupation_clerical=sum(occupation_clerical*sample_weight, na.rm=TRUE),
            occupation_sales=sum(occupation_sales*sample_weight, na.rm=TRUE),
            occupation_craft=sum(occupation_craft*sample_weight, na.rm=TRUE),
            occupation_services=sum(occupation_services*sample_weight, na.rm=TRUE)) %>%
  mutate(`Professional, Technical`=occupation_professional/all,
         `Labourers and Operatives`=occupation_labour/all,
         `Managers, Officials, Proprietors`=occupation_manager/all,
         `Clerical`=occupation_clerical/all,
         `Sales`=occupation_sales/all,
         `Crafts`=occupation_craft/all,
         `Services`=occupation_services/all) %>%
  select(sector,
         bin_year,
         sex,
         `Professional, Technical`,
         `Labourers and Operatives`,
         `Managers, Officials, Proprietors`,
         `Clerical`,
         `Sales`,
         `Crafts`,
         `Services`) %>%
  ungroup

cps_analysis_occupation_fraction_male <- cps_analysis_occupation_fraction_sex %>%
  filter(sex=="Male") %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector, -sex)

cps_analysis_occupation_fraction_female <- cps_analysis_occupation_fraction_sex %>%
  filter(sex=="Female") %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector, -sex)

print(xtable(cps_analysis_occupation_fraction_male,
             digits=c(0, 0, 3, 3, 3, 3, 3, 3, 3)),
      file="result/occupation_proportion_male.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

print(xtable(cps_analysis_occupation_fraction_female,
             digits=c(0, 0, 3, 3, 3, 3, 3, 3, 3)),
      file="result/occupation_proportion_female.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

# cps_analysis_education.R
# Analyse education attainment


# Education attainment in two sectors ####
cps_analysis_education <- cps_analysis %>%
  mutate(sector=ifelse(class_worker=="Private",
                       "Private",
                       ifelse(class_worker %in% c("Public-All",
                                                  "Public-Federal",
                                                  "Public-State",
                                                  "Public-Local"),
                              "Public",
                              NA)),
         all=!is.na(education),
         education_hd=education<12,
         education_hs=education==12,
         education_sc=(12<education & education<16),
         education_c=education==16,
         education_p=education>16) %>%
  filter(!is.na(sector))

cps_analysis_education_fraction <- cps_analysis_education %>%
  group_by(bin_year, sector) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            education_hd=sum(education_hd*sample_weight, na.rm=TRUE),
            education_hs=sum(education_hs*sample_weight, na.rm=TRUE),
            education_sc=sum(education_sc*sample_weight, na.rm=TRUE),
            education_c=sum(education_c*sample_weight, na.rm=TRUE),
            education_p=sum(education_p*sample_weight, na.rm=TRUE)) %>%
  mutate(`Below High School`=education_hd/all,
         `High School Graduate`=education_hs/all,
         `Some College`=education_sc/all,
         `College Graduate`=education_c/all,
         `Above College`=education_p/all) %>%
  select(sector,
         bin_year,
         `Below High School`,
         `High School Graduate`,
         `Some College`,
         `College Graduate`,
         `Above College`) %>%
  ungroup %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector)

print(xtable(cps_analysis_education_fraction,
             digits=c(0, 0, 3, 3, 3, 3, 3)),
      file="Result/education_proportion.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

cps_analysis_education_fraction_sex <- cps_analysis_education %>%
  group_by(bin_year, sector, sex) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            education_hd=sum(education_hd*sample_weight, na.rm=TRUE),
            education_hs=sum(education_hs*sample_weight, na.rm=TRUE),
            education_sc=sum(education_sc*sample_weight, na.rm=TRUE),
            education_c=sum(education_c*sample_weight, na.rm=TRUE),
            education_p=sum(education_p*sample_weight, na.rm=TRUE)) %>%
  mutate(`Below High School`=education_hd/all,
         `High School Graduate`=education_hs/all,
         `Some College`=education_sc/all,
         `College Graduate`=education_c/all,
         `Above College`=education_p/all) %>%
  select(bin_year,
         sector,
         sex,
         `Below High School`,
         `High School Graduate`,
         `Some College`,
         `College Graduate`,
         `Above College`) %>%
  ungroup

cps_analysis_education_fraction_male <- cps_analysis_education_fraction_sex %>%
  filter(sex=="Male") %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector, -sex)

cps_analysis_education_fraction_female <- cps_analysis_education_fraction_sex %>%
  filter(sex=="Female") %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector, -sex)

print(xtable(cps_analysis_education_fraction_male,
             digits=c(0, 0, 3, 3, 3, 3, 3)),
      file="Result/education_proportion_male.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

print(xtable(cps_analysis_education_fraction_female,
             digits=c(0, 0, 3, 3, 3, 3, 3)),
      file="Result/education_proportion_female.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

cps_analysis_education_fraction_race <- cps_analysis_education %>%
  group_by(bin_year, sector, race) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            education_hd=sum(education_hd*sample_weight, na.rm=TRUE),
            education_hs=sum(education_hs*sample_weight, na.rm=TRUE),
            education_sc=sum(education_sc*sample_weight, na.rm=TRUE),
            education_c=sum(education_c*sample_weight, na.rm=TRUE),
            education_p=sum(education_p*sample_weight, na.rm=TRUE)) %>%
  mutate(`Below High School`=education_hd/all,
         `High School Graduate`=education_hs/all,
         `Some College`=education_sc/all,
         `College Graduate`=education_c/all,
         `Above College`=education_p/all) %>%
  select(bin_year,
         sector,
         race,
         `Below High School`,
         `High School Graduate`,
         `Some College`,
         `College Graduate`,
         `Above College`) %>%
  ungroup

cps_analysis_education_fraction_white <- cps_analysis_education_fraction_race %>%
  filter(race=="White") %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector, -race)

cps_analysis_education_fraction_black <- cps_analysis_education_fraction_race %>%
  filter(race=="Black") %>%
  arrange(sector, bin_year) %>%
  rename(`Sector/Year`=bin_year) %>%
  select(-sector, -race)

print(xtable(cps_analysis_education_fraction_white,
             digits=c(0, 0, 3, 3, 3, 3, 3)),
      file="Result/education_proportion_white.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

print(xtable(cps_analysis_education_fraction_black,
             digits=c(0, 0, 3, 3, 3, 3, 3)),
      file="Result/education_proportion_black.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

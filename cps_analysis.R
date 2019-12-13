# cps_analysis.R
# Analyse CPS data


# Preliminary data preparation ####
# Take subsample
cps_analysis <- cps_data %>%
  filter(age>=18 & age<=65,
         race %in% c("White", "Black"),
         region != "",
         class_worker != "",
         labour_force==1,
         work_hrs_wk>=35,
         work_wk>=40,
         sample_weight>=0)

# Create log wage variable in 1999 dollars
cps_analysis <- cps_analysis %>%
  mutate(wage99=wage*cpi99,
         log_wage99=log(wage99),
         experience=pmax(0,
                         age-education-6),
         wk_wage99=wage99/(work_wk),
         log_wk_wage99=log(wk_wage99),
         hr_wage99=wage99/(work_hrs_wk*work_wk),
         log_hr_wage99=log(hr_wage99))

# Filter to those with half the minimum wage in 1990 dollar
cps_analysis <- cps_analysis %>%
  filter(wage99>=103)

count(cps_analysis) #2513292


# Share of public sector employees ####
employee_count <- function(df) {
  df_count <- df %>%
    mutate(all=class_worker!="",
           self_employed=class_worker=="Self-Employed",
           private=class_worker=="Private",
           public_all=class_worker=="Public-All",
           public_federal=class_worker=="Public-Federal",
           public_state=class_worker=="Public-State",
           public_local=class_worker=="Public-Local",
           
           # Impute public total beyond 1988
           public_all=ifelse(public_all |
                               public_federal |
                               public_state |
                               public_local,
                             TRUE,
                             FALSE)) %>%
    group_by(year) %>%
    summarise(all=sum(all*sample_weight, na.rm=TRUE),
              private=sum(private*sample_weight, na.rm=TRUE),
              public_all=sum(public_all*sample_weight, na.rm=TRUE),
              public_federal=sum(public_federal*sample_weight, na.rm=TRUE),
              public_state=sum(public_state*sample_weight, na.rm=TRUE),
              public_local=sum(public_local*sample_weight, na.rm=TRUE)) %>%
    mutate(`Public-All`=public_all/all,
           `Public-Federal`=public_federal/all,
           `Public-State`=public_state/all,
           `Public-Local`=public_local/all,
           `Public-Federal`=ifelse(`Public-Federal`==0, NA, `Public-Federal`),
           `Public-State`=ifelse(`Public-State`==0, NA, `Public-State`),
           `Public-Local`=ifelse(`Public-Local`==0, NA, `Public-Local`)) %>%
    select(year,
           `Public-All`,
           `Public-Federal`,
           `Public-State`,
           `Public-Local`) %>%
    gather(key="sector", value="fraction", -year) %>%
    ungroup
  
  return(df_count)
}

# Graph
ggplot(employee_count(cps_analysis),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=2007, y=0.185,
                label="All Public",
                colour="Public-All"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.105,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.06,
                label="State",
                colour="Public-State"),
            size=4, family="serif") +
  geom_text(aes(x=2006, y=0.0365,
                label="Federal",
                colour="Public-Federal"),
            size=4, family="serif") +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set)
ggsave("Result/share_public.pdf", width=6, height=4)

ggplot(employee_count(cps_analysis %>% filter(sex=="Male")),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=2007, y=0.12,
                label="All Public",
                colour="Public-All"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.08,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.04375,
                label="State",
                colour="Public-State"),
            size=4, family="serif") +
  geom_text(aes(x=1990, y=0.03,
                label="Federal",
                colour="Public-Federal"),
            size=4, family="serif") +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set)
ggsave("Result/share_public_male.pdf", width=6, height=4)

ggplot(employee_count(cps_analysis %>% filter(sex=="Female")),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=2007, y=0.185,
                label="All Public",
                colour="Public-All"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.1325,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.075,
                label="State",
                colour="Public-State"),
            size=4, family="serif") +
  geom_text(aes(x=2006, y=0.0375,
                label="Federal",
                colour="Public-Federal"),
            size=4, family="serif") +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set)
ggsave("Result/share_public_female.pdf", width=6, height=4)


# Share of female in each sector
female_count <- cps_analysis %>%
  mutate(all=sex!="",
         female=sex=="Female",
         sector=ifelse(class_worker=="Private",
                       "Private",
                       ifelse(class_worker %in% c("Public-All",
                                                  "Public-Federal",
                                                  "Public-State",
                                                  "Public-Local"),
                              "Public",
                              NA))) %>%
  group_by(year, sector) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            female=sum(female*sample_weight, na.rm=TRUE)) %>%
  mutate(female_fraction=female/all) %>%
  filter(!is.na(sector)) %>%
  ungroup

female_count_government <- cps_analysis %>%
  mutate(all=sex!="",
         female=sex=="Female",
         sector=class_worker) %>%
  filter(sector %in% c("Public-Federal",
                       "Public-State",
                       "Public-Local")) %>%
  group_by(year, sector) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            female=sum(female*sample_weight, na.rm=TRUE)) %>%
  mutate(female_fraction=female/all) %>%
  ungroup

female_count <- rbind(female_count, female_count_government)

ggplot(female_count,
       aes(x=year, y=female_fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=1970, y=0.425,
                label="All Public",
                colour="Public"),
            size=4, family="serif") +
  geom_text(aes(x=1970, y=0.255,
                label="Private",
                colour="Private"),
            size=4, family="serif") +
  geom_text(aes(x=1995, y=0.375,
                label="Federal",
                colour="Public-Federal"),
            size=4, family="serif") +
  geom_text(aes(x=1995, y=0.485,
                label="State",
                colour="Public-State"),
            size=4, family="serif") +
  geom_text(aes(x=1995, y=0.58,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set)
ggsave("Result/share_female.pdf", width=6, height=4)


# Mean log earnings and standard deviation (naive approach) ####
# Mean log earnings
mean_earnings <- function(df) {
  df_mean_earnings <- df %>%
    mutate(sector=ifelse(class_worker=="Private",
                         "Private",
                         ifelse(class_worker %in% c("Public-All",
                                                    "Public-Federal",
                                                    "Public-State",
                                                    "Public-Local"),
                                "Public",
                                NA))) %>%
    filter(!is.na(sector)) %>%
    group_by(year, sector) %>%
    summarise(mean_earnings=weighted.mean(log_wk_wage99, sample_weight, na.rm=TRUE)) %>%
    ungroup
  
  df_mean_earnings_all <- df %>%
    filter(class_worker %in% c("Private",
                               "Public-All",
                               "Public-Federal",
                               "Public-State",
                               "Public-Local")) %>%
    group_by(year) %>%
    summarise(mean_earnings=weighted.mean(log_wk_wage99, sample_weight, na.rm=TRUE)) %>%
    mutate(sector="All") %>%
    ungroup
  
  df_mean_earnings <- rbind(df_mean_earnings, df_mean_earnings_all)
  return(df_mean_earnings)
}

ggplot(mean_earnings(cps_analysis),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings.pdf", width=6, height=4)

ggplot(mean_earnings(cps_analysis %>% filter(sex=="Male")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_male.pdf", width=6, height=4)

ggplot(mean_earnings(cps_analysis %>% filter(sex=="Female")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_female.pdf", width=6, height=4)

# Standard deviations of log earnings
sd_earnings <- function(df) {
  df_sd_earnings <- df %>%
    mutate(sector=ifelse(class_worker=="Private",
                         "Private",
                         ifelse(class_worker %in% c("Public-All",
                                                    "Public-Federal",
                                                    "Public-State",
                                                    "Public-Local"),
                                "Public",
                                NA))) %>%
    filter(!is.na(sector)) %>%
    group_by(year, sector) %>%
    summarise(sd_earnings=weighted.sd(log_wk_wage99, sample_weight, na.rm=TRUE)) %>%
    ungroup
  
  df_sd_earnings_all <- df %>%
    filter(class_worker %in% c("Private",
                               "Public-All",
                               "Public-Federal",
                               "Public-State",
                               "Public-Local")) %>%
    group_by(year) %>%
    summarise(sd_earnings=weighted.sd(log_wk_wage99, sample_weight, na.rm=TRUE)) %>%
    mutate(sector="All") %>%
    ungroup
  
  df_sd_earnings <- rbind(df_sd_earnings, df_sd_earnings_all)
  return(df_sd_earnings)
}

ggplot(sd_earnings(cps_analysis),
       aes(x=year, y=sd_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Standard Deviation") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.15, 0.85),
        legend.title=element_blank())
ggsave("Result/sd_log_earnings.pdf", width=6, height=4)

ggplot(sd_earnings(cps_analysis %>% filter(sex=="Male")),
       aes(x=year, y=sd_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Standard Deviation Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.15, 0.75),
        legend.title=element_blank())
ggsave("Result/sd_log_earnings_male.pdf", width=6, height=4)

ggplot(sd_earnings(cps_analysis %>% filter(sex=="Female")),
       aes(x=year, y=sd_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Standard Deviation Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.15, 0.825),
        legend.title=element_blank())
ggsave("Result/sd_log_earnings_female.pdf", width=6, height=4)


# Mean log earnings (predicted earnings) ####
cps_analysis_adjust <- cps_analysis %>%
  mutate(r=row_number())

cps_analysis_adjust <- cps_analysis_adjust %>%
  select(r, region) %>%
  mutate(i=1) %>%
  spread(region, i, fill=0, sep="_") %>%
  left_join(cps_analysis_adjust, by="r") %>%
  mutate(sector=ifelse(class_worker=="Private",
                       "Private",
                       ifelse(class_worker %in% c("Public-All",
                                                  "Public-Federal",
                                                  "Public-State",
                                                  "Public-Local"),
                              "Public",
                              NA)),
         race_binary=ifelse(race=="White", 1,
                            ifelse(race=="Black", 0, NA)),
         sex_binary=ifelse(sex=="Male", 1,
                           ifelse(sex=="Female", 0, NA)),
         sector_binary=ifelse(sector=="Public", 1,
                              ifelse(sector=="Private", 0, NA))) %>%
  filter(!is.na(sector),
         # Somehow 1963 is missing education
         year != 1963)

# Create person with average characteristics
average_person <- cps_analysis_adjust %>%
  summarise(sex_binary=weighted.mean(sex_binary, sample_weight, na.rm=TRUE),
            education=weighted.mean(education, sample_weight, na.rm=TRUE),
            age=weighted.mean(age, sample_weight, na.rm=TRUE),
            race_binary=weighted.mean(race_binary, sample_weight, na.rm=TRUE),
            region_new_england=weighted.mean(region_new_england, sample_weight, na.rm=TRUE),
            region_middle_atlantic=weighted.mean(region_middle_atlantic, sample_weight, na.rm=TRUE),
            region_south_atlantic=weighted.mean(region_south_atlantic, sample_weight, na.rm=TRUE),
            region_east_north_central=weighted.mean(region_east_north_central, sample_weight, na.rm=TRUE),
            region_east_south_central=weighted.mean(region_east_south_central, sample_weight, na.rm=TRUE),
            region_west_north_central=weighted.mean(region_west_north_central, sample_weight, na.rm=TRUE),
            region_west_south_central=weighted.mean(region_west_south_central, sample_weight, na.rm=TRUE),
            region_mountain=weighted.mean(region_mountain, sample_weight, na.rm=TRUE)) %>%
  mutate(predict_log_wk_wage99=NA)

average_person_sex <- cps_analysis_adjust %>%
  group_by(sex) %>%
  summarise(sex_binary=weighted.mean(sex_binary, sample_weight, na.rm=TRUE),
            education=weighted.mean(education, sample_weight, na.rm=TRUE),
            age=weighted.mean(age, sample_weight, na.rm=TRUE),
            race_binary=weighted.mean(race_binary, sample_weight, na.rm=TRUE),
            region_new_england=weighted.mean(region_new_england, sample_weight, na.rm=TRUE),
            region_middle_atlantic=weighted.mean(region_middle_atlantic, sample_weight, na.rm=TRUE),
            region_south_atlantic=weighted.mean(region_south_atlantic, sample_weight, na.rm=TRUE),
            region_east_north_central=weighted.mean(region_east_north_central, sample_weight, na.rm=TRUE),
            region_east_south_central=weighted.mean(region_east_south_central, sample_weight, na.rm=TRUE),
            region_west_north_central=weighted.mean(region_west_north_central, sample_weight, na.rm=TRUE),
            region_west_south_central=weighted.mean(region_west_south_central, sample_weight, na.rm=TRUE),
            region_mountain=weighted.mean(region_mountain, sample_weight, na.rm=TRUE)) %>%
  mutate(predict_log_wk_wage99=NA) %>%
  ungroup

average_person_tex <- average_person %>%
  rename(`Percent Male`=sex_binary,
         `Education`=education,
         `Age`=age,
         `Percent White`=race_binary,
         `Region - New England`=region_new_england,
         `Region - Middle Atlantic`=region_middle_atlantic,
         `Region - South Atlantic`=region_south_atlantic,
         `Region - East North Central`=region_east_north_central,
         `Region - East South Central`=region_east_south_central,
         `Region - West North Central`=region_west_north_central,
         `Region - West South Central`=region_west_south_central,
         `Region - Mountain`=region_mountain) %>%
  select(-predict_log_wk_wage99) %>%
  gather()

print(xtable(average_person_tex, digits=3),
      file="Result/average_person.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

average_person_male_tex <- average_person_sex %>%
  filter(sex=="Male") %>%
  rename(`Education`=education,
         `Age`=age,
         `Percent White`=race_binary,
         `Region - New England`=region_new_england,
         `Region - Middle Atlantic`=region_middle_atlantic,
         `Region - South Atlantic`=region_south_atlantic,
         `Region - East North Central`=region_east_north_central,
         `Region - East South Central`=region_east_south_central,
         `Region - West North Central`=region_west_north_central,
         `Region - West South Central`=region_west_south_central,
         `Region - Mountain`=region_mountain) %>%
  select(-predict_log_wk_wage99, -sex, -sex_binary) %>%
  gather()

print(xtable(average_person_male_tex, digits=3),
      file="Result/average_person_male.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

average_person_female_tex <- average_person_sex %>%
  filter(sex=="Female") %>%
  rename(`Education`=education,
         `Age`=age,
         `Percent White`=race_binary,
         `Region - New England`=region_new_england,
         `Region - Middle Atlantic`=region_middle_atlantic,
         `Region - South Atlantic`=region_south_atlantic,
         `Region - East North Central`=region_east_north_central,
         `Region - East South Central`=region_east_south_central,
         `Region - West North Central`=region_west_north_central,
         `Region - West South Central`=region_west_south_central,
         `Region - Mountain`=region_mountain) %>%
  select(-predict_log_wk_wage99, -sex, -sex_binary) %>%
  gather()

print(xtable(average_person_female_tex, digits=3),
      file="Result/average_person_female.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

cps_character_adjust <- NULL
cps_character_adjust_sex <- NULL

for (y in unique(cps_analysis_adjust$year)) {
  for (p in unique(cps_analysis_adjust$sector)) {
    df_sample <- cps_analysis_adjust %>%
      filter(year==y & sector==p)
    
    fit <- lm(log_wk_wage99~sex_binary+
                education+
                age+
                race_binary+
                region_new_england+
                region_middle_atlantic+
                region_south_atlantic+
                region_east_north_central+
                region_east_south_central+
                region_west_north_central+
                region_west_south_central+
                region_mountain,
              data=df_sample,
              weights=sample_weight)
    
    cps_character_adjust <-
      rbind(cps_character_adjust,
            data.frame(year=y,
                       sector=p,
                       predict_log_wk_wage99=
                         as.numeric(predict(fit, average_person))))
  }
}

for (y in unique(cps_analysis_adjust$year)) {
  for (p in unique(cps_analysis_adjust$sector)) {
    for (s in unique(cps_analysis_adjust$sex)) {
      df_sample <- cps_analysis_adjust %>%
        filter(year==y & sector==p & sex==s)
      
      fit <- lm(log_wk_wage99~education+
                  age+
                  race_binary+
                  region_new_england+
                  region_middle_atlantic+
                  region_south_atlantic+
                  region_east_north_central+
                  region_east_south_central+
                  region_west_north_central+
                  region_west_south_central+
                  region_mountain,
                data=df_sample,
                weights=sample_weight)
      
      cps_character_adjust_sex <-
        rbind(cps_character_adjust_sex,
              data.frame(year=y,
                         sector=p,
                         sex=s,
                         predict_log_wk_wage99=
                           as.numeric(predict(fit, average_person_sex %>%
                                                filter(sex==s)))))
    }
  }
}

ggplot(cps_character_adjust,
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_adjusted.pdf", width=6, height=4)

ggplot(cps_character_adjust_sex %>% filter(sex=="Male"),
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.75),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_male_adjusted.pdf", width=6, height=4)

ggplot(cps_character_adjust_sex %>% filter(sex=="Female"),
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_female_adjusted.pdf", width=6, height=4)


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
      file="Result/occupation_proportion.tex",
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
      file="Result/occupation_proportion_male.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

print(xtable(cps_analysis_occupation_fraction_female,
             digits=c(0, 0, 3, 3, 3, 3, 3, 3, 3)),
      file="Result/occupation_proportion_female.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

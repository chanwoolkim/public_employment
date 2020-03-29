# cps_analysis_predict.R
# Earnings for comparable workers


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

average_person_race_sex <- cps_analysis_adjust %>%
  group_by(race, sex) %>%
  summarise(education=weighted.mean(education, sample_weight, na.rm=TRUE),
            age=weighted.mean(age, sample_weight, na.rm=TRUE),
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
      file="result/average_person.tex",
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
      file="result/average_person_male.tex",
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
      file="result/average_person_female.tex",
      floating=FALSE, comment=FALSE, timestamp=NULL,
      include.rownames=FALSE)

average_person_race_sex_tex <- average_person_race_sex %>%
  rename(`Race`=race,
         `Sex`=sex,
         `Education`=education,
         `Age`=age,
         `Region - New England`=region_new_england,
         `Region - Middle Atlantic`=region_middle_atlantic,
         `Region - South Atlantic`=region_south_atlantic,
         `Region - East North Central`=region_east_north_central,
         `Region - East South Central`=region_east_south_central,
         `Region - West North Central`=region_west_north_central,
         `Region - West South Central`=region_west_south_central,
         `Region - Mountain`=region_mountain)

print(xtable(average_person_race_sex_tex, digits=3),
      file="result/average_person_race_sex.tex",
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
ggsave("result/mean_log_earnings_adjusted.png", width=6, height=4)

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
ggsave("result/mean_log_earnings_male_adjusted.png", width=6, height=4)

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
ggsave("result/mean_log_earnings_female_adjusted.png", width=6, height=4)


# Using average person characteristics but altering race and sex
average_person_white_male <- average_person_race_sex %>%
  filter(race=="White", sex=="Male") %>%
  mutate(race_binary=1, sex_binary=1) %>%
  select(-race, -sex)
cps_character_adjust_white_male <- NULL

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
    
    cps_character_adjust_white_male <-
      rbind(cps_character_adjust_white_male,
            data.frame(year=y,
                       sector=p,
                       predict_log_wk_wage99=
                         as.numeric(predict(fit, average_person_white_male))))
  }
}

ggplot(cps_character_adjust_white_male,
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.7),
        legend.title=element_blank())
ggsave("result/mean_log_earnings_adjusted_white_male.png", width=6, height=4)

average_person_white_female <- average_person_race_sex %>%
  filter(race=="White", sex=="Female") %>%
  mutate(race_binary=1, sex_binary=0) %>%
  select(-race, -sex)
cps_character_adjust_white_female <- NULL

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
    
    cps_character_adjust_white_female <-
      rbind(cps_character_adjust_white_female,
            data.frame(year=y,
                       sector=p,
                       predict_log_wk_wage99=
                         as.numeric(predict(fit, average_person_white_female))))
  }
}

ggplot(cps_character_adjust_white_female,
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("result/mean_log_earnings_adjusted_white_female.png", width=6, height=4)

average_person_black_male <- average_person_race_sex %>%
  filter(race=="Black", sex=="Male") %>%
  mutate(race_binary=1, sex_binary=1) %>%
  select(-race, -sex)
cps_character_adjust_black_male <- NULL

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
    
    cps_character_adjust_black_male <-
      rbind(cps_character_adjust_black_male,
            data.frame(year=y,
                       sector=p,
                       predict_log_wk_wage99=
                         as.numeric(predict(fit, average_person_black_male))))
  }
}

ggplot(cps_character_adjust_black_male,
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.7),
        legend.title=element_blank())
ggsave("result/mean_log_earnings_adjusted_black_male.png", width=6, height=4)

average_person_black_female <- average_person_race_sex %>%
  filter(race=="Black", sex=="Female") %>%
  mutate(race_binary=1, sex_binary=0) %>%
  select(-race, -sex)
cps_character_adjust_black_female <- NULL

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
    
    cps_character_adjust_black_female <-
      rbind(cps_character_adjust_black_female,
            data.frame(year=y,
                       sector=p,
                       predict_log_wk_wage99=
                         as.numeric(predict(fit, average_person_black_female))))
  }
}

ggplot(cps_character_adjust_black_female,
       aes(x=year, y=predict_log_wk_wage99, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage (Adjusted)") +
  scale_color_manual(values=colours_set[2:3]) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("result/mean_log_earnings_adjusted_black_female.png", width=6, height=4)

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
         child_support99=child_support*cpi99,
         log_wage99=log(wage99),
         experience=pmax(0,
                         age-education-6),
         wk_wage99=wage99/(work_wk),
         log_wk_wage99=log(wk_wage99),
         hr_wage99=wage99/(work_hrs_wk*work_wk),
         log_hr_wage99=log(hr_wage99))

# Filter to those with half the minimum wage in 1990 dollar
cps_analysis <- cps_analysis %>%
  filter(wk_wage99>=103)

count(cps_analysis) #2472968


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
ggsave("result/share_public.png", width=6, height=4)

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
ggsave("result/share_public_male.png", width=6, height=4)

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
ggsave("result/share_public_female.png", width=6, height=4)

ggplot(employee_count(cps_analysis %>% filter(race=="White")),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=2007, y=0.18,
                label="All Public",
                colour="Public-All"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.075,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.055,
                label="State",
                colour="Public-State"),
            size=4, family="serif") +
  geom_text(aes(x=2006, y=0.035,
                label="Federal",
                colour="Public-Federal"),
            size=4, family="serif") +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set)
ggsave("result/share_public_white.png", width=6, height=4)

ggplot(employee_count(cps_analysis %>% filter(race=="Black")),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=2007, y=0.245,
                label="All Public",
                colour="Public-All"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.1325,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  geom_text(aes(x=2005, y=0.08,
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
ggsave("result/share_public_black.png", width=6, height=4)


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
ggsave("result/share_female.png", width=6, height=4)


# Share of black in each sector
black_count <- cps_analysis %>%
  mutate(all=race!="",
         black=race=="Black",
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
            black=sum(black*sample_weight, na.rm=TRUE)) %>%
  mutate(black_fraction=black/all) %>%
  filter(!is.na(sector)) %>%
  ungroup

black_count_government <- cps_analysis %>%
  mutate(all=race!="",
         black=race=="Black",
         sector=class_worker) %>%
  filter(sector %in% c("Public-Federal",
                       "Public-State",
                       "Public-Local")) %>%
  group_by(year, sector) %>%
  summarise(all=sum(all*sample_weight, na.rm=TRUE),
            black=sum(black*sample_weight, na.rm=TRUE)) %>%
  mutate(black_fraction=black/all) %>%
  ungroup

black_count <- rbind(black_count, black_count_government)

ggplot(black_count,
       aes(x=year, y=black_fraction, group=sector, colour=sector)) +
  geom_line() +
  geom_text(aes(x=1970, y=0.1425,
                label="All Public",
                colour="Public"),
            size=4, family="serif") +
  geom_text(aes(x=1970, y=0.075,
                label="Private",
                colour="Private"),
            size=4, family="serif") +
  geom_text(aes(x=2010, y=0.235,
                label="Federal",
                colour="Public-Federal"),
            size=4, family="serif") +
  geom_text(aes(x=2010, y=0.1725,
                label="State",
                colour="Public-State"),
            size=4, family="serif") +
  geom_text(aes(x=2010, y=0.13,
                label="Local",
                colour="Public-Local"),
            size=4, family="serif") +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set)
ggsave("result/share_black.png", width=6, height=4)


# Difference in characteristics between sectors ####
# Pension
pension_count <- function(df) {
  df_count <- df %>%
    filter(year>=1980) %>%
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
    summarise(all_pension=sum(all*pension*sample_weight, na.rm=TRUE),
              private_pension=sum(private*pension*sample_weight, na.rm=TRUE),
              public_all_pension=sum(public_all*pension*sample_weight, na.rm=TRUE),
              public_federal_pension=sum(public_federal*pension*sample_weight, na.rm=TRUE),
              public_state_pension=sum(public_state*pension*sample_weight, na.rm=TRUE),
              public_local_pension=sum(public_local*pension*sample_weight, na.rm=TRUE),
              all=sum(all*sample_weight, na.rm=TRUE),
              private=sum(private*sample_weight, na.rm=TRUE),
              public_all=sum(public_all*sample_weight, na.rm=TRUE),
              public_federal=sum(public_federal*sample_weight, na.rm=TRUE),
              public_state=sum(public_state*sample_weight, na.rm=TRUE),
              public_local=sum(public_local*sample_weight, na.rm=TRUE)) %>%
    mutate(`All`=all_pension/all,
           `Private`=private_pension/private,
           `Public-All`=public_all_pension/public_all,
           `Public-Federal`=public_federal_pension/public_federal,
           `Public-State`=public_state_pension/public_state,
           `Public-Local`=public_local_pension/public_local,
           `All`=ifelse(`All`==0, NA, `All`),
           `Private`=ifelse(`Private`==0, NA, `Private`),
           `Public-All`=ifelse(`Public-All`==0, NA, `Public-All`),
           `Public-Federal`=ifelse(`Public-Federal`==0, NA, `Public-Federal`),
           `Public-State`=ifelse(`Public-State`==0, NA, `Public-State`),
           `Public-Local`=ifelse(`Public-Local`==0, NA, `Public-Local`)) %>%
    select(year,
           `All`,
           `Private`,
           `Public-All`,
           `Public-Federal`,
           `Public-State`,
           `Public-Local`) %>%
    gather(key="sector", value="fraction", -year) %>%
    ungroup
  
  return(df_count)
}

ggplot(pension_count(cps_analysis),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set) +
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("result/share_pension.png", width=6, height=6)


# Healthcare
health_count <- function(df) {
  df_count <- df %>%
    filter(year>=1996 & year<=2013) %>%
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
    summarise(all_health=sum(all*health_plan*sample_weight, na.rm=TRUE),
              private_health=sum(private*health_plan*sample_weight, na.rm=TRUE),
              public_all_health=sum(public_all*health_plan*sample_weight, na.rm=TRUE),
              public_federal_health=sum(public_federal*health_plan*sample_weight, na.rm=TRUE),
              public_state_health=sum(public_state*health_plan*sample_weight, na.rm=TRUE),
              public_local_health=sum(public_local*health_plan*sample_weight, na.rm=TRUE),
              all=sum(all*sample_weight, na.rm=TRUE),
              private=sum(private*sample_weight, na.rm=TRUE),
              public_all=sum(public_all*sample_weight, na.rm=TRUE),
              public_federal=sum(public_federal*sample_weight, na.rm=TRUE),
              public_state=sum(public_state*sample_weight, na.rm=TRUE),
              public_local=sum(public_local*sample_weight, na.rm=TRUE)) %>%
    mutate(`All`=all_health/all,
           `Private`=private_health/private,
           `Public-All`=public_all_health/public_all,
           `Public-Federal`=public_federal_health/public_federal,
           `Public-State`=public_state_health/public_state,
           `Public-Local`=public_local_health/public_local,
           `All`=ifelse(`All`==0, NA, `All`),
           `Private`=ifelse(`Private`==0, NA, `Private`),
           `Public-All`=ifelse(`Public-All`==0, NA, `Public-All`),
           `Public-Federal`=ifelse(`Public-Federal`==0, NA, `Public-Federal`),
           `Public-State`=ifelse(`Public-State`==0, NA, `Public-State`),
           `Public-Local`=ifelse(`Public-Local`==0, NA, `Public-Local`)) %>%
    select(year,
           `All`,
           `Private`,
           `Public-All`,
           `Public-Federal`,
           `Public-State`,
           `Public-Local`) %>%
    gather(key="sector", value="fraction", -year) %>%
    ungroup
  
  return(df_count)
}

ggplot(health_count(cps_analysis),
       aes(x=year, y=fraction, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Percent",
                     labels=scales::percent_format(accuracy=1)) +
  scale_color_manual(values=colours_set) +
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("result/share_health_plan.png", width=6, height=6)

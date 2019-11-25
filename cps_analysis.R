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
  filter(wage99>=76)

count(cps_analysis) #2513473


# Share of public sector employees ####
employee_count <- function(df) {
  df_count <- df %>%
    mutate(all=TRUE,
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
    summarise(all=sum(all*sample_weight),
              private=sum(private*sample_weight),
              public_all=sum(public_all*sample_weight),
              public_federal=sum(public_federal*sample_weight),
              public_state=sum(public_state*sample_weight),
              public_local=sum(public_local*sample_weight)) %>%
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
    gather(key="sector", value="fraction", -year)
  
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
ggsave("Result/share_public.pdf",width=6, height=4)

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
ggsave("Result/share_public_male.pdf",width=6, height=4)

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
ggsave("Result/share_public_female.pdf",width=6, height=4)


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
    summarise(mean_earnings=weighted.mean(log_wk_wage99, sample_weight)) %>%
    ungroup
  
  df_mean_earnings_all <- df %>%
    filter(class_worker %in% c("Private",
                               "Public-All",
                               "Public-Federal",
                               "Public-State",
                               "Public-Local")) %>%
    group_by(year) %>%
    summarise(mean_earnings=weighted.mean(log_wk_wage99, sample_weight)) %>%
    mutate(sector="All")
  
  df_mean_earnings <- rbind(df_mean_earnings, df_mean_earnings_all)
  return(df_mean_earnings)
}

ggplot(mean_earnings(cps_analysis),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings.pdf",width=6, height=4)

ggplot(mean_earnings(cps_analysis %>% filter(sex=="Male")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_male.pdf",width=6, height=4)

ggplot(mean_earnings(cps_analysis %>% filter(sex=="Female")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_female.pdf",width=6, height=4)

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
    summarise(sd_earnings=weighted.sd(log_wk_wage99, sample_weight)) %>%
    ungroup
  
  df_sd_earnings_all <- df %>%
    filter(class_worker %in% c("Private",
                               "Public-All",
                               "Public-Federal",
                               "Public-State",
                               "Public-Local")) %>%
    group_by(year) %>%
    summarise(sd_earnings=weighted.sd(log_wk_wage99, sample_weight)) %>%
    mutate(sector="All")
  
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
ggsave("Result/sd_log_earnings.pdf",width=6, height=4)

ggplot(sd_earnings(cps_analysis %>% filter(sex=="Male")),
       aes(x=year, y=sd_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Standard Deviation Log Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.15, 0.75),
        legend.title=element_blank())
ggsave("Result/sd_log_earnings_male.pdf",width=6, height=4)

ggplot(sd_earnings(cps_analysis %>% filter(sex=="Female")),
       aes(x=year, y=sd_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Standard Deviation Log Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.15, 0.825),
        legend.title=element_blank())
ggsave("Result/sd_log_earnings_female.pdf",width=6, height=4)


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
                              NA))) %>%
  filter(!is.na(sector))

# Somehow 1963 has missing education
cps_analysis_adjust <- cps_analysis_adjust %>%
  filter(year != 1963) %>%
  group_by(year, sex, sector) %>%
  mutate(predict_log_wk_wage99=
           as.numeric(fitted(lm(log_wk_wage99~education+
                                  age+
                                  race+
                                  region_new_england+
                                  region_middle_atlantic+
                                  region_south_atlantic+
                                  region_east_north_central+
                                  region_east_south_central+
                                  region_west_north_central+
                                  region_west_south_central+
                                  region_mountain,
                                weights=sample_weight,
                                na.action=na.exclude)))) %>%
  ungroup

mean_earnings_adjust <- function(df) {
  df_mean_earnings_adjust <- df %>%
    group_by(year, sector) %>%
    summarise(mean_earnings_adjust=
                weighted.mean(predict_log_wk_wage99,
                              sample_weight,
                              na.rm=TRUE)) %>%
    ungroup
  
  df_mean_earnings_adjust_all <- df %>%
    group_by(year) %>%
    summarise(mean_earnings_adjust=
                weighted.mean(predict_log_wk_wage99,
                              sample_weight,
                              na.rm=TRUE)) %>%
    mutate(sector="All")
  
  df_mean_earnings_adjust <- rbind(df_mean_earnings_adjust,
                                   df_mean_earnings_adjust_all)
  return(df_mean_earnings_adjust)
}

ggplot(mean_earnings_adjust(cps_analysis_adjust),
       aes(x=year, y=mean_earnings_adjust, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Wage (Adjusted)") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_adjust.pdf",width=6, height=4)

ggplot(mean_earnings(cps_analysis_adjust %>% filter(sex=="Male")),
       aes(x=year, y=mean_earnings_adjust, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Wage (Adjusted)") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_adjust_male.pdf",width=6, height=4)

ggplot(mean_earnings(cps_analysis_adjust %>% filter(sex=="Female")),
       aes(x=year, y=mean_earnings_adjust, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Wage (Adjusted)") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_adjust_female.pdf",width=6, height=4)
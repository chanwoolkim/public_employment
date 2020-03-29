# cps_analysis_earnings.R
# Earnings average

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

ggplot(mean_earnings(cps_analysis %>% filter(race=="White" & sex=="Male")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_white_male.pdf", width=6, height=4)

ggplot(mean_earnings(cps_analysis %>% filter(race=="Black" & sex=="Male")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.2),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_black_male.pdf", width=6, height=4)

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

ggplot(mean_earnings(cps_analysis %>% filter(race=="White" & sex=="Female")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_white_female.pdf", width=6, height=4)

ggplot(mean_earnings(cps_analysis %>% filter(race=="Black" & sex=="Female")),
       aes(x=year, y=mean_earnings, group=sector, colour=sector)) +
  geom_line() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Log Weekly Wage") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())
ggsave("Result/mean_log_earnings_black_female.pdf", width=6, height=4)

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

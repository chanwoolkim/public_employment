N <- 10000
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

z <- mvrnorm(N, c(0, 0), Sigma)

sigma1 <- 10
eps1 <- z[,1]*sigma1

sigma2 <- 10
eps2 <- z[,2]*sigma2

mu1 <- 100
skill1 <- mu1+eps1

mu2 <- mu1
skill2 <- mu2+eps2

sort <- data.frame(skill1=skill1, skill2=skill2) %>%
  mutate(w1=skill1,
         w2=skill2,
         avgskill=(skill1+skill2)/2,
         rank_avgskill=rank(-avgskill),
         diffskill=abs(skill1-skill2),
         rank_diffskill=rank(diffskill),
         overall_rank=rank(rank_avgskill+rank_diffskill,
                           ties.method="random"))

sort <- sort %>%
  mutate(rank_top=rank_avgskill<=1500)

ggplot(sort,
       aes(x=skill1, y=skill2, group=rank_top, colour=rank_top)) +
  geom_point() +
  fte_theme() +
  labs(colour="RANK") +
  scale_x_continuous(name="Skill 1") +
  scale_y_continuous(name="Skill 2") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())


wage_g <- data.frame(w1=sort(skill1), w2=sort(skill2)) %>%
  mutate(wage_g=sort((w1+w2)/2),
         r=row_number()) %>%
  select(-w1, -w2)

sort <- sort %>%
  mutate(wg=wage_g[overall_rank,]$wage_g,
         sector=ifelse(w1>w2 & w1>wg, "Private-1",
                       ifelse(w2>w1 & w2>wg, "Private-2",
                              ifelse(wg>w1 & wg>w2, "Public", NA))))

ggplot(sort,
       aes(x=skill1, y=skill2, group=sector, colour=sector)) +
  geom_point() +
  fte_theme() +
  labs(colour="Sector") +
  scale_x_continuous(name="Skill 1") +
  scale_y_continuous(name="Skill 2") +
  scale_color_manual(values=colours_set) +
  theme(legend.position=c(0.85, 0.275),
        legend.title=element_blank())

single_day_dat <- cumul_cases %>%
  group_by(name) %>%
  filter(ymd == last_date, pop >= 2000) %>%
  select(ymd, name, new_cases_7_rate_100k) %>%
  arrange(new_cases_7_rate_100k)

tier_clrs <- list(
  yellow = rgb(250, 241, 215, maxColorValue = 255),
  orange = rgb(247, 228, 217, maxColorValue = 255),
  red =    rgb(243, 216, 221, maxColorValue = 255),
  purple = rgb(230, 213, 225, maxColorValue = 255),
  beyond = rgb(148, 47, 103, maxColorValue = 255)
)
anno_text_clr <- 'grey30'

# plot 1 ----
single_day_dat %>%
  ggplot(aes(x=fct_inorder(name), y=new_cases_7_rate_100k)) +
  # tiers
  geom_rect(ymin = 0, ymax = 1, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$yellow) +
  geom_rect(ymin = 1, ymax = 4, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$orange) +
  geom_rect(ymin = 4, ymax = 7, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$red) +
  geom_rect(ymin = 7, ymax = 25, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$purple) +
  geom_rect(ymin = 25, ymax = 30, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$beyond) +
  geom_col(fill='grey50') +
  geom_hline(yintercept = c(0, 1, 4, 7, 25), color='grey50') +
  geom_text(data = tibble(
    x = c(-2, -1, -2, -1),
    y = c(0, 1, 4, 7),
    label = c('Yellow*', 'Orange*', 'Red*', 'Purple*')),
    aes(x, y, label=label),
    hjust=-0.1, vjust=-2, size=3, color=anno_text_clr) +
  geom_text(data = tibble(
    x = c(-2),
    y = c(25),
    label = c('Public Health: TK-6 in person**')),
    aes(x, y, label=label),
    hjust=1, vjust=-2, size=3, color=anno_text_clr) +
  coord_flip() +
  labs(
    title = glue('OUSD area COVID cases by zip'),
    subtitle = glue('Data for {last_date}'),
    y='7 day average new cases per 100k',
    x='ZIP',
    caption = glue('*CA DPH Tiers (https://covid19.ca.gov/safer-economy)\n',
                   '**CA DPH guidance for TK-6 reopening, 5 consec. days (http://bit.ly/ca_school_reopen, p8)')) +
  scale_y_continuous(breaks=seq(0,30,5)) +
  theme_light() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.caption = element_text(color='grey40'))
ggsave('images/ousd_zip_tiers.jpg', w=6, h=4, dpi=300)


# tiers, last two weeks ----
cumul_cases %>%
  filter(pop >= 2000 & ymd >= last_date - 14) %>%
  ggplot() +
  
  # tiers
  geom_rect(ymin = 0, ymax = 1, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$yellow) +
  geom_rect(ymin = 1, ymax = 4, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$orange) +
  geom_rect(ymin = 4, ymax = 7, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$red) +
  geom_rect(ymin = 7, ymax = 25, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$purple) +
  geom_rect(ymin = 25, ymax = 30, xmin =-Inf, xmax= Inf,
            fill = tier_clrs$beyond) +
  
  # data
  geom_line(aes(x=ymd, y=new_cases_7_rate_100k, group=name),
            color = 'grey50', alpha=0.7) +
  
  # annotation
  geom_text(data = tibble(
    x = last_date + rep(1, 5),
    y = c(mean(c(0,1)), 
          mean(c(1,4)), 
          mean(c(4,7)),
          mean(c(7,25)), 
          mean(c(25,30))
          ),
    label = c('Yellow', 'Orange', 'Red', 'Purple', "No TK-6\nin person")),
    aes(x, y, label=label),
    fontface='italic', 
    size=3, color='black') +
  labs(
    title = glue('OUSD-area COVID cases by zip vs CA Public Health Tiers'),
    subtitle = glue('Two weeks through {last_date}'),
    y='7 day average new cases per 100k',
    x='ZIP',
    caption = glue('CA DPH Tiers (https://covid19.ca.gov/safer-economy)\n',
                   'CA DPH guidance for TK-6 reopening, 5 consec. days (http://bit.ly/ca_school_reopen, p8)')) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c(0, 30)) +
  theme_light() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.caption = element_text(color='grey40'))
ggsave('images/ousd_zip_tiers_14days.jpg', w=6, h=4, dpi=300)

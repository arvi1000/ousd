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

tier_thresh <- list(
  yellow = c(0, 2),
  orange = c(2, 6),
  red = c(6, 10),
  purple = c(10, 25),
  beyond = c(25, 30)
)

# plot 1 ----
single_day_dat %>%
  ggplot(aes(x=fct_inorder(name), y=new_cases_7_rate_100k)) +
  # tiers
  geom_rect(ymin = tier_thresh$yellow[1], ymax = tier_thresh$yellow[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$yellow) +
  geom_rect(ymin = tier_thresh$orange[1], ymax = tier_thresh$orange[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$orange) +
  geom_rect(ymin = tier_thresh$red[1], ymax = tier_thresh$red[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$red) +
  geom_rect(ymin = tier_thresh$purple[1], ymax = tier_thresh$purple[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$purple) +
  geom_rect(ymin = tier_thresh$beyond[1], ymax = tier_thresh$beyond[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$beyond) +
  geom_col(fill='grey50') +
  geom_text(data = tibble(
    x = c(-2, -1, -2, -1),
    y = with(tier_thresh, c(yellow[1], orange[1], red[1], purple[1])),
    label = c('Yellow*', 'Orange*', 'Red*', 'Purple*')),
    aes(x, y, label=label),
    hjust=-0.1, vjust=-2, size=3, color=anno_text_clr) +
  geom_text(data = tibble(
    x = c(-2),
    y = tier_thresh$beyond[1],
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
  geom_rect(ymin = tier_thresh$yellow[1], ymax = tier_thresh$yellow[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$yellow) +
  geom_rect(ymin = tier_thresh$orange[1], ymax = tier_thresh$orange[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$orange) +
  geom_rect(ymin = tier_thresh$red[1], ymax = tier_thresh$red[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$red) +
  geom_rect(ymin = tier_thresh$purple[1], ymax = tier_thresh$purple[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$purple) +
  geom_rect(ymin = tier_thresh$beyond[1], ymax = tier_thresh$beyond[2], 
            xmin =-Inf, xmax= Inf, fill = tier_clrs$beyond) +
  
  # data
  geom_line(aes(x=ymd, y=new_cases_7_rate_100k, group=name),
            color = 'grey50', alpha=0.7) +
  
  # annotation
  geom_text(data = tibble(
    x = last_date + rep(1, 5),
    y = with(tier_thresh, 
             c(mean(yellow), 
               mean(orange), 
               mean(red), 
               mean(purple), 
               mean(beyond))
             ),
    label = c('Yellow', 'Orange', 'Red', 'Purple', "No TK-6\nin person")),
    aes(x, y, label=label),
    fontface='italic', 
    size=3, color='black') +
  labs(
    title = glue('OUSD-area COVID cases by zip vs CA Public Health Tiers'),
    subtitle = glue('Two weeks through {last_date}'),
    y='7 day average new cases per 100k',
    x='date',
    caption = glue(
      'CA DPH Tiers: https://covid19.ca.gov/safer-economy\n',
      '25/100k guidance for TK-6 reopening, 5 consec. days: http://bit.ly/ca_school_reopen, p8')) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c(0, 30)) +
  theme_light() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.caption = element_text(color='grey40'))
ggsave('images/ousd_zip_tiers_14days.jpg', w=6, h=4, dpi=300)

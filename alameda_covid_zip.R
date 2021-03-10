# script to plot covid stuff by zip, alameda county. by twitter.com/arvi1000
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)

# 1. get data ----
# case rate by zip data
# "Alameda County COVID-19 Cases and Case Rates by Zip Code"
# https://data.acgov.org/datasets/5d6bf4760af64db48b6d053e7569a47b_0
zip_rate_json <- fromJSON('https://services5.arcgis.com/ROBnTHSNjoZ2Wm1P/arcgis/rest/services/COVID_19_Statistics/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json')
# extract pop by zip
zip_pop <- zip_rate_json$features$attributes %>%
  mutate(name = paste0('ZC_', Zip_Alpha), pop = Population) %>%
  select(name, pop)
  

# cumulative case count data
# "Alameda County COVID-19 Daily Cumulative Cases by City, Place and Zip Code"
# https://data.acgov.org/datasets/5d6bf4760af64db48b6d053e7569a47b_3
cumul_json <- fromJSON('https://opendata.arcgis.com/datasets/5d6bf4760af64db48b6d053e7569a47b_3.geojson')
# select relevant fields, format dates
cumul_cases_wide <- 
  cumul_json$features$properties %>%
  mutate(ymd = ymd(gsub('T.*Z', '', dtcreate))) %>%
  select(starts_with('ymd') | starts_with('ZC'))

# 2. munge ----
# flesh out to one row per date
full_dates <- tibble(ymd = with(
  cumul_cases_wide,
  seq(from=min(ymd), to=max(ymd), by='day')
  ))
cumul_cases_wide <- 
  merge(full_dates, cumul_cases_wide, by='ymd', all.x = T)
cumul_cases_wide[, 2:ncol(cumul_cases_wide)] <-
  lapply(cumul_cases_wide[, 2:ncol(cumul_cases_wide)], zoo::na.locf)

# reshape & clean up
cumul_cases <- cumul_cases_wide %>% pivot_longer(cols = starts_with('ZC'))
cumul_cases <- cumul_cases %>%
  # assume "<10" (i.e. 1-9) = 5
  mutate(cases = ifelse(value=='<10', 5, as.numeric(value))) %>%
  select(ymd, name, cases)
# new cases & rolling average new cases
cumul_cases <- cumul_cases %>%
  arrange(name, ymd) %>%
  group_by(name) %>%
  mutate(new_cases = cases- lag(cases),
         new_cases_7 = zoo::rollmean(new_cases, k = 7, 
                                     na.pad = T, align = 'right'))
# merge pop data, calc case rate
cumul_cases <- cumul_cases %>%
  merge(zip_pop, by='name') %>%
  mutate(case_rate_100k = cases/(pop/100000),
         new_cases_7_rate_100k = new_cases_7/(pop/100000))

# subset to just OUSD and adjacent
# this csv written out by oakland_zips.R
ousd_area_zips <- read.csv('ousd_area_zips.csv',
                           colClasses = 'character',
                           stringsAsFactors = F)
cumul_cases_all <- cumul_cases
cumul_cases <- cumul_cases_all %>%
  filter(gsub('ZC_', '', name) %in% ousd_area_zips$ousd_zips)

# 3. plot params ----
# top zips as of max date
top_zips <- cumul_cases %>%
  group_by(name) %>%
  filter(ymd == max(ymd)) %>%
  arrange(-case_rate_100k)
top_n <- 5
my_pal <- scales::hue_pal()(top_n)
highlight_zips <- paste0('ZC_', c('94601', '94621', '94603'))
pop_min <- 2000
last_date <- max(cumul_cases$ymd)
start_date <- ymd('2021-01-01')
end_date <- last_date + 16
jpg <- list(w=7, h=4, dpi=200)
txt_size <- 8
label_size <- 2


# 4. do plot 1 ----
set.seed(123) # for reproducible label placement
p1 <- cumul_cases %>% 
  filter(ymd >= start_date) %>%
  ggplot(aes(x=ymd, y=case_rate_100k, group=name,
             color = ifelse(name %in% highlight_zips, name, 'exclude'),
             size = name %in% highlight_zips)
         ) +
  geom_line(alpha=0.7) +
  ggrepel::geom_text_repel(data = (
    cumul_cases %>%
      group_by(name) %>%
      filter(ymd == max(ymd),
             name %in% top_zips$name[1:top_n])
    ),
    hjust=-0.5, size=label_size, segment.color = 'lightgrey',
    aes(label= paste0(name, ' (pop ', round(pop/1000,1), 'k)'))
    ) +
  labs(y='cases per 100k pop', x='date', 
       title='OUSD Area Cumulative Covid Case Rate by Zip',
       subtitle = glue('Data thru {last_date}')) +
  xlim(c(start_date, end_date)) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_manual(values = c(0.5,2)) +
  scale_color_manual(values = c('grey40', my_pal)) +
  theme_light(base_size = txt_size) +
  theme(legend.position = 'none')

# 5. do plot 2 ----
p2 <- cumul_cases %>% 
  filter(ymd >= start_date & pop >= pop_min
         ) %>%
  ggplot(aes(x=ymd, y=new_cases_7_rate_100k, group=name,
             color = ifelse(
               name %in% highlight_zips, name, 'exclude'),
             size = name %in% highlight_zips,
             )) +
  geom_line(alpha=0.7) +
  ggrepel::geom_text_repel(data = (
    cumul_cases %>%
      filter(name %in% highlight_zips) %>%
      group_by(name) %>%
      filter(ymd == max(ymd))
  ),
  hjust= -0.5, size=label_size, segment.color = 'lightgrey',
  aes(label= name)
  ) +
  
  scale_size_manual(values = c(0.5,2)) +
  scale_color_manual(values = c('grey40', my_pal)) +
  xlim(c(start_date, end_date)) + 
  theme_light(base_size = txt_size) +
  theme(legend.position = 'none') +
  labs(title='OUSD area, rolling 7 day average of new COVID cases by zip',
       subtitle = glue('Excluding zips below {pop_min} population. ',
                       'Data thru {last_date}'),
       y='cases per 100k pop',
       x='date')

# show plots
p1
p2
p2_last_14 <- p2 + 
  xlim(last_date - 14, last_date + 2) +
  ylim(c(0,25))

# 6. save data & plots to file ----
csv_fl <- glue('data/ousd_covid_data_{last_date}.csv')
write.csv(cumul_cases, csv_fl)
ggsave('images/ousd_covid_cumul.jpg', plot=p1, w=jpg$w, h=jpg$h, dpi=jpg$dpi)
ggsave('images/ousd_covid_rate.jpg', plot=p2, w=jpg$w, h=jpg$h, dpi=jpg$dpi)
#ggsave('images/ousd_covid_rate_last14.jpg', plot=p2_last_14, w=jpg$w, h=jpg$h, dpi=jpg$dpi)

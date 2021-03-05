options(tigris_use_cache = TRUE)

library(tigris)
library(sf)

# get shapefiles (add cb=T for coarse resolution)
sd_sf <- school_districts(state = 'CA')
zip_sf <- zctas(state='CA')

# which zips overlap OUSD?
ousd_sf <- sd_sf[grep('Oakland', sd_sf$NAME), ] 
oakland_zip_indexes <- which(sapply(st_overlaps(zip_sf, ousd_sf), length) > 0)
ousd_zips <- zip_sf[oakland_zip_indexes, ]

highlight <- c('94601', '94621', '94603')
map_pal <- c('white', scales::hue_pal()(5))

# plot
library(ggplot2)
ggplot() +
  geom_sf(data=ousd_zips, color='grey', alpha=0.5,
          aes(fill=ifelse(ZCTA5CE10 %in% highlight, ZCTA5CE10, '(other)'))) +
  geom_sf(data=ousd_sf, color='black', fill=NA) +
  theme_void() +
  theme(title = element_text()) +
  scale_fill_manual(values=map_pal) +
  labs(title='OUSD adjacent zip codes',
       subtitle = 'school district boundary in black, adjacent zips in grey',
       fill='zip')
ggsave('images/ousd_zip.jpg', w=7, h=5, dpi = 300)

# print the zips
sort(ousd_zips$ZCTA5CE10)


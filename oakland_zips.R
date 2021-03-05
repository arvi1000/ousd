options(tigris_use_cache = TRUE)

library(tigris)
library(sf)

# get shapefiles (coarse resolution)
sd_sf <- school_districts(state = 'CA')
zip_sf <- zctas(state='CA')

# which zips overlap OUSD?
ousd_sf <- sd_sf[grep('Oakland', sd_sf$NAME), ] 
oakland_zip_indexes <- which(sapply(st_overlaps(zip_sf, ousd_sf), length) > 0)
ousd_zips <- zip_sf[oakland_zip_indexes, ]

# plot
library(ggplot2)
ggplot() +
  geom_sf(data=ousd_zips, color='grey', fill='white', alpha=0.5) +
  geom_sf(data=ousd_sf, color='red', fill=NA) +
  theme_void() +
  theme(title = element_text()) +
  ggtitle('OUSD adjacent zip codes',
          subtitle = 'school district boundary in red, adjacent zips in grey')

# print the zips
sort(ousd_zips$ZCTA5CE10)
ggsave('ousd_zip.jpg', w=7, h=5, dpi = 300)

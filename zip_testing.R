library(jsonlite)
library(tidyverse)
library(glue)

# Alameda County COVID-19 Test Rate by Zip Code
# https://data.acgov.org/datasets/5d6bf4760af64db48b6d053e7569a47b_2
zip_test_json <- fromJSON('https://services5.arcgis.com/ROBnTHSNjoZ2Wm1P/arcgis/rest/services/COVID_19_Statistics/FeatureServer/2/query?where=1%3D1&outFields=*&outSR=4326&f=json')
zip_test <- zip_test_json$features$attributes %>%
    select(zip = Zip_Alpha, pop = Population, 
           tests = Tests, test_rates =TestRates)
selected_zips <- c(
  '94601','94603','94577','94621','94605','94607','94704','94608','94606',
  '94602','94612','94619','94609','94618','94705','94611','94610')

all_zips <- zip_test %>%
  summarise(total_pop = sum(pop), total_test = sum(tests)) %>%
  mutate(total_test_rate = total_test / total_pop * 1000)

zip_test %>%
  filter(zip %in% selected_zips) %>%
  arrange(test_rates) %>%
  ggplot(aes(x=fct_inorder(zip), y=test_rates)) +
  geom_col(fill='orange') +
  geom_text(aes(label=round(test_rates)), 
            hjust=1.1, color='grey20', size=3) +
  geom_hline(yintercept = all_zips$total_test_rate,
             linetype='dashed', color='black') +
  coord_flip() +
  theme_light() +
  labs(title=glue('Oakland Test Rates: Zip vs Alameda Co Avg',
                  '\n(dotted line {all_zips$total_test_rate}'),
       x='Zip', y='Test rate (tests/population * 1000)')
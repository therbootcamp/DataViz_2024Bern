


basel <- read_csv('_sessions/Maps/1_Data/taxation.csv')
basel_map = sf::read_sf('_sessions/Maps/1_Data/map/Wohnviertel.shp')

plot(basel_map$geometry)

library(ggrepel)

basel_map %>% 
  left_join(basel %>% filter(year == 2017), by = c("TYPE" = "quarter")) %>% 
  ggplot() + 
  geom_sf(mapping=aes(fill = income_mean), col = 'white') + 
 theme_void() + 
  labs(title = "Inequality in Basel",
       subtitle = "Average income in Basel's quarters in 2017",
       caption = "Source: Source: Open Data Basel Stadt") + 
  scale_fill_continuous(name = 'Income\nin CHF') 

stat_sf_coordinates(p)

names(p)
p$scales

ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
ggplot_build(p)$layout$panel_scales_y[[1]]$range$range

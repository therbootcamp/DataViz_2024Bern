

set.seed(100)
d1 = rnorm(626, 129265, 20000)
d2 = d1 + (d1 / max(d1))**5 * mean(d1)*1
d3 = d2 + (d1 / max(d1))**5 * mean(d1)*1
d4 = d3 + (d1 / max(d1))**5 * mean(d1)*1
d5 = d4 + (d1 / max(d1))**5 * mean(d1)*1
d6 = d5 + (d1 / max(d1))**5 * mean(d1)*1
d7 = d6 + (d1 / max(d1))**5 * mean(d1)*1

tbl = tibble(skew = rep(1:7, rep(626,7)), 
             distr = c(d1, d2, d3, d4, d5, d6, d7))

tbl = tbl %>%
  group_by(skew) %>%
  mutate(cum = cumsum(sort(distr))/sum(distr),
         cum_pos = seq(0,1,length = 626))

tbl = tbl %>% 
  group_by(skew) %>% 
  summarize(distr_mean = mean(distr),
            distr_median = median(distr),
            gini = reldist::gini(distr)) %>% 
  right_join(tbl)



require(magick)
require(gganimate)

cols = viridis::cividis(7)
names(cols) = as.character(1:5)

p1 = ggplot(tbl, aes(x = distr, group = skew)) + 
  geom_histogram(fill = '#EA4B68') + 
  theme_minimal() +
  geom_segment(data = unique(tbl %>% select(skew, distr_mean)), mapping=aes(x = distr_mean, xend = distr_mean, y = 0, yend = 300)) +
  geom_segment(data = unique(tbl %>% select(skew, distr_median)), mapping=aes(x = distr_median, xend = distr_median, y = 0, yend = 300)) + 
  geom_text(data = unique(tbl %>% select(skew, distr_mean)), mapping= aes(x = distr_mean + 2000, y = 300, label = "Mean"), hjust = 0, size = 8) +
  geom_text(data = unique(tbl %>% select(skew, distr_median)), mapping= aes(x = distr_median - 2000, y = 300, label = "Median"), hjust = 1, size = 8) +
  labs(x = "Income", y = "Frequency") +
  theme(axis.title = element_text(size=32),
        axis.text = element_text(size=20)) + 
  guides(fill = F) + 
  gganimate::transition_states(skew)


p2 = ggplot(tbl, aes(x = cum_pos,  y = cum, group = skew)) + 
  geom_polygon(fill = '#606060', alpha = .3) + 
  geom_line(col = '#EA4B68', size = 2) + 
  theme_minimal() + 
  labs(x = "Fraction of population", 
       y = "Cumulative share of income") +
  theme(axis.title = element_text(size=32),
        axis.text = element_text(size=20)) + 
  guides(fill = F) + 
  geom_text(data = unique(tbl %>% select(skew, gini)), 
            mapping= aes(x = .2, y = .8, label = paste0("Gini: ",round(gini, 2))), size = 10) +
  gganimate::transition_states(skew)

gganimate::animate(p1, renderer = gganimate::gifski_renderer(), width=600, height=300)
anim_save('_sessions/Welcome/image/distr.gif')

gganimate::animate(p2, renderer = gganimate::gifski_renderer(), width=600, height=600)
anim_save('_sessions/Welcome/image/gini.gif')




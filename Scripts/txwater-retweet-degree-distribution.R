library(rtweet)
library(igraph)
library(hrbrthemes)
library(tidyverse)

readRenviron("~/.Renviron")

rstats <- search_tweets("txwater", n=1500)


filter(rstats, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rt_g

summary(rt_g)


ggplot(data_frame(y=degree_distribution(rt_g), x=1:length(y))) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand=c(0,0), trans="sqrt") +
  labs(x="Degree", y="Density (sqrt scale)", title="#txwater Retweet Degree Distribution") +
  theme_ipsum_rc(grid="Y", axis="x")

ggsave(here::here("/Figures/retweetdegree.png"), dpi = 300, width = 8, height = 8, units = "in")

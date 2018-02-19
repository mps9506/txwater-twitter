### Visualizing a graph of Retweet Relationships
### recipe from : https://github.com/hrbrmstr/21-recipes/blob/master/07-Visualizing-a-Graph-of-Retweet-Relationships.Rmd


library(rtweet)
library(igraph)
library(hrbrthemes)
library(ggraph)
library(tidyverse)

readRenviron("~/.Renviron")
extrafont::loadfonts()

txh20Stats <- search_tweets("txwater", n=5000) ## only returns last 8 or so days. Anyway around this?

filter(txh20Stats, retweet_count >0) %>%
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rt_g


V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 12, names(V(rt_g)), "")) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 12, degree(rt_g), 0)) 

ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, family=font_rc, fontface="bold") +
  coord_fixed() +
  #scale_size_area(trans="sqrt") +
  labs(title="#txwater Relationships", subtitle="Most retweeted screen names labeled. Darkers edges == more retweets. Node size == larger degree") +
  theme_graph(base_family=font_rc) +
  theme(legend.position="none")

ggsave(here::here("/Figures/retweetrelationships.png"), dpi = 300, width = 9, height = 9, units = "in")

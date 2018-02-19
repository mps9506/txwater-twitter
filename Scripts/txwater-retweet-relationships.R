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

df <- filter(txh20Stats, retweet_count >0) %>%
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) 

tr_g <- graph_from_data_frame(df)



V(rt_g)$node_label <- unname(names(V(rt_g))) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 1, degree(rt_g), 1)) 
V(rt_g)$node_alpha <- unname(degree(rt_g)/max(degree(rt_g)))
nIds <- length(V(rt_g))
V(rt_g)$Id <- seq(1:nIds)
V(rt_g)$label_angle <- 90 - 360 *  V(rt_g)$Id / nIds
V(rt_g)$hjust <- ifelse(V(rt_g)$label_angle < -90, 1, 0)
V(rt_g)$angle <- ifelse(V(rt_g)$label_angle < -90, V(rt_g)$label_angle+180, V(rt_g)$label_angle)

p <- ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(alpha=..index..)) +
  geom_node_point(aes(x = x*1.07, y=y*1.07, size=node_size,  alpha=0.2)) +
  geom_node_text(aes(x=x*1.15, y=y*1.15,label=node_label, angle=angle, hjust=hjust),
                  color="dodgerblue", size=2.7, family=font_rc) +
  coord_fixed() +
  labs(title="#txwater Relationships", subtitle="Darkers edges == more retweets. Node size == larger degree") +
  theme_graph(base_family=font_rc) +
  theme(legend.position="none") +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

ggsave(here::here("/Figures/retweetrelationships.png"),p , dpi = 300, width = 9, height = 9, units = "in")

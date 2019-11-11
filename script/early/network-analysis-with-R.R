# https://www.jessesadler.com/post/network-analysis-with-r/

library(tidyverse)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

edge_list

letters <- read_csv(url("https://raw.githubusercontent.com/jessesadler/intro-to-r/master/data/correspondence-data-1585.csv"))

letters

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route <- letters %>%
  group_by(source, destination) %>%
  summarise(weight = n()) %>%
  ungroup()
per_route

### --------------------

edges <- per_route %>%
  left_join(nodes, by = c("source" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(nodes, by = c("destination" = "label")) %>%
  rename(to = id)


## ---------------

edges <- select(edges, from, to, weight)
edges

## ------------

library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network, vertex.cex = 3)

# igraph ------------

detach(package:network)
rm(routes_network)
library(igraph)

## ----------

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

## -----

plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

# tidygraph and ggraph -------------

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy %>%
  activate(edges) %>%
  arrange(desc(weight))

## ggraph --------------

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

# Interactive network graphs ----------------

library(visNetwork)
library(networkD3)

## visNetwork ------------

visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visEdges(arrows = "middle")

## networkD3 -----------

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
             NodeID = "label", Group = "id", Value = "weight",
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")


## Stochastic block model ---------------

pm <- cbind( c(.4, .01), c(.01, .4) )
g <- sample_sbm(50, pref.matrix=pm, block.sizes=c(15,35))
g
plot(g, layout = layout_with_graphopt)

### Heteronormative SBM -----------

pm <- cbind( c(.01, .4), c(.4, .01) )
g <- sample_sbm(20, pref.matrix=pm, block.sizes=c(10,10))
g
plot(g, layout = layout_with_graphopt)

### SBM more

pm <- (0.3 - 1e-2) * diag(3) + 1e-2
g <- sample_sbm(60, pref.matrix=pm, block.sizes=c(20,20,20))
g
plot(g, layout = layout_with_graphopt)

library(tidyverse) # general data manipulation
library(tidygraph) # manipulating graph objects in a tidy way
library(igraph) # graph muscle
library(visNetwork) # wrapper for vis.js visualization package


## import data and create a graph object ----

funder_edges <- read_csv("https://raw.githubusercontent.com/etachov/funder_network/master/data/avant_peers_edges.csv") 

funder_nodes <- read_csv("https://raw.githubusercontent.com/etachov/funder_network/master/data/avant_peers_nodes.csv") 

# use igraph::graph_from_data_frame to preserve named edge connections
funder_graph <- graph_from_data_frame(d = funder_edges_raw, directed = TRUE, vertices = funder_nodes_raw) %>%
  # then convert to a tbl_graph object
  as_tbl_graph()

## calculate graph metrics ----

funder_graph_metrics <- funder_graph %>%
  # calculate basic degree centrality
  mutate(degree.out = centrality_degree(mode = "out"), 
         degree.in = centrality_degree(mode = "in"))


## style and format nodes object for vis.js ----

# set colors 
funder_color <- "rgba(92, 75, 117, .9)" # flat purple
peer_color <- "rgba(78, 148, 122, 1)" # flat green
funder_color_half_alpha <- "rgba(92, 75, 117, .5)" # same purple, half alpha
edge_color <- "rgba(153, 153, 153, 1)"
white_half_alpha <- "rgba(255, 255, 255, .5)" 

# add conditional styling and formatting for visNetwork
funder_nodes <- funder_graph_metrics %>% 
  as.tibble() %>%
  # visNetwork uses "id" for index
  rename(id = name) %>% 
  # for visNetwork, style are stored in columns; column names must be preserved
  mutate(color.background = ifelse(funder.peer == "Peer", peer_color, funder_color),
         color.highlight.background = ifelse(funder.peer == "Peer", peer_color, funder_color),
         color.border = white_half_alpha,
         color.highlight.border = ifelse(funder.peer == "Peer", peer_color, funder_color),
         shape = ifelse(funder.peer == "Peer",  "square", "dot"),
         # hacking this a bit so it looks ok
         value = (degree.out + 1)*2,
         font.size = ifelse(funder.peer == "Peer", 24, value * 2), 
         group = org.type)

# build the the network and add some shit styling inline
graph <- visNetwork(nodes = funder_nodes, 
                    edges = funder_edges, 
                    height = "600", 
                    width = "100%", 
                    main = list(text = "Funder - Recipient Network", 
                                style = "font-size:36px;text-align:center;"),
                    submain = list(text = "&#x25CF; Funder   &#x25A0; Peer Org   &#x2014; Relationship<br>Data available <a href='https://github.com/etachov/funder_network' style='color:#000000' target='_blank'>here</a>", 
                                   style = "font-size:20px;text-align:center;")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, 
                                    degree = 1, 
                                    labelOnly = FALSE), 
             nodesIdSelection = list(enabled = TRUE, 
                                     style = "background: #000000;color: #ffffff;width:200px;"),  
             selectedBy = list(variable = "group", 
                               style = "background: #000000;color: #ffffff;width:200px"))  %>%
  visEdges(color = list(color = edge_color, highlight = funder_color_half_alpha)) %>%
  visLayout(randomSeed = 86) 
  
  
# save the html widget out as a self-containted file
visSave(graph, 
        file = "funder_peers_graph.html",
        selfcontained = TRUE,
        background = "#EDEDED")


#14 Networks ####

library(tidyverse)
library(politicalds)
library(tidygraph)
library(ggraph)
library(ggcorrplot)

#14.3    Network datasets ####

data("cosponsorship_arg")  
data("senators_arg")  

ls()  

senators_arg  

cosponsorship_arg  

tg_cosponsorship_arg <- tbl_graph(nodes = senators_arg,  
                                  edges = cosponsorship_arg,  
                                  directed = T) # our network is directed 

tg_cosponsorship_arg  

tg_cosponsorship_arg <- tg_cosponsorship_arg %>%  
  activate("edges") %>% # or "nodes" if you want to edit that dataset  
  mutate(d_cosponsorship = if_else(n_cosponsorship >= 1, 1, 0),  
         n_cosponsorship_inv = 1 / n_cosponsorship)  

tg_cosponsorship_arg  

#14.4    Graphic presentation of a network ####

set.seed(1)  

layout_fr <- create_layout(tg_cosponsorship_arg, "fr",  
                           weights = n_cosponsorship)  

ggraph(layout_fr) +  
  geom_node_point(size = 5) +  
  theme_void() # an empty canvas for our network 

ggraph(layout_fr) +  
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')),  
                 color = "lightgrey") +  
  geom_node_point(size = 5) +  
  theme_void()  

ggraph(layout_fr) +  
  geom_edge_link(mapping = aes(alpha = n_cosponsorship_inv)) +  
  geom_node_point(size = 5) +  
  theme_void()  

ggraph(layout_fr) +  
  geom_edge_link(mapping = aes(alpha = n_cosponsorship)) +  
  geom_node_point(mapping = aes(color = political_bloc,  
                                shape = political_bloc),  
                  size = 5) +  
  scale_shape_manual(values = 15:18) + # edit the shapes  
  theme_void()  

#14.5     Measures of centrality ####

#14.5.5    Application in R 

tg_cosponsorship_arg_centr <- tg_cosponsorship_arg %>%  
  activate("nodes") %>% # the next operations will edit the "nodes" tibble  
  mutate(  
    # degree  
    c_in_degree = centrality_degree(mode = "in"),  
    c_out_degree = centrality_degree(mode = "out"),  
    # strength (weighted degree)  
    c_in_strength = centrality_degree(mode = "in",
                                      weights = n_cosponsorship),  
    c_out_strength = centrality_degree(mode = "out",  
                                       weights = n_cosponsorship),  
    # weighted pagerank (eigenvector centrality alternative)  
    c_pagerank = centrality_pagerank(weights = n_cosponsorship),  
    # betweenness, with or without inverted weights  
    c_betweenness = centrality_betweenness(),  
    c_betweenness_w = centrality_betweenness(weights = n_cosponsorship_inv),  
    # closeness, with or without inverted weights  
    c_closeness = centrality_closeness(),  
    c_closeness_w = centrality_closeness(weights = n_cosponsorship_inv)  
  )  

tg_cosponsorship_arg_centr  

#The below select function pulls the node data as a list of lists, not a dataframe or matrix, which makes it incompatible with the cor and plotting functions used
#I've added a "data.frame" command to the code in the text

corr_centrality <- data.frame(tg_cosponsorship_arg_centr %>%  
  select(starts_with("c_"))) %>%  
  # calculate correlation matrix and round to 1 decimal place:  
  cor(use = "pairwise") %>%  
  round(1)  

ggcorrplot(corr_centrality, type = "lower", lab = T, show.legend = F)  

tg_cosponsorship_arg_centr %>%  
  arrange(-c_in_degree) %>%  
  select(id_sen, name_sen, c_in_degree)  

tg_cosponsorship_arg_centr %>%  
  arrange(-c_out_degree) %>%  
  select(id_sen, name_sen, c_out_degree)  

ggplot(tg_cosponsorship_arg_centr %>%  
         as_tibble() %>%  
         filter(political_bloc %in% c("UCR", "JUSTICIALISTA")),  
       aes(x = political_bloc, y = c_pagerank)) +  
  geom_boxplot()  


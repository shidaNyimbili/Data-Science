#load packages
source("scripts/r prep2.r")

#Load the data
# load data
rom <- read.delim("https://slcladal.github.io/data/romeo_tidy.txt", sep = "\t")
rom



#We now transform that table into a co-occurrence matrix
rome <- crossprod(table(rom[1:2]))
diag(rome) <- 0
romeo <- as.data.frame(rome)

romeo

#network visualization
va <- romeo %>%
  dplyr::mutate(Persona = rownames(.),
                Occurrences = rowSums(.)) %>%
  dplyr::select(Persona, Occurrences) %>%
  dplyr::filter(!str_detect(Persona, "SCENE"))

#we define the edges, i.e., the connections between nodes and
#again, we can add information in separate variables that we can use later on.

ed <- romeo %>%
  dplyr::mutate(from = rownames(.)) %>%
  tidyr::gather(to, Frequency, BALTHASAR:TYBALT) %>%
  dplyr::mutate(Frequency = ifelse(Frequency == 0, NA, Frequency))
#Now that we have generated tables for the edges and the nodes, we can generate a graph object
ig <- igraph::graph_from_data_frame(d=ed, vertices=va, directed = FALSE)

#We will also add labels to the nodes as follows:
tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)
#When we now plot our network, it looks as shown below.
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                alpha = .1) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 colour="gray10") +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)
#Now, we use the number of occurrences to define vertex size (or node size): 
#the more often a character appears, the bigger it will appear in the graph.
v.size <- V(tg)$Occurrences
# inspect
v.size

#When we include this into our network, it looks as shown below
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#Next, we modify the edges by using frequency information to define weights:
#the more often two characters appear in the same scene, the bigger the edge

E(tg)$weight <- E(tg)$Frequency
# inspect weights
head(E(tg)$weight, 10)


#When we include this into our network, it looks as shown below
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#Finally, we define colors so that characters belonging 
#to the same family have the same color.
# define colors (by family)
mon <- c("ABRAM", "BALTHASAR", "BENVOLIO", "LADY MONTAGUE", "MONTAGUE", "ROMEO")
cap <- c("CAPULET", "CAPULET’S COUSIN", "FIRST SERVANT", "GREGORY", "JULIET", "LADY CAPULET", "NURSE", "PETER", "SAMPSON", "TYBALT")
oth <- c("APOTHECARY", "CHORUS", "FIRST CITIZEN", "FIRST MUSICIAN", "FIRST WATCH", "FRIAR JOHN" , "FRIAR LAWRENCE", "MERCUTIO", "PAGE", "PARIS", "PRINCE", "SECOND MUSICIAN", "SECOND SERVANT", "SECOND WATCH", "SERVANT", "THIRD MUSICIAN")
# create color vectors
Family <- dplyr::case_when(sapply(tg, "[")$nodes$name %in% mon ~ "MONTAGUE",
                           sapply(tg, "[")$nodes$name %in% cap ~ "CAPULET",
                           TRUE ~ "Other")
# inspect colors
Family


#Now, that we have created the different objects and defined their 
#properties, we can finally visualize the finished network
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(v.size)*2, 
                  aes(color=Family)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#Quanteda Networks
#The quanteda package contains many very useful functions for analyzing texts. Among these functions is the textplot_network function 
#which provides a very handy way to display networks
# create a document feature matrix
romeo_dfm <- quanteda::as.dfm(romeo)
# create feature co-occurrence matrix
romeo_fcm <- quanteda::fcm(romeo_dfm)
# inspect data
head(romeo_fcm)


#This feature-co-occurrence matrix can then serve as the input for 
#the textplot_network function which already generates a nice network graph.
quanteda.textplots::textplot_network(romeo_fcm, 
                                     min_freq = .5, 
                                     edge_alpha = 0.5, 
                                     edge_color = "purple",
                                     vertex_labelsize = log(rowSums(romeo_fcm)),
                                     edge_size = 2)

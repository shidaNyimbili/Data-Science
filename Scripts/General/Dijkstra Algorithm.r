
source("scripts/r prep2.r")

graph <- list(s = c("a", "b"),
              a = c("s", "b", "c", "d"),
              b = c("s", "a", "c", "d"),
              c = c("a", "b", "d", "e", "f"),
              d = c("a", "b", "c", "e", "f"),
              e = c("c", "d", "f", "z"),
              f = c("c", "d", "e", "z"),
              z = c("e", "f"))

weights <- list(s = c(3, 5),
                a = c(3, 1, 10, 11),
                b = c(5, 3, 2, 3),
                c = c(10, 2, 3, 7, 12),
                d = c(15, 7, 2, 11, 2),
                e = c(7, 11, 3, 2),
                f = c(12, 2, 3, 2),
                z = c(2, 2))

# create edgelist with weights
G <- data.frame(stack(graph), weights = stack(weights)[[1]])

set.seed(500)
el <- as.matrix(stack(graph))
g <- graph_from_edgelist(el)

oldpar <- par(mar = c(1, 1, 1, 1))
plot(g, edge.label = stack(weights)[[1]])
par(oldpar)

#we create a helper function to calculate the path length:
path_length <- function(path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # get all consecutive nodes
  pairs <- cbind(values = path[-length(path)], ind = path[-1])
  # join with G and sum over weights
  sum(merge(pairs, G)[ , "weights"])
}


find_shortest_path <- function(graph, start, end, path = c()) {
  # if there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # add next node to path so far
  path <- c(path, start)
  
  # base case of recursion: if end is reached return path
  if (start == end) return(path)
  
  # initialize shortest path as NULL
  shortest <- NULL
  # loop through all nodes linked from the current node (given in start)
  for (node in graph[[start]]) {
    # proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- find_shortest_path(graph, node, end, path)
      # if newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(newpath) < path_length(shortest))
        shortest <- newpath
    }
  }
  # return shortest path
  shortest
}

find_shortest_path(graph, "s", "z") # via b
## [1] "s" "b" "c" "d" "f" "z"

find_shortest_path(graph, "z", "s") # back via a
## [1] "z" "f" "d" "b" "a" "s"


##Dijkstra Algorithm
dij_alg <- as.matrix(read.csv("data/general/dijkstra.csv"))

dij_alg

dij <- graph_from_adjacency_matrix(dij_alg, mode='undirected', weighted = TRUE)
V(dij)$color = 2
E(dij)$color = 2
plot(dij, edge.label=E(dij)$weight)

#Path Calculation
shortest.paths(dij, v=1, to=V(dij), algorithm="dijkstra")

shortest.paths(dij, v=V(dij), to=V(dij), algorithm="dijkstra")

#Shortest path computation
shortest_paths(dij, 1, to=V(dij), output = "both")



sciezka <- shortest_paths(dij, 1, to=V(dij))$vpath[[10]]
V(dij)[sciezka]$color <- 6

sciezka2 <- shortest_paths(dij, 1, to=V(dij), output = "both")$epath[[10]]
E(dij)[sciezka2]$color <- 6

plot(dij,edge.label=E(dij)$weight)

     
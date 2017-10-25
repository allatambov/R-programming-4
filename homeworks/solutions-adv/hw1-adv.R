########################### HW1 solution (advanced) ###########################

# load text file
lines <- readLines("friends.txt")

# create list of names and vector of names
friends_list <- strsplit(lines, " ")
names <- sort(unique(unlist(friends_list)))

# create adjacency matrix
adj_matrix <- matrix(0, nrow = length(names), ncol = length(names))

for (i in 1:length(friends_list))
  adj_matrix[i,] <- as.numeric(names %in% unlist(friends_list[i]))

# set diagonal elements to 0 and add dimnames
diag(adj_matrix) <- 0
dimnames(adj_matrix) <- list(names, names)

# create network
library(igraph)

network <- graph_from_adjacency_matrix(adj_matrix, mode = c("directed"))

plot(network,
     edge.color = "black",
     vertex.color = "darkblue",
     vertex.size = 20,
     vertex.label.color = "white",
     edge.arrow.size = 0.15)
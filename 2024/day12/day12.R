library(purrr)
library(igraph)
library(tidygraph)

read_matrix <- function(path = "input.txt") {
  lines <- readLines(path) %>%
    map(~{strsplit(.x, "")[[1]]})
  mat_items <- unlist(lines)
  matrix(mat_items, nrow = length(lines), byrow = TRUE)
}

# Build adjacency matrix from a matrix of values
# NOTE: some gotchas, if the values contain 0 and you want to use weighted = TRUE,
# you'll have to shift their values up with `weights = m +1` or the `score` will insert 0,
# and make it look like 0-labeled nodes have no connections.
build_adj_mat <- function(m, weighted = TRUE) {

  adj_matrix <- matrix(0, nrow = length(m), ncol=length(m))
  weights <- m
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      node_id <- (i - 1) * ncol(m) + j

      if (weighted) {
        score <- weights[node_id]
      } else {
        score <- 1
      }

      # Add edges for neighboring cells
      if (i > 1) {
        adj_matrix[node_id, (node_id - ncol(m))] <- score
      }
      if (i < nrow(m)) {
        adj_matrix[node_id, (node_id + ncol(m))] <- score
      }
      if (j > 1) {
        adj_matrix[node_id, (node_id - 1)] <- score
      }
      if (j < ncol(m)) {
        adj_matrix[node_id, (node_id + 1)] <- score
      }
    }
  }
  adj_matrix
}

#mat <- read_matrix("example-simple.txt")
#mat <- read_matrix("example.txt")
#mat <- read_matrix("example-xo.txt")
mat <- read_matrix("input.txt")
am <- build_adj_mat(mat, weighted = FALSE)

g <- graph_from_adjacency_matrix(am)
V(g)$name <- mat
#plot(g)

neighbors_by_type <- g %>%
  as_tbl_graph() %>%
  activate(edges) %>%
  mutate(from_name = mat[from],
         to_name = mat[to]
         ) %>%
  filter(from_name == to_name)

#neighbors_by_type %>%
#  plot

region_graphs <- neighbors_by_type %>%
  to_components()

# where g is a region graph
get_graph_perimeter <- function(region_graph) {

  region_graph %>%
    activate(nodes) %>%
    mutate(p = dplyr::case_when(
                        centrality_degree() == 4 ~ 0,
                        centrality_degree() == 3 ~ 1,
                        centrality_degree() == 2 ~ 2,
                        centrality_degree() == 1 ~ 3,
                        centrality_degree() == 0 ~ 4
                        )) %>%
    pull(p) %>%
    sum()
}

map_int(region_graphs, ~{
  get_graph_perimeter(.x) * length(.x)
}) %>%
  sum() %>%
  print


library(dplyr)
library(tidygraph)
library(purrr)
library(igraph)

lines <- readLines("input.txt") %>%
  map(~{as.numeric(strsplit(.x, "")[[1]])})
mat_items <- unlist(lines)
trail_mat <- matrix(mat_items, nrow = length(lines), byrow = TRUE)

# Only keep connections that increment score by 1
build_directed_adj_mat <- function(m) {
  adj_matrix <- matrix(0, nrow = length(m), ncol=length(m))
  # because 0 is a meaningful score, we need to shift up so that 1 is now 0
  # otherwise 0-named nodes will be excluded from the weighted adjacency matrix
  weights <- m + 1
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      node_id <- (i - 1) * ncol(m) + j

      score <- weights[node_id]

      l <- weights[(node_id - ncol(m))]
      r <- weights[(node_id + ncol(m))]
      u <- weights[(node_id - 1)]
      d <- weights[(node_id + 1)]

      # Add directed edges for neighboring cells with higher scores
      if (i > 1 && (l - score == 1)) {
        adj_matrix[node_id, (node_id - ncol(m))] <- 1
      }
      if (i < nrow(m) && (r - score == 1)) {
        adj_matrix[(node_id + ncol(m)), node_id] <- 1
      }
      if (j > 1 && (u - score == 1)) {
        adj_matrix[node_id, (node_id - 1)] <- 1
      }
      if (j < ncol(m) && (d - score == 1)) {
        adj_matrix[(node_id + 1), node_id] <- 1
      }
    }
  }
  adj_matrix
}

# for debugging
view_graph <- function(g) {
  plot(g, vertex.label = igraph::V(g)$weights,
       edge.label = igraph::E(g)$weight
       )
}

build_trail_graph <- function(m) {
  g <- build_directed_adj_mat(m) %>%
    graph_from_adjacency_matrix(weighted = TRUE, mode = "directed")
  # Add real trail position scores as weights
  V(g)$weights <- m[V(g)]

  gf <- g %>%
    as_tbl_graph %>%
    activate(edges) %>%
    mutate(from_score = trail_mat[from],
           to_score = trail_mat[to],
           # These are nodes with backwards edges
           diff = to_score - from_score
           ) %>%
    # Flip backwards edges
    reroute(to = from,
            from = to,
            subset = diff < 0
            )

  gf
}

gf <- build_trail_graph(trail_mat)

#view_graph(gf)

trail_heads <- which(V(gf)$weights == 0)
trail_ends <- which(V(gf)$weights == 9)

paths <- map(trail_heads, ~{
  igraph::shortest_paths(gf, from = .x, to = trail_ends, mode = 'out')
})

linear_paths <- map(paths, ~{
  keep(.x$vpath, ~{
    length(.x) > 0
  })
})

# Part 1
linear_paths %>%
  map(~{
    map(.x, ~{
      list(start = as.numeric(head(.x, 1)),
           end = as.numeric(tail(.x, 1))
           )
    })
  }) %>%
  map(unique) %>%
  map_int(length) %>%
  sum %>%
  print


# Part 2

paths2 <- map(trail_heads, ~{
  igraph::all_simple_paths(gf, from = .x, to = trail_ends, mode = 'out')
})

map_int(paths2, length) %>%
  sum %>%
  print

library(purrr)

x <- readLines("input.txt") %>%
  strsplit("")
mat <- matrix(unlist(x), nrow = length(x), byrow = TRUE)

antenna_types <- unique(split(mat, mat != ".")$`TRUE`) %>%
  .[. != "#"]

antenna_pos <- map(antenna_types, ~{mat == .x}) %>%
  set_names(antenna_types)

get_pos <- function(x) {
  which(antenna_pos[[x]], arr.ind = TRUE)
}

antinode_loc <- function(p, rep=FALSE) {
  if (nrow(p) == 1) {
    return(NULL)
  }
  cn <- combinat::combn(split(p, row(p)), 2, simplify = F)
  map(cn, ~{
    res <- .x[[1]] - .x[[2]]

    antinode_pos <- c(
      .x[[1]] + res,
      .x[[1]] - (res*2)
    )

    if (isTRUE(rep)) {
      do_rep <- 2:(length(diag(mat)))
      addl_pos <- map(do_rep, ~{
        c(
          antinode_pos + (res * .x),
          antinode_pos - (res * .x)
        )
      })
      antinode_pos <- c(antinode_pos, unlist(addl_pos))
    }

    matrix(c(antinode_pos), ncol = 2, byrow = TRUE)
  })
}

drop_oob <- function(m) {
  drops <- m[,1] > nrow(mat) |
           m[,2] > ncol(mat) |
           m[,1] <= 0 |
           m[,2] <= 0
  m[!drops,]
}

get_antinode_loc <- function(type, rep=FALSE) {
  loc <- get_pos(type) %>%
    antinode_loc(rep) %>%
    discard(is_empty)

  if (is.null(loc))
    return(NULL)

  loc %>%
    do.call("rbind", .) %>%
    drop_oob
}

displ <- function(m, nodes=NULL) {
  if (!is.null(nodes)) {
    m[nodes] <- paste0(gsub("\\.", "", m[nodes]), "#")
  }
  cat("\n")
  walk(apply(m,1, paste0, collapse=""), cat, "\n")
  cat("\n")
}

# Part 1
antenna_types %>%
  map(get_antinode_loc) %>%
  do.call("rbind", .) %>%
  unique %>%
  nrow %>%
  print

print(nrow(loc))

# Part 2
loc_p2 <- antenna_types %>%
  map(get_antinode_loc, rep=TRUE) %>%
  do.call("rbind", .) %>%
  unique

print(nrow(loc_p2))


library(purrr)

#x <- readLines("input.txt") %>%
x <- readLines("example.txt") %>%
#x <- readLines("example2.txt") %>%
#x <- readLines("example3.txt") %>%
#x <- readLines("example4.txt") %>% # has an 'a' w/o linear pair @ 10,2
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
    #.x <- cn[[3]]
    res <- .x[[1]] - .x[[2]]
    # TODO: go ahead and compute the antinode loc

    # Not linear
    # TODO: linear check not correct
    #if (!any(abs(res) == 1)) {
    ##if (!any((abs(res) %% 2) == 0)) {
    #  return(NULL)
    #}

    antinode_pos <- c(
      .x[[1]] + res,
      .x[[1]] - (res*2)
    )

    #if (isTRUE(rep)) {
    #  if (all(abs(res) == 1)) {

    #  }
    #  do_rep <- 2:(length(diag(mat))/2)
    #  addl_pos <- map(do_rep, `*`, antinode_pos)

    #  #coords <- .x
    #  #addl_pos <- map(do_rep, ~{
    #  #  c(
    #  #    coords[[1]] + ((res + res) * .x),
    #  #    coords[[1]] - ((res + res) * .x)
    #  #  )
    #  #})
    #  antinode_pos <- c(antinode_pos, unlist(addl_pos))
    #}

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
  if (!is.null(nodes))
    m[nodes] <- paste0(gsub("\\.", "", m[nodes]), "#")
  cat("\n")
  walk(apply(m,1, paste0, collapse=""), cat, "\n")
  cat("\n")
}

loc <- antenna_types %>%
  map(get_antinode_loc) %>%
  do.call("rbind", .) %>%
  unique

# Part 1
print(nrow(loc))

loc_p2 <- antenna_types %>%
  map(get_antinode_loc, rep=TRUE) %>%
  do.call("rbind", .) %>%
  unique

print(nrow(loc_p2))
displ(mat, loc_p2)

# DEBUg

displ(mat, loc)

get_pos('A') %>%
  antinode_loc %>%
  discard(is_empty)

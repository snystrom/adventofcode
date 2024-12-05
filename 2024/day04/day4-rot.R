library(magrittr)
library(purrr)

#lines <- readLines("input.txt") %>%
lines <- readLines("example.txt") %>%
#lines <- readLines("ex2.txt") %>%
  strsplit("")
mat <- matrix(unlist(lines), nrow = length(lines), byrow = TRUE)

# 90 degree matrix rotation
rotate <- function(x) t(apply(x, 2, rev))

match_xmas <- function(str, reverse = FALSE) {
  f <- regmatches(str, gregexpr("XMAS", str))
  #if (!reverse)
  #  # HACK
  #  str <- ""
  r <- regmatches(str, gregexpr("SAMX", str))
  sum(map_int(c(f,r), length))
}

count_linear_match <- function(x) {

  mlist <- list(x, rotate(x))

  lapply(mlist, function(m) {
    str <- apply(m, 1, paste0, collapse = "")
    match_xmas(str, reverse = T)
  }) %>%
    unlist() %>%
    sum
}

all_diagonals <- function(m) {
  diagonal_pos <- 1:(min(nrow(m), ncol(m)) - 1)
  d <- map(diagonal_pos, ~{
    upper <- m[row(m) - col(m) == -.x]
    lower <- m[row(m) - col(m) == .x]
    c(paste0(upper, collapse = ""),
         paste0(lower, collapse = ""))
  })

  c(d, paste0(diag(m), collapse = ""))
}

count_diagonal_match <- function(m) {
  all_diagonals(m) %>%
    map_int(match_xmas, reverse=TRUE) %>%
    sum
}

# I am totally confused because by my count I only see 13 in the example
# so I must be misunderstanding the instructions if the right answer is 18

# WRONG: 1652 - too low
print(count_linear_match(mat) + count_diagonal_match(mat))
# BUT THESE EVAL CORRECTLY ON THE EXAMPLE!?!?!
count_linear_match(mat) == 8
count_diagonal_match(mat) == 5

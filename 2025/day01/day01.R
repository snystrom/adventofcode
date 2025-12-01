library(magrittr)
library(readr)
library(purrr)

#raw <- read_lines("example.txt")
raw <- read_lines("input.txt")
raw

ops <- gsub("L", "-", raw) %>%
  gsub("R", "", .) %>%
  as.numeric(.)

rotate <- function(x, op) {
  i <- x + 1
  dial <- seq(0, 99)

  ((i + op) %% length(dial)) - 1
}


start <- 50

# Part 1
accumulate(ops, rotate, .init = start) %>%
  keep(
    ~ {
      .x == 0
    }
  ) %>%
  length %>%
  print

# Part 2
n_rotations <- function(x, op) {
  i <- x
  rot <- i + op
  dial <- seq(0, 99)

  tick_sequence <- seq(i, rot, by = sign(op)) %% length(dial)
  n_zeros <- sum(tail(tick_sequence, n = -1) == 0)

  return(n_zeros)
}

do_and_count_rotations <- function(x, op) {
  list(
    s_pos = x$pos,
    op = op,
    pos = rotate(x$pos, op),
    n_rot = n_rotations(x$pos, op)
  )
}

accumulate(
  ops,
  do_and_count_rotations,
  .init = list(pos = start, n_rot = 0)
) %>%
  tail(n = -1) %>%
  map_int("n_rot") %>%
  sum %>%
  print

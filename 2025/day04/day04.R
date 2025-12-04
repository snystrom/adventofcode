library(magrittr)
library(purrr)

raw <- readLines("example.txt")
raw <- readLines("input.txt")

stack <- raw %>%
  strsplit("")

stack_mat <- matrix(unlist(stack), ncol = length(stack[[1]]), byrow = TRUE)

safe_empty <- function(x,y = "") {
  if (is_empty(x)) {
    return(y)
  }
  x
}

stack_r6 <- R6::R6Class("stack", public = list(
  initialize = function(x) {
    self$mat <- x
    self$next_mat <- x
    self$cur_i <- 1
    self$cur_j <- 1
    return(invisible(self))
  },
  get = function(i,j) {
    if (i > nrow(self$mat)) {
      return(NULL)
    }

    if (j > ncol(self$mat)) {
      return(NULL)
    }
    self$mat[i, j]
  },
  u = function() {
    safe_empty(self$get(self$cur_i - 1, self$cur_j))
  },
  d = function() {
    safe_empty(self$get(self$cur_i + 1, self$cur_j))
  },
  l = function() {
    safe_empty(self$get(self$cur_i, self$cur_j - 1))
  },
  r = function() {
    safe_empty(self$get(self$cur_i, self$cur_j + 1))
  },
  ul = function() {
    safe_empty(self$get(self$cur_i - 1, self$cur_j - 1))
  },
  ur = function() {
    safe_empty(self$get(self$cur_i - 1, self$cur_j + 1))
  },
  dl = function() {
    safe_empty(self$get(self$cur_i + 1, self$cur_j - 1))
  },
  dr = function() {
    safe_empty(self$get(self$cur_i + 1, self$cur_j + 1))
  },
  #' "Center" aka current pos
  c = function() {
    safe_empty(self$mat[self$cur_i, self$cur_j])
  },
  adjacent_rolls = function() {
    c(
      self$ul(), self$u(), self$ur(),
      self$l(),            self$r(),
      self$dl(), self$d(), self$dr()
    )
  },
  n_adjacent_rolls = function() {
    sum(self$adjacent_rolls() == "@")
  },
  remove = function(i,j) {
    self$next_mat[i, j] <- "."
    return(invisible(self))
  },
  use_next = function() {
    self$mat <- self$next_mat
    return(invisible(self))
  },
  check_pos = function(i, j, n_allowed = 4) {
    remove = TRUE
    stopifnot(j <= ncol(self$mat))
    stopifnot(i <= nrow(self$mat))

    self$cur_i <- i
    self$cur_j <- j

    # only check roll positions
    if (self$c() != "@") {
      return(invisible(self))
    }

    if(self$n_adjacent_rolls() < 4) {
      self$n_safe <- self$n_safe + 1

      if (isTRUE(remove)) {
        self$remove(self$cur_i, self$cur_j)
      }

    }
    return(invisible(self))
  },
  check_row = function(i, ...) {
    for (j in seq(1, ncol(self$mat))) {
      self$check_pos(i, j = j, ...)
    }
    return(invisible(self))
  },
  check_all = function(...) {
    for (i in seq(1, nrow(self$mat))) {
      self$check_row(i = i, ...)
    }
  },
  mat = NULL,
  cur_i = NA_integer_,
  cur_j = NA_integer_,
  next_mat = NULL,
  n_safe = 0
))

part1 <- function(x) {
  s <- stack_r6$new(x)
  s$check_all()
  s$n_safe
}

print(part1(stack_mat))


# Part 2
part2 <- function(x) {
  s <- stack_r6$new(x)
  s$check_all()
  prev_safe <- s$n_safe
  s$use_next()
  while(TRUE) {
    s$check_all()
    #print(prev_safe)
    #print(s$n_safe)
    if (s$n_safe == prev_safe) {
      break
    }
    s$use_next()
    prev_safe <- s$n_safe
  }
  s$n_safe
}

print(part2(stack_mat))

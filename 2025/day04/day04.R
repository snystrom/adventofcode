library(magrittr)
library(purrr)

raw <- readLines("example.txt")
raw <- readLines("input.txt")

stack <- raw %>%
  strsplit("")

stack_mat <- matrix(unlist(stack), ncol = length(stack[[1]]), byrow = TRUE)


#adjacent_lt <- function(x, i, n_allowed = 4) {
#  left <- tail(x[1:i], -1)
#  right <- tail(x[i:length(x)], -1)
#
#  n_left <- length(left)
#  n_right <- length(right)
#
#  take_right <- 8 - n_left
#
#  if (is_empty(left)) {
#    left <- NA_character_
#  }
#  if (is_empty(right)) {
#    right <- NA_character_
#  }
#
#  adj_rolls <- sum((left == "@"), (head(right, take_right) == "@"), na.rm = TRUE)
#  adj_rolls < n_allowed
#}
#
#n_forkliftable <- function(x) {
#  lapply(seq_along(x), function(i) {
#    adjacent_lt(x, i)
#  })
#}

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

s <- stack_r6$new(stack_mat)
s$check_all()
s$n_safe

# Part 2
ss <- stack_r6$new(stack_mat)
ss$check_all()
prev_safe <- ss$n_safe
ss$use_next()
while(TRUE) {
  ss$check_all()
  #print(prev_safe)
  #print(ss$n_safe)
  if (ss$n_safe == prev_safe) {
    break
  }
  ss$use_next()
  prev_safe <- ss$n_safe
}
ss$n_safe

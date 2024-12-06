library(purrr)
library(combinat)
library(furrr)
options(scipen = 99)
plan("multicore", workers = 13)

eqns <- readLines("input.txt") %>%
  map(strsplit, ": ") %>%
  map(pluck, 1) %>%
  map(~{
    list(result = as.numeric(.x[1]),
         values = as.numeric(strsplit(.x[2], " ")[[1]])
         )
  })

get_perm <- function(values, n) {
  g <- expand.grid(replicate(n, values, simplify = FALSE))
  unname(as.matrix(g))
}

do_op <- function(x,y,op) {
  switch(op,
         "*" = x*y,
         "+" = x+y,
         "||" = as.numeric(paste0(x,y)),
         stop("unknown operator")
         )
}

do_ops <- function(values, ops){
  result <- values[1]

  for (i in seq_along(ops)) {
    result <- do_op(result, values[i+1,drop=FALSE], ops[i, drop=FALSE])
  }
  result
}

check_ops <- function(eqn, ops = c("*", "+")) {

  values <- eqn$values
  result <- eqn$result

  all_ops <- get_perm(ops, length(values) - 1)

  results <- apply(all_ops, 1, do_ops, values = values)
  any(result == results)
}


# Part 1
keep(eqns, check_ops) %>%
  map_dbl("result") %>%
  sum %>%
  print

# Part 2
p2 <- future_map_lgl(eqns, check_ops, c("*", "+", "||"))
eqns %>%
  keep(p2) %>%
  map_dbl("result") %>%
  sum %>%
  print

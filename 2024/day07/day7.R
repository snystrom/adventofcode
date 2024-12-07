library(purrr)
library(combinat)

#eqns <- readLines("example.txt") %>%
eqns <- readLines("input.txt") %>%
  map(strsplit, ":") %>%
  map(pluck, 1) %>%
  map(~{
    list(result = as.numeric(.x[1]),
         values = as.numeric(strsplit(gsub("^ ", "", .x[2]), " ")[[1]])
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

og_check_ops <- function(eqn) {

  values <- eqn$values
  result <- eqn$result

  all_ops <- get_perm(c("*", "+"), length(values) - 1)

  results <- apply(all_ops, 1, do_ops, values = values)
  result %in% results
}

# For debugging...
eval_ops <- function(eqn) {

  values <- eqn$values
  result <- eqn$result

  all_ops <- get_perm(c("*", "+"), length(values) - 1)

  results <- apply(all_ops, 1, do_ops, values = values)
  list(results = results,
       ops = all_ops)
}

check_ops <- function(eqn) {
  eqn$result %in% eval_ops(eqn)$results
}


# WRONG?!
keep(eqns, og_check_ops) %>%
  map_dbl("result") %>%
  sum


out <- map(eqns, ~{
  o <- eval_ops(.x)
  .x$results <- o$results
  .x$ops <- o$ops
  .x
})
out[[1]]$result %in% out[[1]]$results


map(out, ~{
  .x$ops[.x$result == .x$results,,drop=FALSE]
})

keeps <- map_lgl(out, ~{
  any(.x$result == .x$results)
})

map_dbl(out, "result") %>%
  keep(keeps) %>%
  sum

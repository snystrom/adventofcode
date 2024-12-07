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

check_ops <- function(eqn, ops_cache) {

  values <- eqn$values
  result <- eqn$result

  all_ops <- ops_cache[[length(values) - 1]]

  results <- apply(all_ops, 1, do_ops, values = values)
  any(result == results)
}

pmax <- map(eqns, "values") %>%
  map_int(length) %>%
  max

ops_cache <- map(seq(1,pmax), get_perm, values = c("*", "+"))

# Part 1
p1_check <- map_lgl(eqns, check_ops, ops_cache = ops_cache)

part1 <- keep(eqns, p1_check) %>%
  map_dbl("result") %>%
  sum
print(part1)

ops_cache_pt2 <- map(seq(1,pmax), get_perm, values = c("*", "+", "||"))

# Part 2
p2_eqns <- discard(eqns, p1_check)
system.time(p2_check <- future_map_lgl(p2_eqns, check_ops, ops_cache = ops_cache_pt2))
#    user  system elapsed
#772.144   3.210  91.672
p2_sum <- p2_eqns %>%
  keep(p2_check) %>%
  map_dbl("result") %>%
  sum

part2 <- part1 + p2_sum
print(part2)

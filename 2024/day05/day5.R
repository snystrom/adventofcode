library(magrittr)
library(purrr)
library(dplyr)

s <- readLines("input.txt")

rules <- s[1:which(s == "") - 1] %>%
  strsplit("\\|") %>%
  map_dfr(~{
    data.frame(first = as.integer(.x[1]),
               second = as.integer(.x[2]))
})

updates <- s[(which(s == "")+1):length(s)] %>%
  strsplit(",") %>%
  map(as.integer)

get_update_rules <- function(update) {
  rules %>%
    filter(first %in% update,
           second %in% update)
}

sort_with_rules <- function(u){
  # Build suffix array
  get_update_rules(u) %>%
    group_by(second) %>%
    mutate(n2 = n()) %>%
    group_by(first) %>%
    filter(n2 == min(n2)) %>%
    arrange(n2) %>%
    select(-n2) %>%
    # Collapse to sort
    as.matrix %>%
    as.vector %>%
    unique
}

update_valid <- function(u) {
  identical(u, sort_with_rules(u))
}

middle <- function(x) {
  x[ceiling(length(x) / 2)]
}

# Part 1
updates %>%
  keep(update_valid) %>%
  map_int(middle) %>%
  sum %>%
  print

# Part 2
updates %>%
  discard(update_valid) %>%
  map(sort_with_rules) %>%
  map_int(middle) %>%
  sum %>%
  print

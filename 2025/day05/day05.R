library(magrittr)
library(purrr)
library(dplyr)

options(scipen = 99)
raw <- readLines("example.txt")
raw <- readLines("input.txt")

food_ids <- as.numeric(tail(raw[which(raw == ""):length(raw)], -1))

#test_ids <- raw[1:which(raw == "") - 1] %>%
#  gsub("-", ":", .) %>%
#  map(~{eval(parse(text = .x))})
#test_ids[[1]] %in% test_ids[[2]]
# Damn... altrep vectors not implemented
# test_ids[[1]] %in% test_ids[[2]]
# Error in match(x, table, nomatch = 0L) :
#   long vectors not supported yet: ../../src/include/Rinlinedfuns.h:551

fresh_ids <- raw[1:which(raw == "") - 1] %>%
  strsplit(., "-") %>%
  map(~{
    as.numeric(.x)
  })

res <- map(food_ids, ~{
  id <- .x
  any(map_lgl(fresh_ids, ~{
    id >= .x[1] && id <= .x[2]
  }))
})

sum(unlist(res))

# Part2
is_contained <- function(x, y) {
  x_in_y <- (min(x) <= max(y) && max(x) <= max(y) && max(x) >= min(y))
  y_in_x <- (min(y) <= max(x) && max(y) <= max(x) && max(y) >= min(x))

  x_in_y || y_in_x
}

#expect_true(is_contained(c(50,100), c(50, 100)))
#expect_true(is_contained(c(51,99), c(50, 100)))
#expect_false(is_contained(c(49,49), c(50, 100)))
#expect_false(is_contained(c(50, 100), c(49,49)))

reduce_ranges <- function(x, y) {
  c(min(c(x, y)), max(c(x, y)))
}

#expect_equal(reduce_ranges(c(50,100), c(50, 100)), c(50, 100))
#expect_equal(reduce_ranges(c(51,99), c(50, 100)), c(50, 100))
#expect_equal(reduce_ranges(c(49,55), c(50, 100)), c(49, 100))

do_reduce <- function(id_list) {

  new_ranges <- list()

  for (candidate in id_list) {
    for (test_range in id_list) {
      if (is_contained(candidate, test_range)) {
        candidate <- reduce_ranges(candidate, test_range)
      }
    }
    new_ranges <- c(new_ranges, list(candidate))
  }
  unique(new_ranges)
}

reduce_id_list <- function(id_list) {
  ilen <- length(id_list)
  reduce_ids <- id_list
  while(TRUE) {
    reduce_ids <- do_reduce(reduce_ids)

    if (length(reduce_ids) == ilen) {
      break
    }
    ilen <- length(reduce_ids)
  }
  reduce_ids
}

reduce_ids <- reduce_id_list(fresh_ids)

map(reduce_ids2, ~{max(.x) - min(.x) + 1}) %>%
  unlist %>%
  sum


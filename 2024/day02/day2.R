library(purrr)
levels <- readLines("input.txt") %>%
  map(strsplit, split = " ") %>%
  map(pluck, 1) %>%
  map(as.integer)

is_safe <- function(row, drop = FALSE) {
  row_diff <- diff(row)
  max_diff <- max(abs(row_diff))
  safe <- max_diff > 0 && max_diff <= 3 && length(unique(sign(row_diff))) == 1

  if (!safe && drop) {
    safe <- any(map_lgl(1:length(row), ~{is_safe(row[-.x])}))
  }
  safe
}

# part 1
sum(map_lgl(levels, is_safe))

# part2
sum(map_lgl(levels, is_safe, drop = TRUE))

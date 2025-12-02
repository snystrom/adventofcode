library(magrittr)
library(readr)
library(purrr)

#raw <- read_file("example.txt")
raw <- read_file("input.txt")
ids <- raw %>%
  gsub("\n", "", .) %>%
  strsplit(",") %>%
  .[[1]] %>%
  strsplit("-") %>%
  map(as.numeric) %>%
  map(~{seq(.x[1], .x[2])})

all_ranges <- unlist(ids)

# only even number ranges can consist of all dups
even_ranges <- all_ranges[nchar(all_ranges) %% 2 == 0]

split_number <- function(x) {
  midpoint <- ceiling(nchar(x)/2)
  left <- substr(x, start = 1, stop = midpoint)
  right <- substr(x, start = midpoint+1, stop = nchar(x))
  as.integer(c(left, right))
}

invalid_idx <- map(even_ranges, split_number) %>%
  map_lgl(~{length(unique(.x)) == 1})

# Part 1
print(sum(even_ranges[invalid_idx]))

# Part 2
k_required <- seq(1, max(unique(nchar(all_ranges)) / 2))

check_k <- function(k, ranges) {
  # Check only ranges that can contain 2k
  valid_ranges <- ranges[nchar(ranges) >= k*2]
  range_strs <- as.character(valid_ranges)

  # "^([0-9]){k}\\1+$"
  regex_str <- paste0("^([0-9]{", k, "})\\1+$")
  matches <- grepl(regex_str, range_strs)
  return(valid_ranges[matches])
}

unique(unlist(map(k_required, check_k, ranges = all_ranges))) %>%
  sum %>%
  print

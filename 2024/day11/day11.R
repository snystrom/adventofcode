library(memoise)
library(purrr)

read_stones <- function(x) {
  strsplit(x, " ")[[1]]
}

split_half <- function(str) {
  h <- nchar(str) / 2
  res <- strsplit(str, "")[[1]]

  # Drop leading 0's - HACK
  as.character(
    as.integer(
      c(
        paste0(res[1:h], collapse = ""),
        paste0(res[(h+1):length(res)], collapse = "")
      )
    )
  )
}

blink_stone <- function(x) {

  if (x == "0") {
    return("1")
  }

  if (nchar(x) %% 2 == 0) {
    return(split_half(x))
  }

  return(as.character(as.numeric(x) * 2024))
}

blink_memo <- memoise(blink_stone)

# original for part 1, too slow for part 2
blink_stones <- function(stones, n=1) {

  for (i in 1:(n)) {
    stones <- unlist(purrr::map(stones, blink_memo))
  }
  stones
}

# use as input to blink_rle
stones_to_counts <- function(stones) {
  r <- rle(sort(stones))
  counts <- list()
  counts[r$values] <- r$lengths
  counts
}

# r is an rle(sort(o)) (or list with $values and $lengths)
blink_rle <- function(counts, n) {
  stopifnot(is.list(counts))

  next_counts <- list()
  for (niter in 1:n) {

    for (value in names(counts)) {
      bv <- blink_memo(value)

      for (v in bv) {
        nv <- next_counts[[v]] %||% 0
        next_counts[[v]] <- nv + counts[[value]]
      }

    }

    counts <- next_counts
    next_counts <- list()

  }

  counts
}

# Example of OG solution
# o <- blink_stones(read_stones("125 17"), 25)
# length(o) == 55312

# Example of fast solution
#o <- blink_rle(stones_to_counts(read_stones("125 17")), 25)
#sum(unlist(o)) == 55312

options(scipen = 999) # lol need scipen

p1 <- blink_rle(stones_to_counts(read_stones(input)), 25)
print(sum(unlist(p1)))

p2 <- blink_rle(stones_to_counts(read_stones(input)), 75)
print(sum(unlist(p2)))

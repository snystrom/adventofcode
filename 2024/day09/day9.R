library(purrr)
library(logger)
log_threshold(INFO)

#lines <- readLines("example.txt")
lines <- readLines("input.txt")
#lines <- "12345"
stopifnot("parse error" = length(lines) == 1)
disk_raw <- as.integer(strsplit(lines, "")[[1]])

block_size <- disk_raw[c(TRUE,FALSE)]
free_size <- disk_raw[c(FALSE,TRUE)]

file_idx <- seq(0,length(block_size) -1)

decompress <- function() {
  files <- map2_chr(file_idx, block_size, ~{paste0(rep(.x, .y), collapse = "")})
  free <- map_chr(free_size, ~{paste0(rep(".", .x), collapse = "")})
  paste0(
    head(files, 1),
    paste0(
      map2_chr(free, tail(files, -1), paste0, collapse = ""),
      collapse = ""
      ),
    collapse = ""
  )
}

disk_as_list <- function() {
  strsplit(decompress(), "")[[1]] %>%
    split(seq_along(.) - 1) %>%
    map(~{
      if (.x == ".") {
        return(NULL)
      }
      as.integer(.x)
    })
}


########
# Vector
disk <- disk_as_list()
disk_vec <- map_int(disk, ~{
  if(is.null(.x))
    return(NA_integer_)
  .x
}) %>% set_names(NULL)

free_pos <- is.na(disk_vec)
free_idx <- which(free_pos)

drop_idx <- rev(seq_along(disk_vec))[1:length(free_idx)]

move_vec <- disk_vec[drop_idx]
move_vec <- move_vec[!is.na(move_vec)]

disk_vec[head(free_idx, length(move_vec))] <- move_vec
disk_vec[drop_idx] <- NA

# 88100595464 - WRONG TOO LOW
disk_vec %>%
  set_names(seq(0,length(.) - 1)) %>%
  imap_int(~{
    .x * as.integer(.y)
  }) %>%
  sum(na.rm = T)

# 88100595464 - WRONG (same above)
map2_int(disk_vec, seq(0, length(disk_vec)-1), ~{
  .x * .y
}) %>%
  sum(na.rm = TRUE) %>%
  print

#####
# List (slower)

disk <- disk_as_list()
N <- as.character(length(disk))

free_pos <- unname(purrr::map_lgl(disk, is_empty))
free_idx <- which(free_pos)
val_idx <- rev(which(!free_pos))

for (i in seq_along(free_idx)) {
  if (i %% 20 == 0)
    log_info("{i} / {N}")

  free <- free_idx[i]
  val <- val_idx[i]

  if (val < free) # if we are at a free idx in the back half, we're done
    break

  if (is.na(val))
    break

  disk[[free]] <- disk[[val]]
  # NOTE: this may be a gotcha later. Inserting NULL drops stuff...
  disk[[val]] <- NULL
}


# WRONG 64165570696 - TOO LOW
# WRONG 88100595464 - (after impl the val < free check)
disk %>%
  discard(is_empty) %>%
  set_names(seq(0,length(.) - 1)) %>%
  imap_int(~{
    .x * as.integer(.y)
  }) -> x
  sum



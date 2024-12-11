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

node <- R6::R6Class("node",
                    public = list(
                      initialize = function(value) {
                        self$value <- value
                        invisible(self)
                      },
                      value = NULL,
                      nnext = NULL,
                      nprev = NULL
                    )
                    )

dll <- R6::R6Class("dll",
                  public = list(
                    initialize = function() {
                    self$head <- NULL
                    self$tail <- NULL
                    self$length <- 0
                    self
                  },
                  append = function(x) {
                    value <- node$new(x)

                    if (self$length == 0) {
                      self$head <- value
                      self$tail <- value
                    } else {
                      self$tail$nnext <- value
                      value$nprev <- self$tail
                      self$tail <- value
                    }

                    self$length <- self$length + 1
                    invisible(self)
                  },
                  get = function(i) {
                    cur <- self$head
                    if (i > 1) {
                      for (n in 2:i) {
                        cur <- cur$nnext
                      }
                    }
                    cur
                  },
                  get_value = function(i) {
                    self$get(i)$value
                  },
                  first_empty_idx = function(start = 1) {
                    for (i in start:self$length) {
                      v <- self$get_value(i)
                      if (is.null(v))
                        break
                    }
                    return(i)
                  },
                  last_filled_idx = function(start=self$length) {
                    for (i in start:1) {
                      v <- self$get_value(i)
                      if (!is.null(v))
                        break
                    }
                    return(i)
                  },
                  swap = function(i,j) {
                    cur_i <- self$get(i)
                    cur_j <- self$get(j)

                    iv <- cur_i$value
                    cur_i$value <- cur_j$value
                    cur_j$value <- iv

                    invisible(self)
                  },
                  defrag = function() {
                    first_empty <- self$first_empty_idx()
                    last_filled <- self$last_filled_idx()

                    while (first_empty < last_filled) {
                      log_debug("empty: {first_empty}, value: {last_filled}")
                      self$swap(first_empty, last_filled)
                      first_empty <- self$first_empty_idx(first_empty)
                      last_filled <- self$last_filled_idx(last_filled)
                    }

                    invisible(self)
                  },
                  read_all = function(){
                    values <- c()
                    cur <- self$head
                    for (i in 1:self$length) {
                      values[i] <- cur$value %||% NA_integer_ # HACK
                      cur <- cur$nnext
                    }
                    values
                  },
                  head = NULL,
                  tail = NULL,
                  length = 0
                  )
                  )

disk <- dll$new()
walk(disk_as_list(), ~{
  disk$append(.x)
})
#dLL$read_all()
system.time(
  disk$defrag()
)
#     user    system   elapsed
#92214.667    14.645 92271.183

# 88100595464 - WRONG NOOO!
disk$read_all() %>%
  set_names(seq(0,length(.) - 1)) %>%
  imap_int(~{
    .x * as.integer(.y)
  }) %>%
  sum(na.rm = TRUE)

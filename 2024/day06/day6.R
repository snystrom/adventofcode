library(magrittr)
library(rlang)
library(R6)
library(logger)
library(purrr)

#x <- readLines("input.txt") %>%
x <- readLines("example.txt") %>%
  strsplit("")
mat <- matrix(unlist(x), nrow = length(x), byrow = TRUE)

guard <- R6Class("guard",
  public = list(
    initialize = function(m) {
      guard_pos <- which(m == "^", arr.ind = TRUE)
      i <- guard_pos[1]
      j <- guard_pos[2]
      self$current_direction <- "u"
      self$start_direction <- "u"
      self$start_i <- i
      self$start_j <- j
      private$mat <- m
      private$cur_i <- i
      private$cur_j <- j
      private$init_i <- i
      private$init_j <- j
      private$ncol <- ncol(m)
      private$nrow <- nrow(m)

      self$visit() # visit starting position!
      #private$mat[i, j] <- "." # set start as empty for stepping later
      return(self)
    },
    # record a visit
    # record current position & direction
    visit = function() {
      i <- private$cur_i
      j <- private$cur_j
      private$visited <- c(private$visited, list(list(position = c(i,j),
                                                 direction = self$current_direction
                                                 )))
      self
    },
    m = function() {
      private$mat
    },
    show = function() {
      m <- private$mat
      m[self$i(), self$j()] <- paste0("O", self$current_direction)
      m
    },
    i = function() {
      private$cur_i
    },
    j = function() {
      private$cur_j
    },
    out_of_bounds = function() {
      if (self$i() > private$nrow || self$i() <= 0) {
        return(TRUE)
      }
      if (self$j() > private$ncol || self$j() <= 0) {
        return(TRUE)
      }

      return(FALSE)
    },
    peek = function() {
      if (self$out_of_bounds()) {
        return(NULL)
      }
      self$m()[self$i(), self$j()]
    },
    # returns "" instead of NULL
    peek_to = function(direction) {
      on.exit(self$reset())
      self$scan_to(direction)$peek() %||% ""
    },
    move = function(i, j) {
      private$cur_i <- i
      private$cur_j <- j
      self
    },
    u = function() {
      self$move(self$i() - 1, self$j())
    },
    d = function() {
      self$move(self$i() + 1, self$j())
    },
    l = function() {
      self$move(self$i(), self$j() - 1)
    },
    r = function() {
      self$move(self$i(), self$j() + 1)
    },
    ul = function() {
      self$move(self$i() - 1, self$j() - 1)
    },
    ur = function() {
      self$move(self$i() - 1, self$j() + 1)
    },
    dl = function() {
      self$move(self$i() + 1, self$j() - 1)
    },
    dr = function() {
      self$move(self$i() + 1, self$j() + 1)
    },
    reset = function() {
      self$move(private$init_i, private$init_j)
    },
    next_rotation = function() {
      switch(self$current_direction,
             "u" = "r",
             "r" = "d",
             "d" = "l",
             "l" = "u",
             stop("unknown direction")
             )
    },
    rotate = function() {
      self$current_direction <- self$next_rotation()
      self
    },
    # have we visited this cursor position before w/ current direction?
    in_loop = function() {
      i <- self$i() # cast so identical() check works below
      j <- self$j()

      # Find prev paths
      prev_idx <- self$visited_locations() %>%
        map("position") %>%
        map_lgl(~{all(.x == c(i,j))})

      # If would rotate into prev direction @ path
      prev_dirs <- self$visited_locations()[prev_idx] %>%
        transpose() %>%
        .$direction %>%
        unlist

      self$current_direction %in% prev_dirs
    },
    # Would we loop if an object was here?
    would_loop = function() {
      init_dir <- self$current_direction
      # can't rely on reset
      i_i <- self$i()
      i_j <- self$j()
      on.exit({
        # Reset direction
        self$current_direction <- init_dir
        private$cur_i <- i_i
        private$cur_j <- i_j
      })

      self$rotate() # hallucinate an obstacle
      in_loop <- self$in_loop()
      while (!in_loop) {
        if (is.null(self$step(commit=FALSE)))
          break
        in_loop <- self$in_loop()
      }

      in_loop
    },
    # commit = FALSE is similar to scanning w/ the step logic
    step = function(commit=TRUE) {
      next_tile <- self$scan_to(self$current_direction)$peek()

      BLOCKED <- "#"
      EMPTY <- c(".", "^")

      if (is.null(next_tile)) {
        log_debug("DONE")
        return(NULL)
      }

      # Blocked
      if (next_tile %in% BLOCKED) {
        self$reset()$rotate()
      }

      # Empty
      if (next_tile %in% EMPTY) {
      #if (next_tile == ".") {
        # Check if you would loop
        # TODO:
        #self$would_loop(self$i(), self$j())

        if (commit) {
          if (self$would_loop()) {
            self$n_loop <- self$n_loop + 1
          }
          # Commit the step
          self$visit()
          private$init_i <- self$i()
          private$init_j <- self$j()
        }
      }

      self
    },
    scan_to = function(direction = c("ul", "ur", "dl", "dr", "u", "d", "l", "r")) {
      direction <- match.arg(direction)
      switch(direction,
        "ul" = self$ul(),
        "ur" = self$ur(),
        "dl" = self$dl(),
        "dr" = self$dr(),
        "u" = self$u(),
        "d" = self$d(),
        "r" = self$r(),
        "l" = self$l(),
        stop("Unknown direction")
      )
    },
    patrol = function() {
      while (!is.null(self$step()))
        next
    },
    patrol_to = function(i,j) {
      while (!is.null(self$step())) {
        if (identical(c(i,j), self$i(), self$j()))
            break
        next
      }
    },
    visited_locations = function(include_start = FALSE) {
      v <- private$visited
      if (include_start) {
        v <- c(list(list(position = c(self$start_i,
                                 self$start_j
                                 ),
                    direction = self$start_direction
                    )
               ),
          v
          )
      }
      v
    },
    part1 = function() {
      self$visited_locations() %>%
        map("position") %>%
        map_chr(paste0, collapse=",") %>%
        unique %>%
        length
    },
    current_direction = NULL,
    n_loop = 0,
    start_i = NULL,
    start_j = NULL,
    start_direction = NULL
  ),
  private = list(
    mat = NULL,
    ncol = NULL,
    nrow = NULL,
    init_i = NULL,
    init_j = NULL,
    cur_i = NULL,
    cur_j = NULL,
    visited = list()
  )
)

g <- guard$new(mat)

g$patrol()
print(g$part1())

g$step()$step()$show()


g$patrol_to(7,5)
g$n_loop
g <- guard$new(mat)
g$patrol()
g$.__enclos_env__$private$cur_i <- 7
g$.__enclos_env__$private$cur_j <- 7
g$current_direction <- "l"
self <- g
g

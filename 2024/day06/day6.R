library(magrittr)
library(rlang)
library(R6)
library(logger)
library(purrr)

library(furrr)
plan("multicore", workers = 13)

logger::log_threshold(DEBUG)

x <- readLines("input.txt") %>%
#x <- readLines("input2.txt") %>%
#x <- readLines("example.txt") %>%
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
      self$path <- list()
      private$cur_i <- i
      private$cur_j <- j
      private$init_i <- i
      private$init_j <- j
      private$ncol <- ncol(m)
      private$nrow <- nrow(m)


      self$init_visited_mat()
      self$init_loop_check_mat()

      self$visit() # visit starting position!
      #private$mat[i, j] <- "." # set start as empty for stepping later
      return(self)
    },
    # blank bool matrix
    blank_visited_mat = function() {
      im <- matrix(FALSE, ncol = ncol(self$m()), nrow = nrow(self$m()))
      list("u" = im,
           "l" = im,
           "r" = im,
           "d" = im
           )
    },
    init_visited_mat = function() {
      private$visit_loc <- self$blank_visited_mat()
      self
    },
    init_loop_check_mat = function() {
      private$loop_loc <- self$blank_visited_mat()[[1]]
      private$tested_loop_loc <- self$blank_visited_mat()[[2]]
      self
    },
    # go back to start
    restart = function(clear=FALSE) {
      self$move(self$start_i, self$start_j)
      self$current_direction <- "u"
      private$init_i <- self$i()
      private$init_j <- self$j()
      self$n_loop <- 0

      if (clear) {
        self$init_visited_mat()
        self$init_loop_check_mat()
      }

      self
    },
    visited_positions = function() {
      private$visit_loc$u |
      private$visit_loc$l |
      private$visit_loc$r |
      private$visit_loc$d
    },
    # check if visited
    visited = function() {

      if (self$out_of_bounds()) {
        return(FALSE)
      }
      private$visit_loc[[self$current_direction]][self$i(), self$j()]
    },
    visit = function() {
      private$visit_loc[[self$current_direction]][self$i(), self$j()] <- TRUE
      self
    },
    m = function() {
      private$mat
    },
    show = function() {
      m <- private$mat
      m[self$i(), self$j()] <- paste0("G", self$current_direction)
      m
    },
    # show with path
    displ = function(loops=TRUE) {
      m <- self$show()
      horiz <- (private$visit_loc$l | private$visit_loc$r) & !(private$visit_loc$u | private$visit_loc$d)
      vert <- !(private$visit_loc$l | private$visit_loc$r) & (private$visit_loc$u | private$visit_loc$d)
      both <- self$visited_positions() & !(horiz | vert)
      #loop <- private$loop_loc$d |
      #  private$loop_loc$l |
      #  private$loop_loc$r |
      #  private$loop_loc$u
      loop <- private$loop_loc
      m[horiz] <- "-"
      m[vert] <- "|"
      m[both] <- "+"
      if (loops)
        m[loop] <- "O"
      #m[self$i(), self$j()] <- paste0("G", self$current_direction)
      m[self$i(), self$j()] <- self$current_direction
      cat("\n")
      walk(apply(m,1, paste0, collapse=""), cat, "\n")
      cat("\n")

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
      on.exit(self$reset(), add = TRUE)
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
    og_in_loop = function() {
      i <- self$i()
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
    in_loop2 = function() {

      i <- self$i() # cast so identical() check works below
      j <- self$j()

      path_tile <- self$path[[toString(c(i,j))]] %||% ""
      self$current_direction %in% path_tile
    },
    in_loop = function() {
      self$visited()
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
      }, add = TRUE)

      # Get location of hypothetical obstacle
      self$scan_to(self$current_direction)

      # Obstacles are not allowed past edges,
      # so we're not in a loop
      if (self$out_of_bounds()) {
        self$reset()
        return(FALSE)
      }

      # NOTE: i think step() is already handling this, but in case, let's not mark existing
      # obstacles
      if (self$peek() == "#") # only checking loops w/ new obstacles
        return(FALSE)

      ob_i <- self$i()
      ob_j <- self$j()
      self$reset()

      log_trace("SIMULATING OBJSTACLE AT: {ob_i}, {ob_j}")
      #if (ob_i == 7 && ob_j == 4) {
      #  browser()
      #}

      # Skip if this is the start position (forbidden)
      if (self$start_i == ob_i && self$start_j == ob_j){
        self$reset()
        return(FALSE)
      }

      # Skip if already tested (we can't insert an obstacle after we pass it)
      if (isTRUE(private$tested_loop_loc[ob_i, ob_j])) {
        self$reset()
        return(FALSE)
      }

      self$rotate() # hallucinate an obstacle
      #if (i_i == 21 && i_j == 79 && init_dir == "l")
      #  browser()

      log_debug("check loop at: {i_i}, {i_j}, {init_dir}->{d}", d = self$current_direction)

      in_loop <- self$in_loop()
      while (!in_loop) {
        if (is.null(self$step(commit=FALSE))) {
          in_loop <- self$in_loop()
          log_debug("break! in loop? {in_loop}")
          break
        }
        #in_loop <- self$in_loop2()
        in_loop <- self$in_loop()
      }

      # If this path loops, record it as visited
      if (in_loop) {
        #log_debug("SAVE LOOP OBJ")
        private$loop_loc[ob_i, ob_j] <- TRUE
      }

      log_debug("In loop? {in_loop}")
      self$reset()
      # record that we tested
      private$tested_loop_loc[ob_i, ob_j] <- TRUE
      return(in_loop)
    },
    # commit = FALSE is similar to scanning w/ the step logic
    step = function(commit=TRUE, check_loop=FALSE) {
      next_tile <- self$scan_to(self$current_direction)$peek()
      logger::log_trace("step to: {i}, {j}", i = self$i(), j = self$j())

      BLOCKED <- "#"
      EMPTY <- c(".", "^")

      if (is.null(next_tile)) {
        log_debug("DONE")
        return(NULL)
      }

      # Blocked
      # blocked tiles cannot be visited so these will always be len=1
      if (next_tile %in% BLOCKED) {
        self$reset()$rotate()
        if (commit)
          self$visit() # record both directions as visited @ cur
      }

      # Empty
      if (next_tile %in% EMPTY) {

        if (commit) {
          # Commit the step
          self$visit()
          private$init_i <- self$i()
          private$init_j <- self$j()

          if (check_loop) {
            if (self$would_loop()) {
                self$n_loop <- self$n_loop + 1
            }
          }
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
    patrol = function(check_loop=FALSE) {
      while (!is.null(self$step(check_loop = check_loop)))
        next
    },
    patrol_brute_force = function() {
      i <- 0
      start_check <- TRUE
      do_check <- FALSE

      size <- ncol(self$m()) * nrow(self$m())
      max_size <- size / 2
      while (!is.null(self$step())) {

        if (i > max_size) {
          log_info("Break On iters: {i}, assume loop")
          self$n_loop <- self$n_loop + 1
          break
        }

        # Really dumb hack
        # this is not guaranteed to be correct
        if (i > path_length && start_check) {
          check_i <- self$i()
          check_j <- self$j()
          check_dir <- self$current_direction
          start_check <- FALSE
          do_check <- TRUE
          seen_i <- 1
          seen_j <- 1
        } else if (do_check) {
          if (self$i() == check_i && self$j() == check_j && self$current_direction == check_dir) {
            seen_i <- seen_i + 1
            seen_j <- seen_j + 1
          }

          if (seen_i > 1 && seen_j > 1) {
            self$n_loop <- self$n_loop + 1
            break
          }
        }

        i <- i +1
      }
      log_info("COMPLETE: {x}", x = self$part1())
      self
    },
    patrol_draw = function() {
      while (!is.null(self$step())) {
        print(self$show())
        next
      }
    },
    patrol_to = function(i,j, ...) {
      while (TRUE) {
        s <- self$step(...)

        log_debug("{i}, {j}", i=self$i(), j=self$j())
        if (self$i() == i && self$j() == j) {
          break
        }

        if (is.null(s)) {
          log_debug("break")
          break
        }
      }
      self
    },
    visited_locations = function() {
      private$visit_loc
    },
    part1 = function() {
      self$visited_positions() %>%
        sum()
    },
    current_direction = NULL,
    n_loop = 0,
    start_i = NULL,
    start_j = NULL,
    start_direction = NULL,
    # holds the path taken
    path = NULL
  ),
  private = list(
    mat = NULL,
    ncol = NULL,
    nrow = NULL,
    init_i = NULL,
    init_j = NULL,
    cur_i = NULL,
    cur_j = NULL,
    visit_loc = NULL,
    loop_loc = NULL,
    tested_loop_loc = NULL
  )
)


g <- guard$new(mat)
g$patrol(T) # 2848 - WRONG
g$n_loop



g <- guard$new(mat)
g$patrol()

# Get path indices (minus start loc)
vp <- g$visited_positions()
vp[g$start_i, g$start_j] <- FALSE
visit_minus_start <- which(vp, arr.ind = TRUE)
test_mats <- map(split(visit_minus_start, row(visit_minus_start)), ~{
                 m <- mat
                 m[.x[[1]], .x[[2]]] <- "#"
                 m
})
path_length <- g$part1()
# NOTE: this won't be right, because it
# is only testing in the path, not alongside,
# ex pos+1 for the "walls" of the path
#
# For reference, this gives 1663,
# seems too low.
log_threshold(INFO)
system.time(
res <- future_map(test_mats, ~{
  brute_guard <- guard$new(.x)
  brute_guard$patrol_brute_force()
  brute_guard$reset()
})
)

# Part2 lol that worked and it was correct w/o the maze wall thing, too.
# I never tried it cause I was rate limited
map_int(res, "n_loop") %>%
  sum



mw <- get_maze_walls(g$visited_positions())
mm <- g$m()
mm[which(mw, arr.ind = T)] <- "="
mm

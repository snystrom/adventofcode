library(magrittr)
library(rlang)
library(R6)
library(logger)

x <- readLines("input.txt") %>%
  strsplit("")
mat <- matrix(unlist(x), nrow = length(x), byrow = TRUE)

pos <- R6Class("pos",
        public = list(
          initialize = function(m, i = 1, j = 1) {
            private$mat <- m
            private$cur_i <- i
            private$cur_j <- j
            private$init_i <- i
            private$init_j <- j
            private$ncol <- ncol(m)
            private$nrow <- nrow(m)
            return(self)
          },
          m = function() {
            private$mat
          },
          i = function() {
            private$cur_i
          },
          j = function() {
            private$cur_j
          },
          out_of_bounds = function() {
            if (self$i() > private$nrow || self$i() <= 0)
              return(TRUE)
            if (self$j() > private$ncol || self$j() <= 0)
              return(TRUE)

            return(FALSE)
          },
          peek = function() {
            if (self$out_of_bounds())
              return(NULL)
            self$m()[self$i(), self$j()]
          },
          # returns "" instead of NULL
          peek_to = function(direction) {
            on.exit(self$reset())
            self$scan_to(direction)$peek() %||% ""
          },
          move = function(i,j) {
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
          i_next = function() {
            i_next <- ifelse(self$j_next() == 1, # if j_next is 1 we're wrapping
                   self$i() + 1,
                   self$i())
            if (i_next > nrow(self$m()))
              return(NULL)
            i_next
          },
          j_next = function() {
            j_next <- ifelse(self$j() + 1 > ncol(self$m()),
                   1,
                   self$j() + 1)
            j_next
          },
          step = function() {
            j_next <- self$j_next()
            i_next <- self$i_next()

            if (is.null(i_next)) {
              return(NULL)
            }

            private$cur_i <- i_next
            private$cur_j <- j_next
            private$init_i <- i_next
            private$init_j <- j_next

            log_debug("step to: {i_next}, {j_next}")

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
          # Scan pattern for part 1
          scan_cur_pt1 = function() {

            if (self$peek() != "X")
              return(self)

            mas <- c("M", "A", "S")
            for (direction in c("dl", "dr", "ul", "ur", "u", "d", "l", "r")) {
              log_debug("{direction}")
              in_match <- FALSE
              for (letter in mas) {
                l <- self$scan_to(direction)$peek() %||% ""
                log_debug("{letter}? : {l}")
                in_match <- letter == l
                if (isTRUE(in_match)) {
                  log_debug("next")
                  next
                } else {
                  log_debug("break")
                  in_match <- FALSE
                  break
                }
              }
              if (isTRUE(in_match)) {
                log_debug("match found!")
                self$n_xmas <- self$n_xmas + 1
              } else {
                log_debug("no match, next direction")
              }
              in_match <- FALSE
              self$reset()
            }
            self$reset()
          },
          scan_all_pt1 = function() {
            while (TRUE) {
              self$scan_cur_pt1()
              if (is.null(self$step())) {
                break
              }
              log_debug("next position")
            }

            self
         },
          # Scan pattern for part 2
          scan_cur_pt2 = function(save_match = FALSE) {

            if (self$peek() != "A")
              return(self)

            in_match <- FALSE
            letters <- list("M" = 0, "S" = 0)
            ul <- NULL
            for (direction in c("ul", "dr", "ur", "dl")) {
              log_debug(direction)
              L <- self$peek_to(direction)
              in_match <- L %in% c("M", "S")

              if (direction == "ul") {
                ul <- L
              }

              # If opposite corners match, break
              if (direction == "dr") {
                if (ul == L) {
                  in_match <- FALSE
                }
              }

              if (!in_match) {
                break
              }

              # Early exit if we are imbalanced
              letters[[L]] <- letters[[L]] + 1
              if (any(unlist(letters) > 2)) {
                in_match <- FALSE
                break
              }
            }

            if (in_match) {
              log_debug("match found!")
              if (save_match) {
                self$match_p2 <- c(self$match_p2, paste0(self$i(),", ", self$j()))
              }
              self$n_pt2 <- self$n_pt2 + 1
            } else {
              log_debug("no match")
            }
            self$reset()
          },
          scan_all_pt2 = function() {
            while (TRUE) {
              self$scan_cur_pt2()
              if (is.null(self$step())) {
                break
              }
              log_debug("next position")
            }
            self
         },
         scan_all = function() {
           self$move(1,1)
           self$scan_all_pt1()
           self$move(1,1)
           self$match_p2 <- list()
           self$scan_all_pt2()
           self
         },
         # view square for pt2 around center
         view_square = function(i,j) {
           ir <- (i-1):(i+1)
           jr <- (j-1):(j+1)
           self$m()[ir, jr]
         },
         # purrr::map(head(unlist(p$match_p2)), p$view_square_str)
         view_square_str = function(x) {
           s <- strsplit(x, ", ")[[1]]
           self$view_square(as.integer(s[1]), as.integer(s[2]))
         },
         part1 = function() {
           self$n_xmas
         },
         part2 = function() {
           self$n_pt2
         },
         n_xmas = 0,
         n_pt2 = 0,
         match_p2 = list()
        ),
        private = list(
          mat = NULL,
          ncol = NULL,
          nrow = NULL,
          init_i = NULL,
          init_j = NULL,
          cur_i = NULL,
          cur_j = NULL
        )

)


s <- pos$new(mat)$scan_all()
print(s$part1())
print(s$part2())

library(magrittr)
library(purrr)

#raw <- readLines("example.txt")
# with proper spaces as trailers
raw <- readLines("e2.txt")
#raw <- readLines("input.txt")
raw <- readLines("i2.txt")

ops <- tail(raw, 1) %>%
  gsub(" +", ",", .) %>%
  strsplit(., ",") %>%
  .[[1]]
nums <- head(raw, -1)

mat <- map(nums, ~{
  gsub(" +", ",", .x) %>%
    gsub("^,", "", .) %>%
    strsplit(., ",") %>%
    .[[1]] %>%
    as.integer
    #as.numeric(strsplit(., ",")[[1]])
}) %>%
  unlist %>%
   matrix(., ncol = length(ops), byrow = TRUE)

numbers <- t(mat)

x <- list()
for (i in seq(1, nrow(numbers))) {
  print(i)
  out <- eval(parse(text = paste0(numbers[i,], collapse = ops[i])))
  print(out)
  x <- c(x, list(out))
}
options(scipen = 99)
sum(unlist(x))

# Part2

nn <- map(nums, ~{
  gsub(" +", ",", .x) %>%
    gsub("^,", "", .) %>%
    strsplit(., ",") %>%
    .[[1]] %>%
    as.integer
    #as.numeric(strsplit(., ",")[[1]])
})

num_split <- nums %>%
  map(~{strsplit(.x, "") %>% .[[1]]})

num_ncol <- unique(map_int(num_split, length))

num_raw_mat <- num_split %>%
  unlist() %>%
   matrix(., ncol = num_ncol, byrow = TRUE)

break_points <- which(apply(num_raw_mat, 2, function(x) {all(x == " ")}))
break_points <- c(1, break_points-1)

x <- num_raw_mat %>%
  t() %>%
  apply(., 1, function(x) {paste0(x, collapse = "")}, simplify = F) %>%
  unlist %>%
  as.numeric

xc <- as.character(x)
xc[is.na(xc)] <- "\n"
nl <- paste0(xc, collapse = ",")
writeLines(nl, "o.txt")
nll <- readLines("o.txt")
nll %>%
  gsub("^,", "", .) %>%
  gsub(",$", "", .) %>%
  strsplit(., ",") %>%
  map(as.integer) %>%
  map2(., ops, ~{
    eval(parse(text = paste0(.x, collapse = .y)))
  }) %>%
  unlist %>%
  sum


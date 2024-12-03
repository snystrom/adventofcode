string <- readLines("input.txt")

# Part 1
string %>%
  paste0(collapse = "") %>%
  regmatches(., gregexpr("mul\\([0-9]{1,3},[0-9]{1,3}\\)", .)) %>%
  .[[1]] %>%
  gsub(",", "*", .) %>%
  gsub("mul", "", .) %>%
  paste0(collapse = "+") %>%
  parse(text = .) %>%
  eval %>%
  print

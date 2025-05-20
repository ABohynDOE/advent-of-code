# Read input data ----
input <- readLines("2024/04-input-test-1")

# Part 1 ----

# library(stringr)
# library(purrr)
# 
# horizontal <- str_flatten(input)
# h_fwd <- str_count(horizontal, "XMAS") # 3
# h_bwd <- str_count(horizontal, "SAMX") # 2
# 
# m <- do.call(rbind, strsplit(input, ""))
# n <- nrow(m)
# vertical <- m |>
#   asplit(MARGIN = 2) |>
#   lapply(str_flatten) |>
#   str_flatten()
# 
# v_fwd <- str_count(vertical, "XMAS") # 1
# v_bwd <- str_count(vertical, "SAMX") # 2
# 
# max_d <- n - 4
# diagonal1 <- map(-max_d:max_d, ~ m[row(m) == col(m) + .x]) |>
#   map(str_flatten)
# 
# d1_fwd <- diagonal1 |>
#   map(~ str_count(.x, "XMAS")) |>
#   unlist() |>
#   sum() # 1
# d1_bwd <- diagonal1 |>
#   map(~ str_count(.x, "SAMX")) |>
#   unlist() |>
#   sum() # 4
# 
# diagonal2 <- map(-max_d:max_d, ~ m[row(m) == (n + 1 - col(m) + .x)]) |>
#   map(str_flatten)
# 
# d2_fwd <- diagonal2 |>
#   map(~ str_count(.x, "XMAS")) |>
#   unlist() |>
#   sum() # 4
# d2_bwd <- diagonal2 |>
#   map(~ str_count(.x, "SAMX")) |>
#   unlist() |>
#   sum() # 1
# 
# total <- v_fwd + v_bwd + h_fwd + h_bwd + d1_bwd + d1_fwd + d2_fwd +
#   d2_bwd
# print(total)

read_matrix <- function(path, sep = "", fill = NA, type = identity) {
  lines <- readLines(path)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(fill, nrow = length(lines), ncol = max(token_lengths))
  
  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

input <- read_matrix("2024/day-04/04-input")

up <- apply(input, 1, identity, simplify = FALSE)
down <- lapply(up, rev)
left <- apply(input, 2, identity, simplify = FALSE)
right <- lapply(left, rev)

nrow <- nrow(input)

diag1 <- purrr::map(seq(-nrow - 1, nrow - 1), \(i) input[row(input) == (col(input) + i)])
diag2 <- purrr::map(seq(2, nrow * 2), \(i) input[row(input) + col(input) == i])

diag3 <- lapply(diag1, rev)
diag4 <- lapply(diag2, rev)

c(left, right, up, down, diag1, diag2, diag3, diag4) |>
  purrr::map_chr(\(x) paste(x, collapse = "")) |>
  stringr::str_count("XMAS") |>
  sum()

# Part 2 ----

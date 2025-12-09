library(stringr)

# Read input data ----
input <- readLines("2025/day-02/02-input")

input |>
  str_split_1(",") |>
  lapply(str_split, "-") |>
  unlist(recursive = FALSE) |>
  purrr::map(\(x) seq(x[[1]], x[[2]])) |>
  unlist() |>
  as.character() |>
  str_subset("^(\\d+)(\\1)$") |>
  as.numeric() |>
  sum()


input |>
  str_split_1(",") |>
  lapply(str_split, "-") |>
  unlist(recursive = FALSE) |>
  purrr::map(\(x) seq(x[[1]], x[[2]])) |>
  unlist() |>
  as.character() |>
  str_subset("^(\\d+)(\\1)+$") |>
  as.numeric() |>
  sum()

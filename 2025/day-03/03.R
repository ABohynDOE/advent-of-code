# Read input data ----
input <- readLines("2025/day-03/03-input") |>
  str_split("")

max_joltage <- function(x) {
  l <- length(x)
  m <- max(x[-l], na.rm = TRUE)
  first_max <- which(x == m)[1]
  x2 <- x[-seq(1, first_max)]
  second_max <- max(x2, na.rm = TRUE)
  str_c(x[first_max], second_max) |>
    as.numeric()
}

# Part 1 ----

purrr::map_dbl(input, max_joltage, .progress = T) |>
  sum()

# Part 2 ----

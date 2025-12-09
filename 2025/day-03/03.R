# Read input data ----
input <- readLines("2025/day-03/03-input") |>
  str_split("")

max_joltage <- function(i) {
  x <- i[[1]]
  l <- length(x)
  m <- max(x[-l], na.rm = TRUE)
  first_max <- which(x == m)[1]
  x2 <- x[-seq(1, first_max)]
  second_max <- max(x2, na.rm = TRUE)
  num <- str_c(x[first_max], second_max) |> as.numeric()
}

# Part 1 ----

purrr::map(input, max_joltage, .progress = T)

# Part 2 ----

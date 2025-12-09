# Read input data ----

input <- readLines("2025/day-01/01-input")[-4318]
combinations <- input |>
  stringr::str_replace_all(c("L" = "-", "R" = "")) |>
  as.numeric()

# Part 1 ----

total <- cumsum(c(50, combinations)) %% 100
n_zeroes <- sum(total == 0, na.rm = TRUE)

# Part 2 ----

position <- 50
n_zeroes2 <- 0
for (i in combinations) {
  for (j in seq_len(abs(i))) {
    position <- (position + sign(i)) %% 100
    if (position == 0) {
      n_zeroes2 <- n_zeroes2 + 1
    }
  }
}

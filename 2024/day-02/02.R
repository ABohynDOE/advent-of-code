
# Read input data ----
input <- readLines("2024/02-input") |>
  strsplit(" ") |>
  lapply(as.integer)
input <- input[-1001]

# Part 1 ----

safe <- function(x) {
  diffs <- diff(x)
  inc_or_dec <- all(diffs > 0) || all(diffs < 0)
  tolerable <- max(abs(diffs)) >= 1 && max(abs(diffs)) <= 3
  inc_or_dec && tolerable
}
safe_reports <- lapply(input, safe) |>
  unlist() |> 
  sum()
print(safe_reports)

# Part 2 ----

safe_skipped <- function(x) {
  is_safe <- FALSE
  for (i in seq_along(x)) {
    is_safe <- safe(x[-i])
    if (is_safe) break
  }
  is_safe
}

safe_reports2 <- lapply(input, safe_skipped) |>
  unlist() |>
  sum()
print(safe_reports2)

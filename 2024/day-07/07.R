library(stringr)
library(rlang)
library(purrr)

# Read input data ----
input <- readLines("2024/day-07/07-input")

# Part 1 ----

# Isolate the final results
results <- str_extract(input, "(\\d+)(?=:.+)") |>
  map(as.numeric)

# Isolate the numbers for the equations
operations <- str_extract(input, "(?<=\\d:\\s)(\\d\\s?)+") |>
  map(~ strsplit(.x, " ")[[1]])

format_operation <- function(num_vec, result) {
  n_nums <- length(num_vec)
  padded_nums <- paste0(num_vec, sep = ")")
  start_parenthesis <- paste0(rep("(", n_nums), collapse = "")
  possible_operators <- gtools::permutations(
    n = 2,
    r = (n_nums - 1),
    v = c("*", "+"),
    repeats.allowed = TRUE
  ) |> asplit(MARGIN = 1)
  equations <- map(.x = possible_operators,
                   .f = ~ paste0(
                     start_parenthesis,
                     paste(padded_nums, c(.x, ""), collapse = " ") |> trimws()
                   ))
  results <- map(equations, ~ parse_expr(.x) |> eval())
  correct <- map(results, ~ .x == result) |>
    unlist()
  any(correct)
}

correct_eq <- map2(
  .x = operations,
  .y = results,
  ~ format_operation(.x, .y),
  .progress = TRUE
)
total <- unlist(results)[unlist(correct_eq)] |>
  sum()

# Part 2 ----

# Compute the number of elements per equation
n_elements <- map(operations, length)

# Compute all possible permutations for each equation
permutations <- map(
  .x = n_elements,
  .f = ~ gtools::permutations(
    n = 3,
    r = .x - 1,
    v = c(')*', ')+', ''),
    repeats.allowed = TRUE
  ) |>
    asplit(MARGIN = 1)
)

# Compute number of permutations per list
n_perm <- map(n_elements, ~ 2 ^ (.x - 1))

# Flatten the list of lists into a single list
operators <- do.call(list, unlist(permutations, recursive = FALSE))

# Repeat the equation numbers the same amount of time
numbers <- map2(n_perm, operations, ~ rep(list(.y), .x))
numbers <- do.call(list, unlist(numbers, recursive = FALSE))

# Function to build the equation
build_equation <- function(nums, op) {
  eq <- paste0(nums, c(op, ""), collapse = "")
  par <- paste0(rep("(", length(op)), collapse = '')
  full_eq <- paste0(par, eq)
  parse(text = full_eq) |> eval()
}

# Evaluate the equation for each element of the whole list
eval_res <- map2(
  .x = numbers,
  .y = operators,
  .f = ~ build_equation(.x, .y),
  .progress = TRUE
)

# Repeat the results the same amount of time
results <- map2(n_perm, results, ~ rep(list(.y), .x))
results <- do.call(list, unlist(results, recursive = FALSE))

total <- sum(unlist(results)[unlist(results) == unlist(eval_res)])

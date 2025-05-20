
# Read input data ----
input <- readLines("2024/03-input")

# Part 1 ----

mul <- stringr::str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)") |>
  unlist()

multiply <- function(x) {
  values <- stringr::str_extract_all(x, "\\d+")[[1]] |>
    as.numeric()
  values[1] * values[2]
}

total <- lapply(mul, multiply) |> 
  unlist() |> 
  sum()

print(total)

# Part 2 ----

statements <- stringr::str_extract_all(
  input, 
  "mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don\\'t\\(\\)"
) |>
  unlist()

cond_mult <- function(x) {
  if (x == "do()" | x == "don't()") {
    return(0)
  } else {
    values <- stringr::str_extract_all(x, "\\d+")[[1]] |>
      as.numeric()
    return(values[1] * values[2])
  }
}

do <- 1
doable <- function(x) {
  if (x == "do()") {
    new_do <- 1
  } else if (x == "don't()") {
    new_do <- 0
  } else {
    new_do <- .GlobalEnv$do
  }
  .GlobalEnv$do <- new_do
  new_do
}

values <- lapply(statements, cond_mult) |> 
  unlist()
condition <- lapply(statements, doable) |>
  unlist()
cond_total <- sum(values * condition)
print(cond_total)

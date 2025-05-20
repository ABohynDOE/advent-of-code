
# Read input data ----
input <- readLines("2024/day-05/05-input")

# Part 1 ----

split <- which(input=="")[1]
conditions <- input[1:split-1] |>
  map(~str_split_1(.x, "\\|") |> as.numeric())
page_nums <- input[(split+1):length(input)] |>
  map(~str_split_1(.x, ",") |> as.numeric())

single_check <- function(condition, page_nums) {
  if(!all(condition %in% page_nums)) {
    return(TRUE)
  } else {
    return(
      which(page_nums == condition[1]) < which(page_nums == condition[2])
    )
  }
}

all_checks <- function(page_nums) {
  checks <- purrr::map(conditions, ~single_check(.x, page_nums)) |>
    unlist()
  if (all(checks)) {
    middle <-  ceiling(length(page_nums)/2)
    return(page_nums[middle])
  } else {
    return(NA)
  }
}

total <- map(page_nums, all_checks) |>
  unlist() |> 
  sum(na.rm = TRUE)
print(total)


# Part 2 ----

split <- which(input=="")[1]
conditions <- input[1:split-1] |>
  map(~str_split_1(.x, "\\|") |> as.numeric())
page_nums <- input[(split+1):length(input)] |>
  map(~str_split_1(.x, ",") |> as.numeric())

shuffle_pages <- function(condition, page_nums) {
  if(!all(condition %in% page_nums)) {
    return(page_nums)
  } else {
    index_1 <- which(page_nums == condition[1])
    index_2 <- which(page_nums == condition[2])
    check_cond <-  index_1 < index_2
    if (check_cond) {
      return(page_nums)
    } else {
      new_page_nums <- page_nums
      new_page_nums[index_1] <- page_nums[index_2]
      new_page_nums[index_2] <- page_nums[index_1]
      return(new_page_nums)
    }
  }
}

shuffle_all <- function(page_nums) {
  pages <- page_nums
  for (c in conditions) {
    pages <- shuffle_pages(c, pages)
  }
  pages
}

keep_bad <- function(page_nums) {
  checks <- purrr::map(conditions, ~single_check(.x, page_nums)) |>
    unlist()
  if (any(!checks)) {
    page <- shuffle_all(page_nums)
    middle <-  ceiling(length(page)/2)
    return(page[middle])
  } else {
    return(NA)
  }
}

total <- map(page_nums, keep_bad) |>
  unlist() |> 
  sum(na.rm = TRUE)
print(total)

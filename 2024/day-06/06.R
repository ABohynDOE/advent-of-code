
# Read input data ----
input <- readLines("2024/day-06/06-input") |> 
  strsplit("")
start_mat <- do.call(rbind, input)

# Part 1 ----

# Find starting position
start_row <- row(start_mat)[start_mat == "^"]
start_col <- col(start_mat)[start_mat == "^"]

# Function to define the new direction
new_dir <- function(dir) {
  switch(
    dir,
    "up" = "right",
    "right" = "down",
    "down" = "left",
    "left" = "up"
  )
}


# Function to check what's ahead
# If ok, the matrix is modified, else the direction is changed
check_ahead <- function(m, r, c, d){
  # cli::cli_alert_info(
  #   "Evaluating cell [{r}, {c}]"
  # )
  directions <- c("up", "right", "down", "left")
  new_r <- switch(
    d,
    "up" = r - 1,
    "down" = r + 1,
    "left" = r,
    "right" = r,
    stop("Invalid direction")  # Default case for invalid input
  )
  new_c <- switch(
    d,
    "up" = c,
    "down" = c,
    "left" = c - 1,
    "right" = c + 1,
    stop("Invalid direction")  # Default case for invalid input
  )
  if (new_r > nrow(mat) | new_c > ncol(mat)) {
    return(FALSE)
  }
  ahead <- m[new_r, new_c]
  if (ahead == "#") {
    new_d <- new_dir(d)
    .GlobalEnv$dir <- new_d
    .GlobalEnv$row <- r
    .GlobalEnv$col <- c
    return(TRUE)
    # cli::cli_alert_warning(
    #   "[{new_r}, {new_c}] = # - Staying at [{r}, {c}], new direction is {.strong {new_d}}\n"
    # )
  } else if (ahead == "." | ahead == "X" | ahead == "^") {
    .GlobalEnv$row <- new_r
    .GlobalEnv$col <- new_c
    .GlobalEnv$mat[r, c] <- "X"
    return(TRUE)
    # cli::cli_alert_success(
    #   "[{new_r}, {new_c}] = {ahead} - Moving to  [{new_r}, {new_c}], new direction stays {.strong {d}}\n"
    # )
  } else {
    cli::cli_alert_danger("Unknown cell: {ahead}")
    break
  }
}

row <- start_row - 1
col <- start_col
mat <- start_mat
dir <- "up"
cur_cell <- mat[row, col]
in_map <- TRUE

while (in_map) {
  in_map <- check_ahead(mat, row, col, dir)
  if(!in_map) {
    cli::cli_alert_danger("Leaving the map from [{row}, {col}]")
  }
}

total <- length(mat[mat == "X"]) + 1

# Part 2 ----

# Define starting values
row <- start_row - 1
col <- start_col
mat <- start_mat
dir <- "up"
cur_cell <- mat[row, col]
stuck <- FALSE
in_map <- TRUE

# Lists of previously visited positions
dir_mat <- matrix(data = "", nrow = nrow(mat), ncol = ncol(mat))
dir_mat[start_row, start_col] = dir
dir_mat[start_row, start_col] = dir

while (!stuck & in_map) {
  # Check ahead and move accordingly
  in_map <- check_ahead(mat, row, col, dir)
  # Check if we have already been exactly in this position
  if (dir_mat[row, col] == dir) {
    cli::cli_alert_warning("Stuck in a loop at [{row}, {col}]")
    stuck <- TRUE
  }
  # Add the current cell to the list of visited cell
  dir_mat[row, col] <- dir
  if(!in_map) {
    cli::cli_alert_danger("Leaving the map from [{row}, {col}]")
  }
}

total <- length(mat[mat == "X"]) + 1

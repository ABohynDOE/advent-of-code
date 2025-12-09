# You can find SESSION by using Chrome tools:
# 1) Go to https://adventofcode.com/2025/day/1/input
# 2) right-click -> inspect -> click the "Application" tab.
# 3) Refresh
# 5) Click https://adventofcode.com under "Cookies"
# 6) Grab the value for session. Fill it in.

SESSION <- "53616c7465645f5f006a66d0f0ddfa4ece13bfc1e1fc5c3aa793e1ea9678d2af652b57255d5876c2d544ad4f6aaa04ac692f286971a6b0c461a90bd731f03472"

get_input <- function(year = 2025, day = 1, session = SESSION, file = NULL) {
  # Create the request
  req <- httr2::request("https://adventofcode.com/") |>
    httr2::req_url_path_append(
      as.character(year),
      "day",
      as.character(day),
      "input"
    ) |>
    httr2::req_cookies_set(session = session) |>
    httr2::req_user_agent("github.com/ABohynDOE")
  # Fetch the response
  resp <- httr2::req_perform(req)
  input <- httr2::resp_body_string(resp)
  # Save the AoC problem input to file
  if (is.null(file)) {
    day_str <- sprintf("%02d", day)
    dir <- glue::glue("{year}/day-{day_str}")
    file <- glue::glue("{dir}/{day_str}-input")
  }
  # Check if the dir exists, else create it
  if (!fs::dir_exists(dir)) {
    fs::dir_create(dir)
  }
  writeLines(input, con = file)
  cli::cli_alert_info(
    "Input for day {day} of year {year} saved to {.path {file}}"
  )
}

setup_r_file <- function(year = 2024, day = 1) {
  # Generate directory for the year and
  day_str <- sprintf("%02d", day)
  dir <- glue::glue("{year}/day-{day_str}")
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  # Define the basic file structure
  file_content <- whisker::whisker.render(
    '
# Read input data ----
input <- "{{year}}/day-{{day}}/{{day}}-input"

# Part 1 ----

# Part 2 ----',
    list(year = year, day = day_str)
  )
  # Render the template and save to file
  file_name <- glue::glue("{year}/day-{day_str}/{day_str}.R")
  if (fs::file_exists(file_name)) {
    cli::cli_alert_warning("File {.path {file_name}} already exists")
  } else {
    writeLines(text = file_content, con = file_name)
    cli::cli_alert_info(
      "R files set up for day {day} of year {year} at {.path {file_name}}"
    )
  }
}

# setup_r_file(year = 2024, day = 2)
# get_input(year = 2024, day = 2)

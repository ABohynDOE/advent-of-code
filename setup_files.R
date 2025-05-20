SESSION <- "53616c7465645f5fa8c95371ce3a71bbd60895d4ebde139eea12fe45664c22aa7d296a50db247e861e5cbec3444a5ac4e988784f127ba5b589e2d5a74e3d15bd"

get_input <- function(year = 2024, day = 1, session = SESSION, file = NULL) {
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

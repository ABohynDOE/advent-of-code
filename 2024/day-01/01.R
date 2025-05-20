
# Read input data ----
input <- readr::read_delim(
  "2024/01-input",
  delim = "   ",
  col_names = FALSE,
  show_col_types = FALSE
)

# Part 1 ----

distance <- (sort(input$X1) - sort(input$X2)) |>
  abs() |> 
  sum()
print(distance)

# Part 2 ----

similarity <- function(x) {
  sum(input$X2 == x) * x
}
total_similarity <- lapply(input$X1, similarity) |>
  unlist() |>
  sum()
print(total_similarity)
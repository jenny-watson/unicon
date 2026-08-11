#' @title Category relationships
#' @param derived A dataframe made from `unicon_make_derived_data_from_jsons`
#' @returns Makes a dataframe with all combination of relationships between
#' unit categories, where any unit category can be every combination of `id`,
#' `x`, and `y` with the correct `operator` assigned.
#' @import dplyr
#' @export

unicon_make_relationships_data <- function(derived) {
  relationships <- bind_rows(
    derived,
    derived |>
      rename(
        "id" = "y",
        "y" = "id"
      ),
    derived |>
      rename(
        "id" = "x",
        "x" = "id"
      ) |>
      mutate(
        operator = "multiply"
      ),
    derived |>
      rename(
        "id" = "x",
        "y" = "id",
        "x" = "y"
      ) |>
      mutate(
        operator = "multiply"
      )
  ) |>
    distinct()

  relationships
}

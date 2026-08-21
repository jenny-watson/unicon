#' @title Category relationships
#' @param derived A dataframe made from `unicon_make_derived_data_from_jsons`
#' @returns Makes a dataframe with all combination of relationships between
#' unit categories, where any unit category can be every combination of `id`,
#' `x`, and `y` with the correct `operator` assigned.
#' @import dplyr
#' @export

unicon_make_relationships_data <- function(derived) {
  ## separate data if divide or multipy in .jsons provided

  derived_divide <- derived |>
    filter(
      .data$operator == "divide"
    )

  derived_multiply <- derived |>
    filter(
      .data$operator == "multiply"
    )

  ## for divide supplied relationships

  relationships_divide <- bind_rows(
    derived_divide,
    derived_divide |>
      rename(
        "id" = "y",
        "y" = "id"
      ),
    derived_divide |>
      rename(
        "id" = "x",
        "x" = "id"
      ) |>
      mutate(
        operator = "multiply"
      ),
    derived_divide |>
      rename(
        "id" = "x",
        "y" = "id",
        "x" = "y"
      ) |>
      mutate(
        operator = "multiply"
      )
  )

  ## for multiply supplied relationships

  relationships_multiply <- bind_rows(
    derived_multiply,
    derived_multiply |>
      rename(
        "x" = "y",
        "y" = "x"
      ),
    derived_multiply |>
      rename(
        "id" = "x",
        "x" = "id"
      ) |>
      mutate(
        operator = "divide"
      ),
    derived_multiply |>
      rename(
        "id" = "y",
        "y" = "x",
        "x" = "id"
      ) |>
      mutate(
        operator = "divide"
      )
  )

  ## bind together

  relationships <- bind_rows(
    relationships_divide,
    relationships_multiply
  ) |>
    distinct()

  ## in add their own operator

  if (!all(derived$operator %in% c("divide", "multiply"))) {
    warning("Relationship could not be derived as not a multiply or divide operator") # nolint
  }

  relationships
}

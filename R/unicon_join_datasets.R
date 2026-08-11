#' @title Join .json tibbles together
#' @description Join base, derived, operator datasets together so that every
#' combination has been mapped.
#' @param base A tibble with the columns "id", "alias", "category", "srp",
#' "slope", "intercept"
#' @param derived A tibble with the columns "id", "x", "y", "operator"
#' @param operators A tibble with the columns "operator", "id", "fun", "alias"
#' @returns A dataframe with all information from .jsons inside mapped at all
#' levels
#' @import dplyr purrr
#' @export

unicon_join_datasets <- function(base,
                                derived,
                                operators) {

  join <- derived |>
    ## join to x
    left_join(
      base |>
        rename_with(~ paste0(., ".x")),
      by = c("x" = "category.x"),
      relationship = "many-to-many"
    ) |>
    ## join to y
    left_join(
      base |>
        rename_with(~ paste0(., ".y")),
      by = c("y" = "category.y"),
      relationship = "many-to-many"
    ) |>
    ## join to operators
    left_join(
      operators |>
        rename_with(~ paste0(., ".o")),
      by = c("operator" = "operator.o"),
      relationship = "many-to-many"
    ) |>
    ## format and calculate
    mutate(
      category = .data$id,
      id = paste0(.data$id.x, .data$id.o, .data$id.y),
      alias = paste0(.data$alias.x, .data$alias.o, .data$alias.y), # problem per has no spaces?
      srp = paste0(.data$srp.x, .data$id.o, .data$srp.y),
      slope = pmap_dbl(list(.data$fun.o,
                            .data$slope.x,
                            .data$slope.y),
                       function(op, x, y)
                         do.call(op, list(x, y))),
      intercept = 0,
      type = "derived",
      .keep = "none"
    ) |>
    ## bind to base data
    bind_rows(
      base |>
        mutate(type = "base")
    ) |>
    ## duplicate & wrong units if both base and derived unit
    filter(
      !(.data$category == "area" & .data$srp == "l__m"),
      !(.data$category == "length" & .data$srp == "ha__m")
    )

  join

}

#' @title Helper for unit conversion
#' @description Return full unit conversion table with optional filter-by-regex
#' args.
#' @param ... Optional filter parameters, named regular expressions for
#' filtering by one or more variables. May be ignored if full table is required.
#' @importFrom stringr str_detect
#' @import dplyr purrr
#' @export

unicon_help <- function(...) {
  # list up args
  args <- list(...)

  # full unit data — always reads from the active state so custom data is shown
  unit_full <- .unicon_state$unit_alias |>
    left_join(.unicon_state$unit_srp,
      by = "id",
      multiple = "any"
    ) |>
    left_join(.unicon_state$unit_models,
      by = "id",
      multiple = "any"
    )

  # subselect by args
  unit_full <- reduce2(
    names(args),
    args,
    ~ filter(
      ..1,
      str_detect(
        .data[[..2]],
        ..3
      )
    ),
    .init = unit_full
  )

  unit_full
}

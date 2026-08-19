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
  ## as area and volume are base and derived, haven't derived them yet
  ## derive then by length^2 or length^3

  # get length alias and slope

  length <- base |>
    filter(
      .data$category == "length"
    ) |>
    distinct(
      .data$id,
      .data$alias,
      .data$slope
    )

  ## 1ha = 100m * 100m

  area <- tibble(
    area_alias = c(
      "2",
      "squared",
      "square"
    )
  ) |>
    cross_join(length) |>
    mutate(
      id = paste0(.data$id, "2"),
      alias = if_else(
        .data$area_alias == "square",
        paste0(.data$area_alias, .data$alias),
        paste0(.data$alias, .data$area_alias)
      ),
      type = "derived",
      category = "area",
      srp = "ha",
      slope = (.data$slope / 100)^2,
      intercept = 0,
      .keep = "none"
    )

  # 1l = 0.1m * 0.1m * 0.1m

  volume <- tibble(
    area_alias = c(
      "3",
      "cubed",
      "cubic"
    )
  ) |>
    cross_join(length) |>
    mutate(
      id = paste0(.data$id, "3"),
      alias = if_else(
        .data$area_alias == "cubic",
        paste0(.data$area_alias, .data$alias),
        paste0(.data$alias, .data$area_alias)
      ),
      type = "derived",
      category = "volume",
      srp = "l",
      slope = (.data$slope / 0.1)^3,
      intercept = 0,
      .keep = "none"
    )

  ## create new base data

  new_base <- bind_rows(
    base |>
      mutate(type = "base"),
    area,
    volume
  )

  ## join new base, derived and operator data together

  join <- derived |>
    ## ensure units only derived that are in base data, NA otherwise
    filter(
      .data$x %in% base$category,
      .data$y %in% base$category,
    ) |>
    ## join to x
    left_join(
      new_base |>
        rename_with(~ paste0(., ".x")),
      by = c("x" = "category.x"),
      relationship = "many-to-many"
    ) |>
    ## join to y
    left_join(
      new_base |>
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
      alias = paste0(.data$alias.x, .data$alias.o, .data$alias.y),
      srp = paste0(.data$srp.x, .data$id.o, .data$srp.y),
      slope = pmap_dbl(
        list(
          .data$fun.o,
          .data$slope.x,
          .data$slope.y
        ),
        function(op, x, y) {
          do.call(op, list(x, y))
        }
      ),
      intercept = 0,
      type = "derived",
      .keep = "none"
    ) |>
    ## bind to new_base data
    bind_rows(
      new_base
    ) |>
    ## duplicate & wrong units if both base and derived unit
    filter(
      !(.data$category == "area" & .data$srp == "l__m"),
      !(.data$category == "length" & .data$srp == "ha__m")
    ) |>
    ## make sure pressure consistent unit
    mutate(
      srp = if_else(
        .data$category == "pressure",
        "pa",
        .data$srp
      )
    )

    if (all(derived$x %in% base$category) |
        all(derived$y %in% base$category)) {

          warning <- derived |>
            filter(
              !.data$x %in% base$category |
              !.data$y %in% base$category
            )

          warning("Derived data is not just made of base units. Please check all units are present in base data.") # nolint
          print(warning)

    }

    if (all(derived$x %in% base$category) &
        all(derived$y %in% base$category)) {

      warning <- derived |>
        filter(
          !.data$x %in% base$category,
            !.data$y %in% base$category
        )

      stop("Derived data is not made of base units. Add missing units to base data.") # nolint
      print(warning)

    }

  join
}

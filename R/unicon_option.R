#' @title Deriving and calculating unit conversion between one or two metrics
#' @description When you have a mixture of unit categories, but want one unit
#' outcome. This function uses both unicon_full and unicon_advance to give a
#' single value outcome in a specified unit.
#' @param value_in Numeric scalar or vector, values to convert.
#' @param unit_in Character scalar or vector, input units for \code{value_in}.
#' Must be of \code{length(1L)} or \code{length(value_in)}.
#' @param unit_out Character scalar or vector, output units for conversion. Must
#' be of \code{length(1L)} or \code{length(value_in)}.
#' @param extras A list of other unit categories needed for use in
#' `unicon_advance`, which themsevles are a list of two specificying `unit` and
#' `value`. Must use unit category names (see `unicon_help()`).
#' @import dplyr
#' @importFrom purrr imap_dfc
#' @export


unicon_option <- function(value_in,
                          unit_in,
                          unit_out,
                          extras) {

  if (!(is.character(unit_out)) || all(is.na(unit_out))) {
    stop("Argument `unit_out` must be a character vector or `NA`.")
  }

  ## format data so have all data expanded for joins later

  pre <- tibble(
    value_in = value_in,
    unit_in = unit_in,
    unit_out = unit_out
  ) |>
    bind_cols(
      imap_dfc(
        extras,
        ~ tibble(
          !!paste0(.y, "_value") := .x$value,
          !!paste0(.y, "_unit") := .x$unit
        )
      )
    ) |>
    pivot_longer(
      cols = c(
        ends_with("_value"),
        ends_with("_unit")
      ),
      names_to = c("category_y", ".value"),
      names_pattern = "(.*)_(value|unit)"
    ) |>
    rename(
      "value_y" = "value",
      "unit_y" = "unit"
    )

  ## check if categories provided in extra are recognised by unicon

  chk_cat_exist <- pre |>
    distinct(
      .data$category_y
    ) |>
    anti_join(
      unicon_help() |>
        distinct(
          .data$category
        ),
      by = c(
        "category_y" = "category"
      )
    )


  if (nrow(chk_cat_exist) != 0) {
    stop("Please use category names used in unicon; see `unicon_help()`")
  }

  # work out unit in and out categories

  unit_cat <- pre |>
    distinct(
      .data$value_in,
      .data$unit_in,
      .data$unit_out
    ) |>
    mutate(
      alias_in = str_replace_all(str_to_lower(.data$unit_in), "\\s+", ""),
      alias_out = str_replace_all(str_to_lower(.data$unit_out), "\\s+", "")
    ) |>
    left_join(
      unicon_help() |>
        select(
          "alias",
          "category_in" = "category"
        ),
      by = c("alias_in" = "alias")
    ) |>
    left_join(
      unicon_help() |>
        select(
          "alias",
          "category_out" = "category"
        ),
      by = c("alias_out" = "alias")
    )

  ## check for blanks

  if (any(is.na(unit_cat$category_in))) {

    bad_unit_in <- unit_cat |>
      filter(is.na(.data$category_in)) |>
      pull(.data$unit_in)

    stop(
      paste0(
        "`unit_in` does not have a recognised category: ",
        paste(bad_unit_in, collapse = ", ")
      )
    )
  }

  if (any(is.na(unit_cat$category_out))) {

    bad_unit_out <- unit_cat |>
      filter(is.na(.data$category_out)) |>
      pull(.data$unit_out)

    stop(
      paste0(
        "`unit_out` does not have a recognised category: ",
        paste(bad_unit_out, collapse = ", ")
      )
    )
  }

  ## create emtpy df for joins later on

  same_out <- tibble(
    value_in = numeric(),
    unit_in = character(),
    unit_out = character(),
    value_out = numeric()
  )

  diff_out <- tibble(
    x_value_in = numeric(),
    x_unit_in = character(),
    unit_out = character(),
    value_out = numeric()
  )

  ## if category in == out

  cat_same <- unit_cat |>
    filter(
      .data$category_in == .data$category_out
    )

  if (nrow(cat_same) != 0) {

    same_out <- unicon_full(
      value_in = cat_same$value_in,
      unit_in = cat_same$unit_in,
      unit_out = cat_same$unit_out,
      pull = FALSE)

  }


  ## if category in != out


  cat_diff <- unit_cat |>
    filter(.data$category_in != .data$category_out) |>
    left_join(
      .unicon_state$relationships,
      by = c(
        "category_out" = "id",
        "category_in" = "x"
      )
    ) |>
    left_join(pre,
              by = c(
                "value_in",
                "unit_in",
                "unit_out",
                "y" = "category_y"
              )
    )

  if (nrow(cat_diff) != 0) {

    diff_out <- unicon_advance(
      x_value_in = cat_diff$value_in,
      x_unit_in = cat_diff$unit_in,
      y_value_in = cat_diff$value_y,
      y_unit_in = cat_diff$unit_y,
      unit_out = cat_diff$unit_out,
      operator_in = NA,
      pull = FALSE)

  }

  ## join and same and diff out together so output is in right order

  out <- pre |>
    left_join(
      same_out |>
        select(
          "value_in",
          "unit_in",
          "unit_out",
          "value_out"
        ),
      by = c(
        "value_in",
        "unit_in",
        "unit_out"
      )
    ) |>
    left_join(
      diff_out |>
        select(
          "value_in" = "x_value_in",
          "unit_in" = "x_unit_in",
          "unit_out",
          "value_out"
        ),
      by = c(
        "value_in",
        "unit_in",
        "unit_out"
      )
    ) |>
    mutate(
      value_out = coalesce(.data$value_out.x, .data$value_out.y)
    ) |>
    select(
      -"value_out.x",
      -"value_out.y"
    )

  out$value_out
}

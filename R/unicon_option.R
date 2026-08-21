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
  if (length(unit_out) != 1L) {
    stop("Argument `unit_out` must have length 1.")
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
      names_to = c("category", ".value"),
      names_pattern = "(.*)_(value|unit)"
    ) |>
    rename(
      value_y = .data$value,
      unit_y = .data$unit
    )

  ## check if categories provided in extra are recognised by unicon

  chk_cat_exist <- pre |>
    distinct(
      .data$category
    ) |>
    anti_join(
      unicon_help() |>
        distinct(
          .data$category
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
      unicon::unicon_help() |>
        select(
          .data$alias,
          category_in = .data$category
        ),
      by = c("alias_in" = "alias")
    ) |>
    left_join(
      unicon::unicon_help() |>
        select(
          .data$alias,
          category_out = .data$category
        ),
      by = c("alias_out" = "alias")
    )


  ## if category in == out

  cat_same <- unit_cat |>
    filter(
      .data$category_in == .data$category_out
    )

  same_out <- unicon_full(
    value_in = cat_same$value_in,
    unit_in = cat_same$unit_in,
    unit_out = cat_same$unit_out,
    pull = FALSE
  )

  ## if category in != out


  cat_diff <- unit_cat |>
    filter(.data$category_in != .data$category_out) |>
    left_join(
      unicon:::relationships,
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
        "y" = "category"
      )
    )


  diff_out <- unicon_advance(
    x_value_in = cat_diff$value_in,
    x_unit_in = cat_diff$unit_in,
    y_value_in = cat_diff$value_y,
    y_unit_in = cat_diff$unit_y,
    unit_out = cat_diff$unit_out,
    operator_in = NA,
    pull = FALSE
  )

  out <- pre |>
    distinct(
      .data$value_in,
      .data$unit_in,
      .data$unit_out
    ) |>
    left_join(
      same_out |>
        select(
          .data$value_in,
          .data$unit_in,
          .data$unit_out,
          .data$value_out
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
          value_in = .data$x_value_in,
          unit_in = .data$x_unit_in,
          .data$unit_out,
          .data$value_out
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
      -.data$value_out.x,
      -.data$value_out.y
    )

  out$value_out
}

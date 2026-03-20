#' @title Deriving and calculating category relationship between two metrics
#' @description Using the category_relationship package data, this function uses
#' 'unicon_full' to calculate new metrics from parent metrics by firstly
#' converting parent units to SI, calculating the new metric in SI units then
#' converting to the desired units if specified.
#' @param parent_1_value_in Numeric scalar or vector, values to convert from
#' first parent metric and use in calculations of new metrics
#' @param parent_2_value_in Numeric scalar or vector, values to convert from
#' second parent metric and use in calculations of new metric
#' @param parent_1_unit_in Character scalar or vector, input units for
#' \code{parent_1_value_in}.#' Must be of \code{length(1L)} or
#' \code{length(parent_1_value_in)}.
#' @param parent_2_unit_in Character scalar or vector, input units for
#' \code{parent_2_value_in}.Must be of \code{length(1L)} or
#' \code{length(parent_2_value_in)}.
#' @param unit_out Character scalar or vector, output units for conversion. Must
#' be of \code{length(1L)} or \code{length(parent_1_value_in)}. Defaults to
#' \code{NA}; if default is passed, function will return standard index (SI)
#' units as conversion.
#' @param pull Logical; should the function pull out and return the converted
#' values (TRUE) or should a full table with conversion record be returned?
#' Defaults to TRUE.
#' @import dplyr
#' @export

unicon_catrel <- function(parent_1_unit_in,
                          parent_2_unit_in,
                          parent_1_value_in,
                          parent_2_value_in,
                          unit_out = NA,
                          pull = TRUE) {
  # check, all values must be either length 1 or consistent length
  l1 <- length(parent_1_unit_in)
  l2 <- length(parent_2_unit_in)
  l3 <- length(parent_1_value_in)
  l4 <- length(parent_2_value_in)
  l5 <- length(unit_out)
  if (l1 == 0L) stop("Length for value_in argument must be >= 1L")
  if (l2 != l1 && l2 != 1L) stop("Length for unit_in argument incompatible")
  if (l3 != l1 && l3 != 1L) stop("Length for unit_out argument incompatible")
  if (l4 != l1 && l4 != 1L) stop("Length for unit_out argument incompatible")
  if (l5 != l1 && l5 != 1L) stop("Length for unit_out argument incompatible")

  # confirm relationship between parent 1 and 2 units
  relationship_check <-
    ## make into df for easier calculations
    tibble(
      parent_1_unit_in = parent_1_unit_in,
      parent_2_unit_in = parent_2_unit_in,
      parent_1_value_in = parent_1_value_in,
      parent_2_value_in = parent_2_value_in,
      unit_out = unit_out
    ) |>
    # convert both parent metrics to SI first and bind to df as vectors
    bind_cols(
      parent_1_si_value = unicon_full(
        value_in = df$parent_1_value_in,
        unit_in = df$parent_1_unit_in,
        unit_out = NA,
        pull = TRUE
      ),
      parent_2_si_value = unicon_full(
        value_in = df$parent_2_value_in,
        unit_in = df$parent_2_unit_in,
        unit_out = NA,
        pull = TRUE
      )
    ) |>
    # get category type of parent 1
    left_join(
      unit_alias |>
        left_join(
          unit_si |>
            select(-type),
          by = "id"
        ) |>
        select(-id) |>
        rename_with(~ paste0("parent_1_", .)),
      by = c("parent_1_unit_in" = "parent_1_alias")
    ) |>
    # get category type of parent 2
    left_join(
      unit_alias |>
        left_join(
          unit_si |>
            select(-type),
          by = "id"
        ) |>
        select(-id) |>
        rename_with(~ paste0("parent_2_", .)),
      by = c("parent_2_unit_in" = "parent_2_alias")
    ) |>
    # find relationship between parent 1 and 2
    left_join(category_relationships,
      by = c(
        "parent_1_category" = "parent_1",
        "parent_2_category" = "parent_2"
      )
    )

  if (is.na(pull(distinct(relationship_check, category))) == TRUE) {
    stop("There is no recorded relationship between parent units.
         Please change assignment of parent_1 and parent_2 and re-run.")
  }

  if (pull(distinct(relationship_check, category)) == 'acceleration') {

    warning("Please ensure you have read the vignettes and calculated change in
            speed before using this function")

  }

  if (is.na(pull(distinct(relationship_check, unit_out))) == FALSE) {
    unit_out_check <- relationship_check |>
      distinct(
        assigned_category = category,
        unit_out
      ) |>
      left_join(
        unit_alias |>
          left_join(
            unit_si |>
              select(-type),
            by = "id"
          ) |>
          select(alias,
            unit_category = category
          ),
        by = c("unit_out" = "alias")
      )

    if (unit_out_check$assigned_category != unit_out_check$unit_category) {
      stop("unit_out does not exist for the relationship derived between parent units")
    }
  }

  workings <- relationship_check |>
    # find SI for relationship between parent 1 and 2
    left_join(
      unit_si |>
        distinct(category,
          si_unit_out = si
        ),
      by = "category"
    ) |>
    mutate(
      # calculate value in SI
      si_value_out = case_when(
        operator == "divide" ~ parent_1_si_value / parent_2_si_value,
        operator == "multiply" ~ parent_1_si_value * parent_2_si_value
      ),
      # assign a unit_out to SI if not already assigned in function
      unit_out = if_else(is.na(unit_out),
        si_unit_out,
        unit_out
      )
    )

  # get value out in assigned units
  value_out <- unicon_full(
    value_in = workings$si_value_out,
    unit_in = workings$si_unit_out,
    unit_out = workings$unit_out,
    pull = TRUE
  )

  if (isTRUE(pull)) {
    value_out
  } else {
    final <- workings |>
      bind_cols(value_out = value_out) |>
      select(parent_1_unit_in,
        parent_1_value_in,
        parent_1_category,
        parent_1_si_unit = parent_1_si,
        parent_1_si_value,
        parent_2_unit_in,
        parent_2_value_in,
        parent_2_category,
        parent_2_si_unit = parent_2_si,
        parent_2_si_value,
        operator,
        category_out = category,
        si_unit_out,
        si_value_out,
        unit_out,
        value_out
      )
  }
}

#' @title Deriving and calculating unit conversion between two metrics
#' @description Using the `relationships` package data, this function uses
#' 'unicon_full' to calculate new metrics from parent metrics by firstly
#' converting parent units to standard reference point (SRP), calculating the
#' new metric in SRP units then converting to the desired units if specified.
#' @param x_value_in Numeric scalar or vector, values to convert from `x` and
#' use in calculations of new metrics
#' @param y_value_in Numeric scalar or vector, values to convert from `y` and
#' use in calculations of new metric
#' @param x_unit_in Character scalar or vector, input units for
#' \code{x_value_in}. Must be of \code{length(1L)} or \code{length(x_value_in)}.
#' @param y_unit_in Character scalar or vector, input units for
#' \code{y_value_in}. Must be of \code{length(1L)} or \code{length(y_value_in)}.
#' @param unit_out Character scalar or vector, output units for conversion. Must
#' be of \code{length(1L)} or \code{length(x_value_in)}. Defaults to
#' \code{NA}; if default is passed, function will return standard reference
#' point (SRP) units as conversion.
#' @param operator_in Character scalar; 'divide' or 'multiply'. Only needed for
#' calculating volume from volume_fraction and mass from mass_fraction.
#' @param pull Logical; should the function pull out and return the converted
#' values (TRUE) or should a full table with conversion record be returned?
#' Defaults to TRUE.
#' @import dplyr
#' @importFrom purrr pmap_dbl
#' @export

unicon_advance <- function(x_unit_in,
                           y_unit_in,
                           x_value_in,
                           y_value_in,
                           unit_out = NA,
                           operator_in = NA,
                           pull = TRUE) {

  # checks are minimal as relying in unicon_full checks

  l1 <- length(x_value_in)
  l2 <- length(y_value_in)

  if (l2 != l1 ) {
    stop("Argument `x_value_in` and `y_value_in` must have same length.")
  }


  ## convert units for x and y
  ## suppress warning as have own warnings here and not to confuse users

  x_srp_value <- suppressWarnings(
    unicon_full(
      value_in = x_value_in,
      unit_in = x_unit_in,
      unit_out = NA,
      pull = FALSE
    )
  ) |>
    mutate(
      row_number = 1:n()
    )

  y_srp_value <- suppressWarnings(
    unicon_full(
      value_in = y_value_in,
      unit_in = y_unit_in,
      unit_out = NA,
      pull = FALSE
    )
  ) |>
    mutate(
      row_number = 1:n()
    )

  ## identify what type of metrics x and y are & their srp

  x_category <- filter(
    unit_srp,
    srp %in% x_srp_value$srp_in
  ) |>
    distinct(
      category,
      srp
    )

  y_category <- filter(
    unit_srp,
    srp %in% y_srp_value$srp_in
  ) |>
    distinct(
      category,
      srp
    )

  ## figure out if a relationship exists between x and y

  rel = relationships |>
    filter(
      x %in% x_category$category,
      y %in% y_category$category
    )

  ## if no parent relationship, stop
  if (nrow(rel) == 0) {
    stop("There is no recorded relationship between x and y units")
  }

  ## special case check:
  ## does the operator need specifying?

  rel_check <- relationships |>
    count(
      id,
      x,
      y
    ) |>
    filter(
      n == 2,
      x %in% x_category$category,
      y %in% y_category$category
    )

  ## check and apply operator_in - only needed for mass & volume fractions

  if (nrow(rel_check) != 0) {

    if(!is.na(operator_in)) {

      rel <- rel |>
        filter(
          operator == operator_in
        )

    } else {

      stop("Please specify `operator_in`")

    }
  }

  # check that the unit_out specified is valid

  if (!any(is.na(unit_out))) {

    check_unit_out <- unit_alias |>
      filter(alias %in% unit_out) |>
      left_join(
        unit_srp,
        by = 'id'
      ) |>
      filter(
        !category %in% rel$id
      )

    if (nrow(check_unit_out) != 0) {

      stop("`unit_out` does not exist for the specified x and y relationship")

    }

  }

  ## join all datasets together and do final unicon_full

  workings <- rel |>
    # find srp for relationship between parent x and y
    left_join(
      unit_srp |>
        distinct(category,
                 srp_unit_out = srp
        ),
      by = c("id" = "category")
    ) |>
    ## join to operators function
    left_join(
      operators_helper(),
      by = "operator"
    ) |>
    # join to x info
    left_join(
      x_category |>
        rename(x_srp_in = srp),
      by = c("x" = "category")
    ) |>
    left_join(
      x_srp_value |>
        rename_with(~ paste0("x_", .x)),
      by = "x_srp_in"
    ) |>
    # join to y info
    left_join(
      y_category |>
        rename(y_srp_in = srp),
      by = c("y" = "category")
    ) |>
    left_join(
      y_srp_value |>
        rename_with(~ paste0("y_", .x)),
      by = c(
        "y_srp_in",
        "x_row_number" = "y_row_number"
      )
    ) |>
    mutate(
      # calculate value in srp
      srp_value_out = pmap_dbl(
        list(
          fun,
          x_value_out,
          y_value_out
        ),
        function(op, x, y)
          do.call(op, list(x, y))
      ),
      # get value out in assigned units
      value_out = unicon_full(
        value_in = srp_value_out,
        unit_in = srp_unit_out,
        unit_out = unit_out,
        pull = FALSE
      )
    ) |>
    ## get rid of any unneeded columns
    select(
      starts_with("x"),
      starts_with("y"),
      everything(),
      -fun,
      -x_srp_in,
      -y_srp_in,
      -srp_unit_out,
      -srp_value_out,
      -x_row_number
    )

  if (isTRUE(pull)) {

    # provide brief warnings

    if (any(is.na(workings$value_out))) {
      warning("Some units failed to convert or had invalid IDs. Set `pull = FALSE` for detailed output.") # nolint
    }

    # provide only value_out

    workings$value_out

  } else {

    # provide detailed warnings

    if (any(select(workings, ends_with("error_in")), na.rm = TRUE)) {
      warning("Some input units failed to find matches.")
    }
    if (any(select(workings, ends_with("error_out")), na.rm = TRUE)) {
      warning("Some output units failed to find matches.")
    }
    if (any(select(workings, ends_with("error_srp")), na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }

    workings

  }

}


operators_helper <- function() {

  operators_dir <- file.path("inst", "units", "operators")

  operators_data <- unicon_make_operators_data_from_jsons(operators_dir) |>
    distinct(
      operator,
      fun
    )

  operators_data

}


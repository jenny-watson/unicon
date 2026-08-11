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
#' @param operator_in Character scalar; \code{'divide'} or \code{'multiply'}.
#' Only needed for calculating volume from volume_fraction and mass from
#' mass_fraction.
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

  ## create tibble for joining

  tib <- tibble(
    x_unit_in = x_unit_in,
    y_unit_in = y_unit_in,
    x_value_in = x_value_in,
    y_value_in = y_value_in,
    unit_out = unit_out,
    operator_in = operator_in
  )

  ## convert units for x and y to SRP
  ## suppress warning as have own warnings here and not to confuse users

  x_srp_value <- suppressWarnings(
    suppressMessages(
      unicon_full(
        value_in = x_value_in,
        unit_in = x_unit_in,
        unit_out = NA,
        pull = FALSE
      )
    )
  )

  y_srp_value <- suppressWarnings(
    suppressMessages(
      unicon_full(
        value_in = y_value_in,
        unit_in = y_unit_in,
        unit_out = NA,
        pull = FALSE
      )
    )
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

  rel <- relationships |>
    filter(
      x %in% x_category$category,
      y %in% y_category$category
    )

  ## if no parent relationship, stop
  if (nrow(rel) == 0) {
    stop("There is no recorded relationship between parent units")
  }

  ## if operator_in is given, validate it against the available relationships

  if (!is.na(operator_in) && !operator_in %in% rel$operator) {
    stop("`operator_in` does not match the relationship derived between parent units")
  }

  ## special case check:
  ## does the operator need specifying?

  rel_check <- relationships |>
    count(
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

    if (!is.na(operator_in)) {

      rel <- rel |>
        filter(
          operator == operator_in
        )

    } else {

      stop("Please specify `operator_in`")

    }
  }

  ## check that the unit_out specified is valid

  if (!all(is.na(unit_out))) {

    check_unit_out <- unit_alias |>
      filter(alias %in% unit_out) |>
      left_join(
        unit_srp,
        by = "id"
      ) |>
      filter(
        !category %in% rel$id
      )

    if (nrow(check_unit_out) != 0) {

      stop("`unit_out` does not exist for the relationship derived between parent units")

    }

  }

  ## join all datasets together and create new metric data

  workings <- tib |>
    # join to x info
    left_join(
      x_srp_value |>
        rename_with(~ paste0("x_", .x)),
      by = c(
        "x_unit_in",
        "x_value_in"
      )
    ) |>
    left_join(
      x_category |>
        rename(
          x_srp_in = srp,
          x_category = category
        ),
      by = "x_srp_in"
    ) |>
    # join to y info
    left_join(
      y_srp_value |>
        rename_with(~ paste0("y_", .x)),
      by = c(
        "y_unit_in",
        "y_value_in"
      )
    ) |>
    left_join(
      y_category |>
        rename(
          y_srp_in = srp,
          y_category = category
        ),
      by = "y_srp_in"
    ) |>
    # join to relationships data
    left_join(
      rel,
      by = c(
        "x_category" = "x",
        "y_category" = "y"
      )
    ) |>
    # find srp for relationship between parent x and y
    left_join(
      unit_srp |>
        distinct(
          category,
          srp_unit_out = srp
        ),
      by = c("id" = "category")
    ) |>
    # sort operator out and get fun for map in mutate
    mutate(
      operator_in = if_else(
        !is.na(operator_in),
        operator_in,
        operator
      )
    ) |>
    select(
      -operator
    ) |>
    left_join(
      operators_helper(),
      by = c(
        "operator_in" = "operator"
      )
    ) |>
    # calculate value in SRP for derived metric
    mutate(
      srp_value_out = pmap_dbl(
        list(
          fun,
          x_value_out,
          y_value_out
        ),
        function(op, x, y)
          do.call(op, list(x, y))
      )
    )

  ## do final unicon_full

  final <- unicon_full(
    value_in = workings$srp_value_out,
    unit_in = workings$srp_unit_out,
    unit_out = workings$unit_out,
    pull = FALSE
  ) |>
    left_join(
      workings,
      by = c(
        "value_in" = "srp_value_out",
        "unit_in" = "srp_unit_out",
        "unit_out" = "unit_out"
      )
    ) |>
    ## get rid of any unneeded columns
    select(
      x_category,
      starts_with("x"),
      y_category,
      starts_with("y"),
      operator_in,
      id,
      everything(),
      -fun,
      -x_srp_in,
      -x_unit_out,
      -x_alias_out,
      -x_value_out,
      -y_srp_in,
      -y_unit_out,
      -y_alias_out,
      -y_value_out
    )

  if (isTRUE(pull)) {

    # provide brief warnings

    if (any(is.na(final$value_out))) {
      warning("Some units failed to convert or had invalid IDs. Set `pull = FALSE` for detailed output.") # nolint
    }

    # provide only value_out

    final$value_out

  } else {

    # provide detailed warnings

    if (any(select(final, ends_with("error_in")), na.rm = TRUE)) {
      warning("Some input units failed to find matches.")
    }
    if (any(select(final, ends_with("error_out")), na.rm = TRUE)) {
      warning("Some output units failed to find matches.")
    }
    if (any(select(final, ends_with("error_srp")), na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }

    final

  }

}


operators_helper <- function() {

  operators_dir <- system.file(
    "units",
    "operators",
    package = "unicon"
  )

  operators_data <- unicon_make_operators_data_from_jsons(operators_dir) |>
    distinct(
      operator,
      fun
    )

  operators_data

}


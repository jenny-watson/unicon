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
#' @param operator_in 'divide' or 'multiply' input. Only needed for calculating
#' volume from volume_fraction and mass from mass_fraction.
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

  ## --- argument validation ---

  if (length(x_value_in) < 1L) {
    stop("Length for x_value_in argument must be >= 1L")
  }
  if (length(y_value_in) < 1L) {
    stop("Length for y_value_in argument must be >= 1L")
  }

  n <- max(length(x_value_in), length(y_value_in))

  if (!length(x_unit_in) %in% c(1L, n)) {
    stop("Length for x_unit_in argument incompatible with length(x_value_in)")
  }
  if (!length(y_unit_in) %in% c(1L, n)) {
    stop("Length for y_unit_in argument incompatible with length(x_value_in)")
  }
  if (!length(x_value_in) %in% c(1L, n)) {
    stop("Length for x_value_in argument incompatible with length(x_value_in)")
  }
  if (!length(y_value_in) %in% c(1L, n)) {
    stop("Length for y_value_in argument incompatible with length(x_value_in)")
  }
  if (!all(is.na(unit_out)) && !length(unit_out) %in% c(1L, n)) {
    stop("Length for unit_out argument incompatible with length(x_value_in)")
  }

  ## --- recycle scalar inputs to length n ---

  if (length(x_unit_in) == 1L) x_unit_in <- rep(x_unit_in, n)
  if (length(y_unit_in) == 1L) y_unit_in <- rep(y_unit_in, n)
  if (length(x_value_in) == 1L) x_value_in <- rep(x_value_in, n)
  if (length(y_value_in) == 1L) y_value_in <- rep(y_value_in, n)
  if (!all(is.na(unit_out)) && length(unit_out) == 1L) unit_out <- rep(unit_out, n)

  ## convert units for x and y to SRP
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
      row_number = seq_len(n())
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
      row_number = seq_len(n())
    )

  ## check for failed unit lookups before proceeding

  if (any(x_srp_value$error_in)) {
    stop("Some x_unit_in values failed to find matches")
  }
  if (any(y_srp_value$error_in)) {
    stop("Some y_unit_in values failed to find matches")
  }

  ## identify what type of metrics x and y are & their srp

  x_category <- filter(
    unit_srp,
    .data$srp %in% x_srp_value$srp_in
  ) |>
    distinct(
      .data$category,
      .data$srp
    )

  y_category <- filter(
    unit_srp,
    .data$srp %in% y_srp_value$srp_in
  ) |>
    distinct(
      .data$category,
      .data$srp
    )

  ## figure out if a relationship exists between x and y

  rel <- relationships |>
    filter(
      .data$x %in% x_category$category,
      .data$y %in% y_category$category
    )

  ## if no parent relationship, stop
  if (nrow(rel) == 0) {
    stop("There is no recorded relationship between parent units")
  }

  ## if operator_in is given, validate it against the available relationships

  if (!is.na(operator_in) && !operator_in %in% rel$operator) {
    stop("operator_in does not match the relationship derived between parent units")
  }

  ## special case check:
  ## does the operator need specifying?

  rel_check <- relationships |>
    count(
      .data$id,
      .data$x,
      .data$y
    ) |>
    filter(
      .data$n == 2,
      .data$x %in% x_category$category,
      .data$y %in% y_category$category
    )

  ## check and apply operator_in - only needed for mass & volume fractions

  if (nrow(rel_check) != 0) {

    if (!is.na(operator_in)) {

      rel <- rel |>
        filter(
          .data$operator == operator_in
        )

    } else {

      stop("Please specify operator_in")

    }
  }

  ## check that the unit_out specified is valid

  if (!all(is.na(unit_out))) {

    check_unit_out <- unit_alias |>
      filter(.data$alias %in% unit_out) |>
      left_join(
        unit_srp,
        by = "id"
      ) |>
      filter(
        !.data$category %in% rel$id
      )

    if (nrow(check_unit_out) != 0) {

      stop("unit_out does not exist for the relationship derived between parent units")

    }

  }

  ## load operators data

  operators_data <- operators_helper()

  ## join all datasets together and do final unicon_full

  workings <- rel |>
    # find srp for relationship between parent x and y
    left_join(
      unit_srp |>
        distinct(.data$category,
                 srp_unit_out = .data$srp
        ),
      by = c("id" = "category")
    ) |>
    ## join to operators function
    left_join(
      operators_data,
      by = "operator"
    ) |>
    # join to x info
    left_join(
      x_category |>
        rename(x_srp_in = .data$srp),
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
        rename(y_srp_in = .data$srp),
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
      # calculate value in SRP for derived metric
      srp_value_out = pmap_dbl(
        list(
          .data$fun,
          .data$x_value_out,
          .data$y_value_out
        ),
        function(op, x, y)
          do.call(op, list(x, y))
      ),
      # convert from SRP to requested output units (default pull = TRUE returns vector)
      value_out = unicon_full(
        value_in = .data$srp_value_out,
        unit_in = .data$srp_unit_out,
        unit_out = .env$unit_out
      )
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

    workings |>
      mutate(unit_out = dplyr::coalesce(.env$unit_out, .data$srp_unit_out)) |>
      select(
        x_unit_in = .data$x_unit_in,
        x_value_in = .data$x_value_in,
        x_category = .data$x,
        x_srp_unit = .data$x_unit_out,
        x_srp_value = .data$x_value_out,
        y_unit_in = .data$y_unit_in,
        y_value_in = .data$y_value_in,
        y_category = .data$y,
        y_srp_unit = .data$y_unit_out,
        y_srp_value = .data$y_value_out,
        .data$operator,
        category_out = .data$id,
        .data$srp_unit_out,
        .data$srp_value_out,
        .data$unit_out,
        .data$value_out
      )

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


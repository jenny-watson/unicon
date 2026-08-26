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

unicon_advance <- function(x_value_in,
                           y_value_in,
                           x_unit_in,
                           y_unit_in,
                           unit_out = NA,
                           operator_in = NA,
                           pull = TRUE) {
  # checks are minimal as relying in unicon_full checks

  if (length(x_value_in) != length(y_value_in)) {
    stop("Argument `x_value_in` and `y_value_in` must have same length.")
  }

  if (length(unit_out) != length(x_unit_in) && length(unit_out) != 1L) {
    stop("Argument `unit_out` must have length 1 or length(x_unit_in).")
  }


  # if units have 1L, increase to length of values

  n <- length(x_value_in)

  if (length(x_unit_in) == 1L) {
    x_unit_in <- rep(x_unit_in, n)
  }

  if (length(y_unit_in) == 1L) {
    y_unit_in <- rep(y_unit_in, n)
  }

  if (length(unit_out) == 1L) {
    unit_out <- rep(unit_out, n)
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
  ) |>
    distinct()

  y_srp_value <- suppressWarnings(
    suppressMessages(
      unicon_full(
        value_in = y_value_in,
        unit_in = y_unit_in,
        unit_out = NA,
        pull = FALSE
      )
    )
  ) |>
    distinct()

  ## identify what type of metrics x and y are & their srp

  x_category <- filter(
    .unicon_state$unit_srp,
    .data$srp %in% x_srp_value$srp_in
  ) |>
    distinct(
      .data$category,
      .data$srp
    )

  y_category <- filter(
    .unicon_state$unit_srp,
    .data$srp %in% y_srp_value$srp_in
  ) |>
    distinct(
      .data$category,
      .data$srp
    )

  ## figure out if a relationship exists between x and y

  rel <- .unicon_state$relationships |>
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
    stop("`operator_in` does not match the relationship derived between parent units")
  }

  ## special case check:
  ## does the operator need specifying?

  rel_check <- .unicon_state$relationships |>
    count(
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
      stop("Please specify `operator_in`")
    }
  }

  ## check that the unit_out specified is valid

  if (!all(is.na(unit_out))) {
    check_unit_out <- .unicon_state$unit_alias |>
      filter(.data$alias %in% unit_out) |>
      left_join(
        .unicon_state$unit_srp,
        by = "id"
      ) |>
      filter(
        !.data$category %in% rel$id
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
          "x_srp_in" = "srp",
          "x_category" = "category"
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
          "y_srp_in" = "srp",
          "y_category" = "category"
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
      .unicon_state$unit_srp |>
        distinct(
          .data$category,
          srp_unit_out = .data$srp
        ),
      by = c("id" = "category")
    ) |>
    # sort operator out and get fun for map in mutate
    mutate(
      operator_in = if_else(
        !is.na(.data$operator_in),
        .data$operator_in,
        .data$operator
      )
    ) |>
    select(
      -"operator"
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
          .data$fun,
          .data$x_value_out,
          .data$y_value_out
        ),
        function(op, x, y) {
          do.call(op, list(x, y))
        }
      )
    )

  ## do final unicon_full

  final <- unicon_full(
    value_in = workings$srp_value_out,
    unit_in = workings$srp_unit_out,
    unit_out = workings$unit_out,
    pull = FALSE
  ) |>
    distinct() |> # needed in case user values are duplicates, expands next join
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
      "x_category",
      "x_unit_in",
      "x_value_in",
      "x_alias_in",
      "x_id_in",
      "x_id_out",
      "x_error_in",
      "x_error_srp",
      "x_error_out",
      "x_value_srp",
      "y_category",
      "y_unit_in",
      "y_value_in",
      "y_alias_in",
      "y_id_in",
      "y_id_out",
      "y_error_in",
      "y_error_srp",
      "y_error_out",
      "y_value_srp",
      "operator_in",
      category = "id",
      "srp_in",
      "value_in",
      "unit_out",
      "alias_out",
      "id_out",
      "error_in",
      "error_srp",
      "error_out",
      "value_srp",
      "value_out"
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
      .data$operator,
      .data$fun
    )

  operators_data
}

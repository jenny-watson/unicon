#' @title make your own base data for use with unicon's functionality
#' @description
#' If a user wants to use unicon but the unit they want is not in the package
#' data, they are welcome to add their own. This function is for the base data
#' i.e. units cannot be derived from other units. When designing your own data
#' please reference the data available using `unicon_help`. This is used at the
#' users own discretion and they will need to conduct their own checks for data
#' quality.
#' @param id the default name of the metric, needs to be consistent across
#' aliases
#' @param alias alternative names for the metric, many of these can map to id
#' @param category what is the unit measuring?
#' @param srp the standard reference point, the default unit of the category
#' @param slope the difference between the unit and srp unit
#' @param intercept defaults to 0. Only use if relationship is not linear
#'
#' @returns A tibble with columns \code{id}, \code{alias}, \code{category},
#' \code{srp}, \code{slope} and \code{intercept}.
#' @export

unicon_make_own_base_data <- function(id,
                                      alias,
                                      category,
                                      srp,
                                      slope,
                                      intercept = 0) {
  if (any(is.na(id)) ||
        any(is.na(alias)) ||
        any(is.na(category)) ||
        any(is.na(srp)) ||
        any(is.na(slope)) ||
        any(is.na(intercept))) {
    stop(
      "Base unit inputs cannot contain NA values."
    )
  }

  if (any(is.numeric(id)) ||
        any(is.numeric(alias)) ||
        any(is.numeric(category)) ||
        any(is.numeric(srp))) {
    stop("`id`, `alias`, `category` and `srp` need to be characters")
  }

  if (any(is.character(slope)) ||
        any(is.character(intercept))) {
    stop("`slope` and `intercept` need to be numeric")
  }

  lengths <- c(
    id = length(id),
    alias = length(alias),
    category = length(category),
    srp = length(srp),
    slope = length(slope),
    intercept = length(intercept)
  )

  if (length(unique(lengths)) > 1) {
    stop(
      "All vectors supplied to `unicon_make_own_base_data` must be the same ",
      "length. Lengths provided: ",
      paste(names(lengths), lengths, sep = " = ", collapse = ", ")
    )
  }

  if (any(intercept != 0)) {
    warning("`intercept` is not zero, please check this is correct")
  }

  tibble(
    id = id,
    alias = alias,
    category = category,
    srp = srp,
    slope = slope,
    intercept = intercept
  )
}

#' @title Make your own derived data for use with unicon
#' @description
#' If a user wants to use unicon but the derived data they want is not in the
#' package data, they are welcome to add their own. This function is for the
#' derived data i.e. if two measures are calculated to make another measure.
#' When designing your own data please reference the data available using
#' `unicon_help`. This is used at the users own discretion and they will need to
#' conduct their own checks for data quality.
#' @param id Character scalar or vector; the new derived metric
#' @param x Character scalar or vector; the base (numerator) metric that is
#' used to calculate the current metric
#' @param y Character scalar or vector; the base (denominator) metric that is
#' used to calculate the current metric
#' @param operator Character scalar or vector; the operator used to calculate
#' the metric using x and y
#'
#' @returns A tibble with columns \code{id}, \code{x}, \code{y}, and
#' \code{operator}.
#' @export

unicon_make_own_derived_data <- function(id,
                                         x,
                                         y,
                                         operator) {
  if (any(is.na(id)) ||
        any(is.na(x)) ||
        any(is.na(y)) ||
        any(is.na(operator))) {
    stop("Derived unit inputs cannot contain NA values")
  }

  if (any(is.numeric(id)) ||
        any(is.numeric(x)) ||
        any(is.numeric(y)) ||
        any(is.numeric(operator))) {
    stop("`id`, `x`, `y`, `operator` need to be characters")
  }

  lengths <- c(
    id = length(id),
    x = length(x),
    y = length(y),
    operator = length(operator)
  )

  if (length(unique(lengths)) > 1) {
    stop(
      "All vectors supplied to `unicon_make_own_derived_data` must be the ",
      "same length. Lengths provided: ",
      paste(names(lengths), lengths, sep = " = ", collapse = ", ")
    )
  }

  tibble(
    id = id,
    x = x,
    y = y,
    operator = operator
  )
}


#' @title Make your own operators data for use with unicon
#' @description
#' If a user wants to use unicon but the operator they want is not in the
#' package data, they are welcome to add their own. This function is for the
#' operator data i.e. how the units are transformed. When designing your own
#' data please reference the data available using `unicon_help`. This is used at
#' the users own discretion and they will need to conduct their own checks for
#' data quality. This is not likely to be required.
#' @param operator Character scalar or vector; the name of the operator
#' @param id Character scalar or vector; either _ or .
#' @param fun Character scalar or vector; the preserved R function e.g. /, *, +, -
#' @param alias Character scalar or vector; alternative names, many of these
#' can map to id
#'
#' @returns A tibble with columns \code{operator}, \code{id}, \code{fun}, and
#' \code{alias}.
#' @export

unicon_make_own_operators_data <- function(operator,
                                           id,
                                           fun,
                                           alias) {
  if (any(is.na(operator)) ||
        any(is.na(id)) ||
        any(is.na(fun)) ||
        any(is.na(alias))) {
    stop("Operator inputs cannot contain NA values")
  }

  if (any(is.numeric(operator)) ||
        any(is.numeric(id)) ||
        any(is.numeric(fun)) ||
        any(is.numeric(alias))) {
    stop("`operator`, `id`, `fun` and `alias` need to be characters")
  }

  lengths <- c(
    operator = length(operator),
    id = length(id),
    fun = length(fun),
    alias = length(alias)
  )

  if (length(unique(lengths)) > 1) {
    stop(
      "All vectors supplied to `unicon_make_own_operators_data` must be the ",
      "same length. Lengths provided: ",
      paste(names(lengths), lengths, sep = " = ", collapse = ", ")
    )
  }

  tibble(
    operator = operator,
    id = id,
    fun = fun,
    alias = alias
  )
}

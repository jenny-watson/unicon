#' @Title Make own unicon package data
#' @description If data is missing from unicon, users can add there own in. They
#' could even make a request to the authours on Github to request this data be
#' included directly if a use case is large enough.
#'
#' @param base_id the default name of the base metric, needs to be consistent
#' across base aliases
#' @param base_alias alternative names for the base metric, many of these can
#' map to base id
#' @param category what is the unit measuring?
#' @param srp the standard reference point, the default unit of the category
#' @param slope the difference between the unit and srp unit
#' @param intercept defaults to 0. Only use if relationship is not linear
#' @param derived_id the default name of the derived metric
#' @param x the base (numerator) metric that is used to calculate the current
#' metric
#' @param y the base (denominator) metric that is used to calculate the current
#' metric
#' @param operator the default operator name used to calculate the metric using
#' x and y
#' @param operator_id either __ or .
#' @param fun the perserved R function e.g. /, *, +, -
#' @param operator_alias alternative operator names, many of these can map to
#' operator_id
#'
#' @returns A list of three dataframes to use as own data in `unicon_full` and
#' `unicon_lite`.
#' @export

unicon_own = function(base_id = NA,
                      base_alias = NA,
                      category = NA,
                      srp = NA,
                      slope = NA,
                      intercept = NA,
                      derived_id = NA,
                      x = NA,
                      y = NA,
                      operator = NA,
                      operator_id = NA,
                      fun = NA,
                      operator_alias = NA) {

  ## make data that is available

  if(!is.na(base_id) &&
     !is.na(alias) &&
     !is.na(category) &&
     !is.na(srp) &&
     !is.na(slope) &&
     !is.na(intercept)) {

    message("Creating base data")

    own_base_data <- unicon_make_own_base_data(
      id = base_id,
      alias = base_alias,
      category = category,
      srp = srp,
      slope = slope,
      intercept = intercept
    )

  }

  if(!is.na(derived_id) &&
     !is.na(x) &&
     !is.na(y) &&
     !is.na(operator)) {

    message("Creating derived data")

    own_derived_data < - unicon_make_own_derived_data(
      id = derived_id,
      x = x,
      y = y,
      operator = operator
    )

  }

  if(!is.na(operator) &&
     !is.na(operator_id) &&
     !is.na(fun) &&
     !is.na(alias)) {

    message("Creating operator data")

    own_operators_data <- unicon_make_own_operators_data(
      operator = operator,
      id = operator_id,
      fun = fun,
      alias = operator_alias
    )

  }



  ## folders with package .json files

  base_dir <- file.path("inst", "units", "base")

  derived_dir <- file.path("inst", "units", "derived")

  operators_dir <- file.path("inst", "units", "operators")


  ## load data and join to user's data

  if(!is.na(own_base_data)) {

    base_data <- unicon_make_base_data_from_jsons(base_dir) |>
      bind_rows(own_base_data)

  } else {

    base_data <- unicon_make_base_data_from_jsons(base_dir)

  }

  if(!is.na(own_derived_data)) {

    derived_data <- unicon_make_derived_data_from_jsons(derived_dir) |>
      bind_rows(own_derived_data)

  } else {

    derived_data <- unicon_make_derived_data_from_jsons(derived_dir)

  }

  if(!is.na(own_operators_data)) {

    operators_data <- unicon_make_operators_data_from_jsons(operators_dir) |>
      bind_rows(own_operators_data)

  } else {

    operators_data <- unicon_make_operators_data_from_jsons(operators_dir)

  }

  ## join datasets together

  join <- unicon_join_datasets(base_data,
                               derived_data,
                               operators_data)

  ## final datasets

  unit_alias <- unicon_make_unit_alias(join)

  unit_srp <- unicon_make_unit_srp(join)

  unit_models <- unicon_make_unit_models(join)

  return(
    list(
      unit_alias,
      unit_srp,
      unit_models
    )
  )

}

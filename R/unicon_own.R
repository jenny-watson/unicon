#' @Title Make own unicon package data
#' @description If data is missing from unicon, users can add there own in. They
#' could even make a request to the authors on Github to request this data be
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
#' @param fun the preserved R function e.g. /, *, +, -
#' @param operator_alias alternative operator names, many of these can map to
#' operator_id
#'
#' @returns Updated package data.
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

  own_base_data <- NULL
  own_derived_data <- NULL
  own_operators_data <- NULL

  if(!is.na(base_id) &&
     !is.na(base_alias) &&
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

    if(!is.na(derived_id) &&
       !is.na(x) &&
       !is.na(y) &&
       !is.na(operator)) {

      message("Creating derived data")

      own_derived_data <- unicon_make_own_derived_data(
        id = derived_id,
        x = x,
        y = y,
        operator = operator
      )

      if(!is.na(operator_id) &&
         !is.na(fun) &&
         !is.na(operator_alias)) {

        message("Creating operator data")

        own_operators_data <- unicon_make_own_operators_data(
          operator = operator,
          id = operator_id,
          fun = fun,
          alias = operator_alias
        )

      }

    }

  } else {

    stop(
      "Not enough data provided to create a dataset. At minimum, `base_id`, ",
      "`base_alias`, `category`, `srp`, `slope`, and `intercept` must all be ",
      "supplied."
    )

  }



  ## folders with package .json files

  base_dir <- file.path("inst", "units", "base")

  derived_dir <- file.path("inst", "units", "derived")

  operators_dir <- file.path("inst", "units", "operators")


  ## load data and join to user's data

  if(!is.null(own_base_data)) {

    base_data <- unicon_make_base_data_from_jsons(base_dir) |>
      bind_rows(own_base_data)

  } else {

    base_data <- unicon_make_base_data_from_jsons(base_dir)

  }

  if(!is.null(own_derived_data)) {

    derived_data <- unicon_make_derived_data_from_jsons(derived_dir) |>
      bind_rows(own_derived_data)

  } else {

    derived_data <- unicon_make_derived_data_from_jsons(derived_dir)

  }

  if(!is.null(own_operators_data)) {

    operators_data <- unicon_make_operators_data_from_jsons(operators_dir) |>
      bind_rows(own_operators_data)

  } else {

    operators_data <- unicon_make_operators_data_from_jsons(operators_dir)

  }

  ## make every combination of unit category calculations

  relationships <- unicon_make_relationships_data(derived_data)

  ## join datasets together

  ## check derived data categories and operators exist in combined data

  if(!is.null(own_derived_data)) {

    missing_x <- unique(own_derived_data$x[
      !own_derived_data$x %in% base_data$category
    ])

    missing_y <- unique(own_derived_data$y[
      !own_derived_data$y %in% base_data$category
    ])

    if(length(missing_x) > 0 || length(missing_y) > 0) {

      stop(
        "The following categories used in derived data do not exist in the ",
        "base data. Please add base data for these categories first.\n",
        if(length(missing_x) > 0)
          paste0("  x: ", paste(missing_x, collapse = ", ")),
        if(length(missing_y) > 0)
          paste0("\n  y: ", paste(missing_y, collapse = ", "))
      )

    }

    missing_operators <- unique(own_derived_data$operator[
      !own_derived_data$operator %in% operators_data$operator
    ])

    if(length(missing_operators) > 0) {

      stop(
        "The following operators used in derived data do not exist in the ",
        "operator data. Please add operator data for these operators first: ",
        paste(missing_operators, collapse = ", ")
      )

    }

  }

  join <- unicon_join_datasets(base_data,
                               derived_data,
                               operators_data)

  ## final datasets

  alias <- unicon_make_unit_alias(join)

  srp <- unicon_make_unit_srp(join)

  models <- unicon_make_unit_models(join)

  ## replace package data

  .unicon_state$unit_alias <- alias
  .unicon_state$unit_srp <- srp
  .unicon_state$unit_models <- models
  .unicon_state$relationships <- relationships

  .unicon_state$using_custom <- TRUE

}

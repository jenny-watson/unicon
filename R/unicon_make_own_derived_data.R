#' If a user wants to use unicon but the derived data they want is not in the
#' package data, they are welcome to add their own. This function is for the
#' derived data i.e. if two measures are calculated to make another measure.
#' When designing your own data please reference the data available using
#' `unicon_help`. This is used at the users own discretion and they will need to
#' conduct their own checks for data quality.
#' @param id the new derived metric
#' @param x the base (numerator) metric that is used to calculate the current
#' metric
#' @param y the base (denominator) metric that is used to calculate the current
#' metric
#' @param operator the operator used to calculate the metric using x and y
#'
#' @returns
#' @export

unicon_make_own_derived_data = function(id,
                                        x,
                                        y,
                                        operator){

  if(any(is.numeric(id)) |
     any(is.numeric(x)) |
     any(is.numeric(y)) |
     any(is.numeric(operator))){

    stop("`id`, `x`, `y`, `operator` need to be characters")

  }

  df = tibble(id = id,
              x = x,
              y = y,
              operator = operator)

}

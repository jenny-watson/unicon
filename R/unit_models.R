#' The slope and intercept between units and SI units for unicon package
#'
#' Using slope * unit + intercept = SI unit, the relationship between units and
#' SI units can be calculated. Normally intercept = 0 and if units are metric are
#' multiples of 10.
#'
#' @format A data frame with 2 variables and 809 observations:
#' \describe{
#'   \item{id}{An ID for different units}
#'   \item{model}{A list of slope and intercept}
#' }
#'
#' @source Generated internally
"unit_models"

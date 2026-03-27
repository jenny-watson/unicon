#' The relationships between different types of units in the unicon package.
#'
#' The all relationships on how to derive metrics from other available data.
#'
#' @format A data frame with 4 variables and 41 observations:
#' \describe{
#'   \item{category}{The type of unit}
#'   \item{parent_1}{A type of unit used to caluculate the unit in question, numerator}
#'   \item{opertor}{The mathematical relationship between parent 1 and 2 to calculate metric}
#'   \item{parent_2}{A type of unit used to caluculate the unit in question, denominator}
#' }
#'
#' @source Generated internally
"category_relationships"

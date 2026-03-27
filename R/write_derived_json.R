#' @title Write derived unit .json files
#' @description If a user wishes to add to the package's data using their own,
#' they need to create .json files with the relevant information for derived units,
#' assuming the relationship is either a fraction where x is numerator and y is
#' denominator or a multiplication.
#' @param x A character vector of L1. The numerator in fraction.
#' @param y A character vector of L1. The denominator in fraction.
#' @param operator A numeric vector of L1. Either 'divide' or 'multiply'.
#' @param path A file pathway and file name. Note file name will be the unit id.
#' @importFrom jsonlite write_json
#' @export

write_derived_json = function(x,
                              y,
                              operator,
                              path) {


  # Basic validation
  stopifnot(is.character(x), length(x) == 1)
  stopifnot(is.character(y), length(y) == 1)
  stopifnot(operator %in% c("divide", "multiply"), length(operator) == 1)

  # Build the structure
  json_list <- list(
    x = x,
    y = y,
    operator = operator
  )

  # Write JSON
  jsonlite::write_json(
    json_list,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE
  )


}

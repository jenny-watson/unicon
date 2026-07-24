#' If a user wants to use unicon but the operator they want is not in the
#' package data, they are welcome to add their own. This function is for the
#' operator data i.e. how the units are transformed. When designing your own
#' data please reference the data available using `unicon_help`. This is used at
#' the users own discretion and they will need to conduct their own checks for
#' data quality. This is not likely to be required.
#' @param operator the name of the operator
#' @param id either __ or .
#' @param fun the perserved R function e.g. /, *, +, -
#' @param alias alternative names, many of these can map to id
#'
#' @returns
#' @export

unicon_make_own_operators_data = function(operator,
                                          id,
                                          fun,
                                          alias){

  if(any(is.numeric(operator)) |
     any(is.numeric(id)) |
     any(is.numeric(fun)) |
     any(is.numeric(alias))){

    stop("`operator`, `id`, `fun` and `alias` need to be characters")

  }

     df = tibble(operator = operator,
                 id = id,
                 fun = fun,
                 alias = alias)

}

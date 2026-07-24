#' @title make your own base data for use with unicon's functionaility
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
#' @returns
#' @export

unicon_make_own_base_data = function(id,
                                     alias,
                                     category,
                                     srp,
                                     slope,
                                     intercept = 0){

  if(any(is.numeric(id)) |
     any(is.numeric(alias)) |
     any(is.numeric(category)) |
     any(is.numeric(srp))){

    stop("`id`, `alias`, `category` and `srp` need to be characters")

  }

  if(any(is.character(slope)) |
     any(is.character(intercept))){

    stop("`slope` and `intercept` need to be numeric")

  }

  if(any(intercept) != 0){

    warning("`intercept` is not zero, please check this is correct")

  }


  df = tibble(id = id,
              alias = alias,
              category = category,
              srp = srp,
              slope = slope,
              intercept = intercept)


}

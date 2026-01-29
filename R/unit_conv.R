#' unit_conv converts measurement data (e.g. km, gallons, kelvin) into Trinity's standard units.
#' Input *must* be a dataframe.
#' To apply to multiple columns please use `unit_conv_scale`.
#'
#' @param df a data frame
#' @param measure the column that contains the units - must be character
#' @param quantity the column that contains the values - must be integer
#'
#' @returns a dataframe with two additional columns, si and si_quantity
#'
#' @examples
#' d = data.frame(unit = c('inch',
#'                         'cm',
#'                         'meters',
#'                         'mile',
#'                         'yard',
#'                         'stone',
#'                         'pints',
#'                         'mls',
#'                         'k',
#'                         'uk_gallon'),
#'                value = c(10:19))
#'
#' d_si = d |>
#'   unit_conv(measure = unit,
#'             quantity = value)
#'
#' Warning message:
#' In unit_conv(d, measure = unit, quantity = value) :
#'   NA values detected — results may be incomplete



unit_conv = function(df,
                     measure,
                     quantity){

  ## make sure data types are correct

  if (!is.character(dplyr::pull(df, {{measure}}))) {
    stop("measure must be character", call. = FALSE)
  }

  if (!is.numeric(dplyr::pull(df, {{quantity}}))) {
    stop("quantity must be numeric", call. = FALSE)
  }

  ## join and calcs

  dataframe = df |>
    dplyr::left_join(unit_alias, ## join alias to find id
                     dplyr::join_by({{measure}} == .data$alias)) |> ## {{}} allows any column to be passed regardless of name
    dplyr::left_join(unit_models, ## get slope and intercept info
                     dplyr::join_by(.data$id)) |>
    dplyr::left_join(unit_si, ## get SI info
                     dplyr::join_by(.data$id)) |>
    tidyr::unnest_wider(.data$model) |> ## unnest model vars for calcs
    dplyr::mutate(si_quantity = ({{quantity}} * .data$slope) + .data$intercept) |>
    dplyr::select(-c(.data$id, ## drop unneeded cols so df looks same before function with added cols
                     .data$slope,
                     .data$intercept,
                     .data$type,
                     .data$category))

  ## warning if model is missing

  if (any(is.na(dataframe))) {
    warning("NA values detected — results may be incomplete")
  }

  return(dataframe)

}



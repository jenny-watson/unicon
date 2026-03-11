#' unit_conv converts measurement data (e.g. km, gallons, kelvin) into
#' standard units.
#' Input *must* be a dataframe.
#' To apply to multiple columns please use `unit_conv_scale`.
#'
#' @importFrom dplyr left_join mutate select join_by
#' @importFrom tidyr unnest_wider
#' @importFrom rlang .data
#'
#' @param df a data frame
#' @param measure the column that contains the units - must be character
#' @param quantity the column that contains the values - must be integer
#'
#' @returns a dataframe with two additional columns, si and si_quantity
#'
#' @examples
#' d <- data.frame(
#'   unit = c(
#'     "inch",
#'     "cm",
#'     "meters",
#'     "mile",
#'     "yard",
#'     "stone",
#'     "pints",
#'     "mls",
#'     "k",
#'     "uk_gallon"
#'   ),
#'   value = c(10:19)
#' )
#'
#' d_si <- d |>
#'   unit_conv(
#'     measure = unit,
#'     quantity = value
#'   )
#'
#' @export


unit_conv <- function(df,
                      measure,
                      quantity) {
  ## make sure data types are correct

  if (!is.character(pull(df, {{ measure }}))) {
    stop("measure must be character", call. = FALSE)
  }

  if (!is.numeric(pull(df, {{ quantity }}))) {
    stop("quantity must be numeric", call. = FALSE)
  }

  ## join and calcs

  dataframe <- df |>
    left_join(
      unit_alias, ## join alias to find id
      join_by({{ measure }} == "alias")
    ) |> ## {{}} allows any column to be passed regardless of name
    left_join(
      unit_models, ## get slope and intercept info
      by = "id"
    ) |>
    left_join(
      unit_si, ## get SI info
      by = "id"
    ) |>
    unnest_wider(.data$model) |> ## unnest model vars for calcs
    mutate(si_quantity = ({{ quantity }} * .data$slope) + .data$intercept) |>
    select(-c(
      .data$id, ## drop unneeded cols so df looks same before function
      .data$slope,
      .data$intercept,
      .data$type,
      .data$category
    ))

  ## warning if model is missing

  if (any(is.na(dataframe))) {
    warning("NA values detected so results may be incomplete")
  }

  return(dataframe)
}

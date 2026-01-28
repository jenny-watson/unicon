#' unit_conv_scale uses unit_conv() but on multiple columns.
#' Will need to rename cols to have pattern x, x_value for measures, quantities to ensure correct linkage.
#' Input *must* be a dataframe.
#'
#' @param df a data frame
#' @param measure the column that contains the units - must be character
#' @param quantity the column that contains the values - must be integer
#'
#' @returns a dataframe with two additional columns, si and si_quantity
#'
#' @examples
#' df = data.frame(info = letters[1:10],
#'                 length = c(rep('inch', 10)),
#'                 length_value = c(10:19),
#'                 temp = c(rep('k', 10)),
#'                 temp_value = c(360:369))

#' unit_conv_scale(df,
#'                 measures = c('length', 'temp')
#'                 quantities = c('length_value', 'temp_value'))


unit_conv_scale = function(df,
                           measures,
                           quantities){

  ## make sure data types are correct

  if (!is.character(dplyr::pull(df, measures))) {
    stop("measure must be character", call. = FALSE)
  }

  if (!is.numeric(dplyr::pull(df, quantities))) {
    stop("quantity must be numeric", call. = FALSE)
  }


  ## get all other cols apart from ones transforming
  other_cols = setdiff(setdiff(names(df), quantities), measures)


  ## get data into 2 cols so that you can apply unit_conv()

  df_long =

    ## pivot longer = sure there is a better way of doing this
    dplyr::full_join(

      df |>
        dplyr::select(-(all_of(quantities))) |>
        dplyr::pivot_longer(cols = measures,
                            names_to = 'measure',
                            values_to = 'unit'),

      df |>
        dplyr::select(-(all_of(measures))) |>
        dplyr::pivot_longer(cols = quantities,
                            names_to = 'quantity_title',
                            values_to = 'value') |>
        dplyr::mutate(quantity_title = str_remove(quantity_title, '_value')),

      by = c(other_cols,
             'measure' = 'quantity_title')) |>

    ## use unit_conv function
    unit_conv(measure = unit,
              quantity = value) |>


    ## set up cols to do pivot_wider
    dplyr::mutate(measure_value = paste0(measure, '_value'),
                  measure_si = paste0(measure, '_si'),
                  measure_si_value = paste0(measure, '_si_value'))

  ## do pivot_wider separately to ensure names are correct and get data back into original format

  df_wide = df_long |>
    dplyr::select(all_of(other_cols),
                  measure,
                  unit) |>
    dplyr::pivot_wider(id_cols = other_cols,
                       names_from = measure,,
                       values_from = unit) |>

    dplyr::left_join(df_long |>
                       dplyr::select(all_of(other_cols),
                                     measure_value,
                                     value) |>
                       dplyr::pivot_wider(id_cols = other_cols,
                                          names_from = measure_value,
                                          values_from = value),
                     by = other_cols) |>

    dplyr::left_join(df_long |>
                       dplyr::select(all_of(other_cols),
                                     measure_si,
                                     si) |>
                       dplyr::pivot_wider(id_cols = other_cols,
                                          names_from = measure_si,
                                          values_from = si),
                     by = other_cols) |>

    dplyr::left_join(df_long |>
                       dplyr::select(all_of(other_cols),
                                     measure_si_value,
                                     si_quantity) |>
                       dplyr::pivot_wider(id_cols = other_cols,
                                          names_from = measure_si_value,
                                          values_from = si_quantity),
                     by = other_cols)

  ## warning if model is missing

  if (any(is.na(df_wide))) {
    warning("NA values detected — results may be incomplete")
  }

  return(df_wide)

}





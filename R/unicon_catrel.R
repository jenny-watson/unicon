#' @title Unit conversion category relationship
#' @description Using the category_relationship package data, this function uses 'unicon_full' to calculate metrics from other metrics
#' @param
#' @import dplyr purrr
#' @importFrom stringr str_replace_all str_to_lower
#' @export





unicon_catrel = function(cat_out){


  ## make into df for easier calculations
  df = tibble(parent_1_type_in = 'length',
              parent_2_type_in = 'time',
              parent_1_unit_in = 'miles',
              parent_2_unit_in = 'hour',
              parent_1_value_in = c(1,2,3,4,5,6),
              parent_2_value_in = c(9,8,7,5,4,2),
              unit_out = NA) # 'km/day')


  # check unit out and type out same
  # do need type_out = 'speed'?
  # confirm relationship between parent 1 and 2
  # other warnings


  workings = df |>
    # convert both parent metrics to SI first
    bind_cols(parent_1_si_value = unicon_full(value_in = df$parent_1_value_in,
                                              unit_in = df$parent_1_unit_in,
                                              unit_out = NA,
                                              pull = TRUE),
              parent_2_si_value = unicon_full(value_in = df$parent_2_value_in,
                                              unit_in = df$parent_2_unit_in,
                                              unit_out = NA,
                                              pull = TRUE)) |>
    # find relationship between parent 1 and 2
    left_join(category_relationships,
              by = c('parent_1_type_in' = 'parent_1',
                     'parent_2_type_in' = 'parent_2')) |>
    # find SI for relationship between parent 1 and 2
    left_join(unit_si |>
                distinct(category,
                         si_unit_out = si),
              by = 'category') |>
    mutate(
      # calculate value in SI
      si_value_out = case_when(operator == 'divide' ~ parent_1_si_value / parent_2_si_value,
                               operator == 'multiply' ~ parent_1_si_value * parent_2_si_value),
      # assign a unit_out to SI if not already assigned in function
      unit_out = if_else(is.na(unit_out),
                         si_unit_out,
                         unit_out))

  # get value out in assigned units
  value_out = unicon_full(value_in = workings$si_value_out,
                          unit_in = workings$si_unit_out,
                          unit_out = workings$unit_out,
                          pull = TRUE)



}

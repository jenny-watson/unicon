#' @title Make unit_alias dataset
#' @description All unique aliases with matching id
#' @param join_dataset The tibble created by the `unicon_join_datasets` function
#' @returns A dataframe with all "id" and "alias" combinations
#' @import dplyr stringr
#' @export

unicon_make_unit_alias <- function(join_dataset) {

  unit_alias <- join_dataset |>
    distinct(.data$id, .data$alias) |> ## ensure all unique
    ## add in id as alias to ensure all combinations captured
    bind_rows(
      join_dataset |>
        distinct(.data$id) |>
        mutate(alias = .data$id)
    ) |>
    # remove whitespace and upper case
    mutate(alias = str_replace_all(str_to_lower(.data$alias), "\\s+", "")) |>
    distinct() |>
    arrange(.data$id)

  unit_alias

}

#' @title Make unit_srp dataset
#' @description All unique standard reference points
#' @param join_dataset The tibble created by the `unicon_join_datasets` function
#' @returns A dataframe with all "id", "type", "category" and "srp" combinations
#' @import dplyr
#' @export

unicon_make_unit_srp <- function(join_dataset) {

  unit_srp <- join_dataset |>
    distinct(
      .data$id,
      .data$type,
      .data$category,
      .data$srp
    )

  unit_srp

}

#' @title Make unit_model dataset
#' @description All unique models
#' @param join_dataset The tibble created by the `unicon_join_datasets` function
#' @returns A dataframe with all "id" and "model" combinations, where "model" is
#' a list of "slope" and "intercept"
#' @import dplyr
#' @export

unicon_make_unit_models <- function(join_dataset) {

  unit_models <- join_dataset |>
    distinct(
      .data$id,
      .data$slope,
      .data$intercept
    ) |>
    ## add in blanks
    bind_rows(
      tibble(id = NA, slope = NA, intercept = NA)
    ) |>
    ## to get list back
    nest(model = c(slope, intercept)) |>
    # make into list rather than mini dataframes
    mutate(model = map(.data$model, ~ as.list(.x)))

  unit_models

}

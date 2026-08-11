#' @title Load .json files
#' @description Generic helper function for loading json files
#' @param file_pathway The file pathway where .json files are stored. Note this
#' is the top level 'parent' folder; all sub folder wll also be examined for
#' .json files
#' @returns A dataframe with all information from .jsons inside
#' @importFrom jsonlite read_json
#' @import purrr stringr
#' @export


unicon_load_json_files <- function(file_pathway) {
  paths <- list.files(
    path = file_pathway, ## folder pathway
    pattern = "\\.json$", # file type
    recursive = TRUE, ## files inside sub folders
    full.names = TRUE
  )

  # read in all json files in folder
  json <- map(paths, ~ read_json(.x, simplifyVector = FALSE)) |>
    set_names(str_remove(
      basename(paths), ## use file name as list names (rather than numbers)
      ".json" ## remove ext
    ))

  json
}

#' @title Load base data .jsons
#' @description Using the `load_json_files` wrapper function, makes a tibble
#' with the columns "id", "alias", "category", "srp", "slope", "intercept" that
#' were formally lists.
#' @param dir The file pathway where .json files for base data are
#' stored. Please refer to vignette for more information.
#' @import tibble purrr dplyr tidyr
#' @export

unicon_make_base_data_from_jsons <- function(dir) {

  base_data <- imap_dfr(
    unicon_load_json_files(dir),
    ~ tibble(
      id = .y, ## get into df rather than list
      alias = .x$alias,
      category = .x$category,
      srp = .x$srp,
      model = list(.x$model)
    )
  ) |>
    unnest_wider("model") |> # further unlist model
    mutate(alias = as.character(.data$alias)) # was list before

  base_data

}

#' @title Load derived data .jsons
#' @description Using the `load_json_files` wrapper function, makes a tibble
#' with the columns "id", "x", "y", "operator" that were formally lists.
#' @param dir The file pathway where .json files for derived data are
#' stored. Please refer to vignette for more information.
#' @import tibble purrr
#' @export

unicon_make_derived_data_from_jsons <- function(dir) {

  derived_data <- imap_dfr(
    unicon_load_json_files(dir),
    ~ tibble(
      id = .y,
      x = .x$x,
      y = .x$y,
      operator = .x$operator
    )
  )

  derived_data

}

#' @title Load operator data .jsons
#' @description Using the `load_json_files` wrapper function, makes a tibble
#' with the columns "operator", "id", "fun", "alias" that were formally lists.
#' @param dir The file pathway where .json files for operator data are
#' stored. Please refer to vignette for more information.
#' @import tibble purrr
#' @export

unicon_make_operators_data_from_jsons <- function(dir) {

  operators_data <- imap_dfr(
    unicon_load_json_files(dir),
    ~ tibble(
      operator = .y,
      id = .x$id,
      fun = .x$fun,
      alias = .x$alias
    )
  )

  operators_data

}

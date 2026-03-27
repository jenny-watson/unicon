#' @title Load base unit data .json files into a tibble
#' @description Get all base unit data from .json files into a tibble for onward
#' manipulation.
#' @param base_dir A file pathway where base unit .json files are stored.
#' Defaults to package data directory file.path("inst", "units", "base")
#' @importFrom dplyr mutate
#' @importFrom tidyr tibble unnest_wider
#' @keywords internal


load_base_data = function(base_dir = file.path("inst", "units", "base")) {

  base_data <- imap_dfr(
    load_json_files(base_dir), ## use function to load relevant files
    ~ tibble( ## get into df rather than list
      id = .y,
      alias = .x$alias,
      category = .x$category,
      si = .x$si,
      model = list(.x$model)
    )
  ) |>
    unnest_wider(model) |> # further unlist model
    mutate(alias = as.character(alias)) # was list before

  base_data

}


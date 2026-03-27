#' @title Load derived unit data .json files into a tibble
#' @description Get all derived unit data from .json files into a tibble for onward
#' manipulation.
#' @param derived_dir A file pathway where derived unit .json files are stored.
#' Defaults to package data directory file.path("inst", "units", "derived")
#' @importFrom dplyr mutate
#' @importFrom tidyr tibble
#' @importFrom stringr str_remove
#' @keywords internal

load_derived_data = function(derived_dir = file.path("inst", "units", "derived")) {

  derived_data <- imap_dfr(
    load_json_files(derived_dir),
    ~ tibble(
      id = .y,
      x = .x$x,
      y = .x$y,
      operator = .x$operator
    )
  ) |>
    mutate(id = str_remove(str_remove(id, "_1"), "_2"))

  derived_data

}

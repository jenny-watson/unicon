#' @title Load .json files
#' @description Small helper function to load all .json files within a directory
#' and its sub folders.
#' @param file_pathway A file pathway where .json files are stored.
#' E.g. file.path("inst", "units", "base")
#' @importFrom stringr str_remove
#' @importFrom purrr set_names map
#' @importFrom jsonlite read_json
#' @keywords internal

load_json_files <- function(file_pathway) {

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

library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)
library(stringr)

## record environment state
env_in <- ls()

## folders
base_dir <- file.path("inst", "units", "base")
derived_dir <- file.path("inst", "units", "derived")
operators_dir <- file.path("inst", "units", "operators")

## generic function for loading json files
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

## load base data
base_data <- imap_dfr(
  load_json_files(base_dir), ## use function to load relevant files
  ~ tibble(
    id = .y, ## get into df rather than list
    alias = .x$alias,
    category = .x$category,
    srp = .x$srp,
    model = list(.x$model)
  )
) |>
  unnest_wider(model) |> # further unlist model
  mutate(alias = as.character(alias)) # was list before

## load derived data
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

## load operators data
operators_data <- imap_dfr(
  load_json_files(operators_dir),
  ~ tibble(
    operator = .y,
    id = .x$id,
    fun = .x$fun,
    alias = .x$alias
  )
)

## final datasets

# alias
## make sure complete and clean
unit_alias <- join |>
  distinct(id, alias) |> ## ensure all unique
  ## add in folder name as alias to ensure all combinations captured
  bind_rows(
    join |>
      distinct(id) |>
      mutate(alias = id)
  ) |>
  distinct() |>
  arrange(id) |>
  # remove whitespace and upper case
  mutate(alias = str_replace_all(str_to_lower(alias), "\\s+", ""))

# standard units
unit_srp <- join |>
  distinct(
    id,
    type,
    category,
    srp
  )

# models
## add in blanks
unit_models <- join |>
  distinct(
    id,
    slope,
    intercept
  ) |>
  bind_rows(
    tibble(id = NA, slope = NA, intercept = NA)
  ) |>
  ## to get list back
  nest(model = c(slope, intercept)) |>
  # make into list rather than mini dataframes
  mutate(model = map(model, ~ as.list(.x)))


## write to package internal data
usethis::use_data(
  unit_alias,
  unit_models,
  unit_srp,
  overwrite = TRUE,
  internal = TRUE
)

## clean env
rm(list = setdiff(
  ls(),
  env_in
))

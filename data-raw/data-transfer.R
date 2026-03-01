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
  json <- map(paths, ~read_json(.x, simplifyVector = FALSE)) |>
    set_names(str_remove(
      basename(paths), ## use file name as list names (rather than numbers)
      '.json' ## remove ext
    ))

  return(json)
}

## load base data
base_data <- imap_dfr(
  load_json_files(base_dir), ## use function to load relevant files
  ~tibble(
    id = .y, ## get into df rather than list
    alias = .x$alias,
    category = .x$category,
    si = .x$si,
    model = list(.x$model))
) |>
  unnest_wider(model) |> # further unlist model
  mutate(alias = as.character(alias)) # was list before

## load derived data
derived_data <- imap_dfr(
  load_json_files(derived_dir),
  ~tibble(id = .y,
          x = .x$x,
          y = .x$y,
          operator = .x$operator)
)

## load operators data
operators_data <- imap_dfr(
  load_json_files(operators_dir),
  ~tibble(operator = .y,
          id = .x$id,
          fun = .x$fun,
          alias = .x$alias)
)

## join datasets together
join <- derived_data |>

  ## join to x
  left_join(
    base_data |>
      filter(intercept == 0) |> ## is this needed?
      rename_with(~paste0(., ".x")),
    by = c('x' = 'category.x'),
    relationship = "many-to-many"
  ) |>

  ## join to y
  left_join(
    base_data |>
      filter(intercept == 0) |> ## is this needed?
      rename_with(~paste0(., ".y")),
    by = c('y' = 'category.y'),
    relationship = "many-to-many"
  ) |>

  ## join to operators
  left_join(
    operators_data |>
      rename_with(~paste0(., ".o")),
    by = c('operator' = 'operator.o'),
    relationship = "many-to-many"
  ) |>

  ## format and calculate
  mutate(
    category = id,
    id = paste0(id.x, id.o, id.y),
    alias = paste0(alias.x, alias.o, alias.y), # is it a problem per doesnt have spaces?
    si = paste0(si.x, id.o, si.y),
    slope = slope.x/slope.y,
    intercept = 0,
    type = 'derived',
    .keep = "none"
  ) |>

  ## bind to base data
  bind_rows(
    base_data |>
      mutate(type = 'base')
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
## but have this info already so why replicating it/should delete json file?
unit_si <- join |>
  distinct(
    id,
    type,
    category,
    si
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
  unit_si,
  overwrite = TRUE,
  internal = TRUE
)

## clean env
rm(list = setdiff(
  ls(),
  env_in
))

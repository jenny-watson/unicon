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
    si = .x$si,
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
  mutate(id = str_remove(str_remove(id, '_1'), '_2'))

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

## join datasets together
join <- derived_data |>
  ## join to x
  left_join(
    base_data |>
      filter(intercept == 0) |> ## is this needed?
      rename_with(~ paste0(., ".x")),
    by = c("x" = "category.x"),
    relationship = "many-to-many"
  ) |>
  ## join to y
  left_join(
    base_data |>
      filter(intercept == 0) |> ## is this needed?
      rename_with(~ paste0(., ".y")),
    by = c("y" = "category.y"),
    relationship = "many-to-many"
  ) |>
  ## join to operators
  left_join(
    operators_data |>
      rename_with(~ paste0(., ".o")),
    by = c("operator" = "operator.o"),
    relationship = "many-to-many"
  ) |>
  ## format and calculate
  mutate(
    category = id,
    id = paste0(id.x, id.o, id.y),
    alias = paste0(alias.x, alias.o, alias.y), # problem per has no spaces?
    si = paste0(si.x, id.o, si.y),
    slope = slope.x / slope.y,
    intercept = 0,
    type = "derived",
    .keep = "none"
  ) |>
  ## bind to base data
  bind_rows(
    base_data |>
      mutate(type = "base")
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

# category relationships
## unique categories
u_cat = base_data |>
  distinct(category) |>
  bind_rows(
    derived_data |>
      pivot_longer(cols = -operator,
                   names_to = 'type',
                   values_to = 'category')) |>
  distinct(category)

## one copy of each relationship
cat_rel = derived_data |>
  select(den_1 = id,
         num = x,
         den_2 = y)

## all copies of relationships with operators
category_relationships = bind_rows(
  left_join(t,
            d,
            by = c('category' = 'num')),
  left_join(t,
            d,
            by = c('category' = 'den_1')),
  left_join(t,
            d,
            by = c('category' = 'den_2'))) |>
  # get rid of blank joins
  filter(!(is.na(num) & is.na(den_1) & is.na(den_1))) |>
  mutate(operator = if_else(is.na(num),
                            'multiply',
                            'divide'),
         uid = 1:n()) |>
  # remove blank parent cells
  pivot_longer(cols = c(num,
                        den_1,
                        den_2),
               names_to = 'type',
               values_to = 'parent_metric') |>
  filter(!is.na(parent_metric)) |>
  # assign so correct order (matters for divide relationships)
  mutate(parent_type = case_when(operator == 'multiply' & type == 'den_1' ~ 'parent_1',
                                 operator == 'multiply' & type == 'den_2' ~ 'parent_2',
                                 operator == 'divide' & type == 'num' ~ 'parent_1',
                                 operator == 'divide' & type == 'den_1' ~ 'parent_2',
                                 operator == 'divide' & type == 'den_2' ~ 'parent_2')) |>
  pivot_wider(id_cols = c(uid,
                          category,
                          operator),
              names_from = parent_type,
              values_from = parent_metric) |>
  select(category,
         parent_1,
         operator,
         parent_2)

## write to package internal data
usethis::use_data(
  unit_alias,
  unit_models,
  unit_si,
  category_relationships,
  overwrite = TRUE,
  internal = TRUE
)

## clean env
rm(list = setdiff(
  ls(),
  env_in
))

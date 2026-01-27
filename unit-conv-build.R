library(tidyverse)

# derive base unit read in paths and read
fdir <- file.path("inst", "units", "base")
files <- list.files(fdir, recursive = TRUE, pattern = "\\.json$", full.names = FALSE)
paths <- list.files(fdir, recursive = TRUE, pattern = "\\.json$", full.names = TRUE)
split <- str_split(files, "/")
groups <- map_chr(split, ~.x[[1L]])
ids <- str_replace(map_chr(split, ~.x[[2L]]), "\\.json$", "")
base <- map(paths, ~jsonlite::read_json(.x, simplifyVector = TRUE, simplifyDataFrame = FALSE)) %>%
  set_names(ids)

# read in unit standard master
# this is similar to the SI concept but adapted for our needs
si <- jsonlite::read_json(file.path("inst", "units", "si.json"), simplifyVector = FALSE) # needed in build????

# make tabular
# id will be added to aliases and distinct rows taken to avoid pointless repetition
#unit_alias <- distinct(imap_dfr(base, ~tibble(id = .y, alias = c(.y, .x$alias))))
#unit_conv <- imap_dfr(base, ~tibble(id = .y, cat = .x$category, si = .x$si, model = list(.x$model)))
unit_conv <- imap_dfr(base, ~tibble(id = .y, alias = .x$alias, category = .x$category, si = .x$si, model = list(.x$model)))

## derived units
fdir <- file.path("inst", "units", "derived")
derived <- map(
  list.files(fdir, pattern = "\\.json$", full.names = TRUE),
  jsonlite::read_json
) %>%
  set_names(str_replace(list.files(fdir, pattern = "\\.json"), "\\.json", ""))

fdir <- file.path("inst", "units", "operators")
operators <- map(
  list.files(fdir, pattern = "\\.json$", full.names = TRUE),
  ~jsonlite::read_json(.x, simplifyVector = TRUE)
) %>%
  set_names(str_replace(list.files(fdir, pattern = "\\.json"), "\\.json", ""))

# create basic derived schema

# filtering out to units with no offset, no simple formulae for combining
unit_conv_affine <- unit_conv %>%
  filter(map_lgl(model, ~.x$intercept == 0))

derived_conv <- map(derived, ~mutate(crossing(
  rename_all(filter(unit_conv_affine, category == .x$x), ~paste0(.x, ".x")),
  rename_all(filter(unit_conv_affine, category == .x$y), ~paste0(.x, ".y"))
), operator = .x$operator)) %>%
  bind_rows(.id = "category")

# open out by operator alias
derived_conv <- left_join(
  derived_conv,
  map(operators, ~tibble(id.o = .x$id, fun.o = .x$fun, alias.o = .x$alias)) %>%
    bind_rows(.id = "operator") %>%
    mutate(fun.o = map(fun.o, get)),
  by = "operator",
  relationship = "many-to-many"
)

# solve derived conversion ids and models
derived_conv <- derived_conv %>%
  mutate(
    id = paste0(id.x, id.o, id.y),
    alias = paste0(alias.x, alias.o, alias.y),
    category,
    si = paste0(si.x, id.o, si.y),
    model = pmap(
      list(x = model.x, y = model.y, fun = fun.o),
      function(x, y, fun) list(slope = fun(x$slope, y$slope), intercept = 0)
    ),
    .keep = "none"
  )

# combine with id
unit_conv <- bind_rows(list(base = unit_conv, derived = derived_conv), .id = "type")

# separate into distinct components to reduce pkg data size
unit_alias <- distinct(unit_conv, id, alias)
unit_si <- distinct(unit_conv, id, type, category, si)
unit_models <- distinct(unit_conv, id, model)

# final steps for aliases

# add ids as alias so ids may be used in place of aliases without case handling
# filter to distinct to avoid repetition, arrange by id
unit_alias <- bind_rows(
  mutate(distinct(unit_alias, id), alias = id),
  unit_alias
) %>%
  distinct() %>%
  arrange(id)

# remove all whitespace and uppercase characters from aliases
# in-function pre-process to avoid need to over-inflate alias data
unit_alias$alias <- str_replace_all(str_to_lower(unit_alias$alias), "\\s+", "")

# final step for models -- add NA models so mis-joins are handled cleanly
unit_models <- bind_rows(
  tibble(id = NA, model = list(list(slope = NA, intercept = NA))),
  unit_models
)

# remove intermediates
rm(
  fdir, files, groups, ids, paths,
  base, derived, derived_conv, operators, si, split, unit_conv, unit_conv_affine
)

# write out
write_rds(unit_alias, "data/unit-alias.rds")
write_rds(unit_models, "data/unit-models.rds")
write_rds(unit_si, "data/unit-si.rds")

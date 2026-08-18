# retrieve package
pkg <- "unicon"

# unit conversion source #######################################################
# derive base unit read in paths and read
fdir <- system.file(
  "units",
  "base",
  lib.loc = .libPaths(),
  package = pkg
)

files <- list.files(
  fdir,
  recursive = TRUE,
  pattern = "\\.json$",
  full.names = FALSE
)

paths <- list.files(
  fdir,
  recursive = TRUE,
  pattern = "\\.json$",
  full.names = TRUE
)

split <- stringr::str_split(files, "/")

unit_groups <- purrr::map_chr(split, ~ .x[[1L]])

unit_ids <- stringr::str_replace(
  purrr::map_chr(split, ~ .x[[2L]]),
  "\\.json$", ""
)

base <- purrr::map(
  paths,
  ~ jsonlite::read_json(.x,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE
  )
) |>
  rlang::set_names(unit_ids)

# read in unit standard master
# this is similar to the SI concept but adapted for our needs

srp <- jsonlite::read_json(
  system.file("units",
    "srp.json",
    lib.loc = .libPaths(),
    package = pkg
  ),
  simplifyVector = FALSE
)
################################################################################


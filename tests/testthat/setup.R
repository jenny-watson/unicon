# retrieve package
pkg <- "unicon"

# unit conversion source ###########################################################################
# derive base unit read in paths and read
fdir <- system.file("units", "base", lib.loc = .libPaths(), package = pkg)
files <- list.files(fdir, recursive = TRUE, pattern = "\\.json$", full.names = FALSE)
paths <- list.files(fdir, recursive = TRUE, pattern = "\\.json$", full.names = TRUE)
split <- stringr::str_split(files, "/")
unit_groups <- map_chr(split, ~.x[[1L]])
unit_ids <- stringr::str_replace(map_chr(split, ~.x[[2L]]), "\\.json$", "")
base <- map(paths, ~jsonlite::read_json(.x, simplifyVector = TRUE, simplifyDataFrame = FALSE)) %>%
  set_names(unit_ids)

# read in unit standard master
# this is similar to the SI concept but adapted for our needs
si <- jsonlite::read_json(
  system.file("units", "si.json", lib.loc = .libPaths(), package = pkg),
  simplifyVector = FALSE
)
####################################################################################################

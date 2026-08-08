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

## load data

base_data <- unicon_make_base_data_from_jsons(base_dir)

derived_data <- unicon_make_derived_data_from_jsons(derived_dir)

operators_data <- unicon_make_operators_data_from_jsons(operators_dir)

## make every combination of unit category calculations

relationships <- unicon_make_relationships_data(derived_dir)

## join datasets together

join <- unicon_join_datasets(base_data,
                             derived_data,
                             operators_data)

## final datasets

unit_alias <- unicon_make_unit_alias(join)

unit_srp <- unicon_make_unit_srp(join)

unit_models <- unicon_make_unit_models(join)

## write to package internal data

usethis::use_data(
  unit_alias,
  unit_models,
  unit_srp,
  relationships,
  overwrite = TRUE,
  internal = TRUE
)

## clean env

rm(list = setdiff(
  ls(),
  env_in
))

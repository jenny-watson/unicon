## make an environment to point to package data
## can be overwritten if user creates their own

.unicon_state <- new.env(parent = emptyenv())

## Default package logic

.onLoad <- function(libname, pkgname) {

  unicon_reset_units()

}


#' @title Reset the `unit_*` datasets to the package stored ones.
#' Used when package loads or to revert after using custom data.
#' @export

unicon_reset_units <- function() {

  .unicon_state$unit_alias <- unit_alias
  .unicon_state$unit_srp <- unit_srp
  .unicon_state$unit_models <- unit_models

  .unicon_state$using_custom <- FALSE

}

## Internal getters — used by conversion functions for zero-overhead lookup

get_unit_alias <- function() .unicon_state$unit_alias

get_unit_srp <- function() .unicon_state$unit_srp

get_unit_models <- function() .unicon_state$unit_models

#' @title Set custom `unit_*` datasets for use in `unicon_full` and
#' `unicon_lite`.
#' @description Validates the supplied tables and stores them in the internal
#' package state so that all subsequent calls to `unicon_full` /
#' `unicon_lite` use the custom data. Revert at any time with
#' `unicon_reset_units()`.
#' @param unit_alias A data frame with columns `id` (character) and `alias`
#' (character).
#' @param unit_srp A data frame with columns `id` (character) and `srp`
#' (character).
#' @param unit_models A data frame with columns `id` (character) and `model`
#' (list column of named lists with elements `slope` and `intercept`).
#' @export

set_unicon_data <- function(unit_alias, unit_srp, unit_models) {

  ## --- validate unit_alias ---
  if (!is.data.frame(unit_alias)) {
    stop("`unit_alias` must be a data frame.")
  }
  if (!all(c("id", "alias") %in% names(unit_alias))) {
    stop("`unit_alias` must contain columns `id` and `alias`.")
  }
  if (!is.character(unit_alias$id) || !is.character(unit_alias$alias)) {
    stop("`unit_alias$id` and `unit_alias$alias` must be character vectors.")
  }

  ## --- validate unit_srp ---
  if (!is.data.frame(unit_srp)) {
    stop("`unit_srp` must be a data frame.")
  }
  if (!all(c("id", "srp") %in% names(unit_srp))) {
    stop("`unit_srp` must contain columns `id` and `srp`.")
  }
  if (!is.character(unit_srp$id) || !is.character(unit_srp$srp)) {
    stop("`unit_srp$id` and `unit_srp$srp` must be character vectors.")
  }

  ## --- validate unit_models ---
  if (!is.data.frame(unit_models)) {
    stop("`unit_models` must be a data frame.")
  }
  if (!all(c("id", "model") %in% names(unit_models))) {
    stop("`unit_models` must contain columns `id` and `model`.")
  }
  if (!is.character(unit_models$id)) {
    stop("`unit_models$id` must be a character vector.")
  }
  if (!is.list(unit_models$model)) {
    stop("`unit_models$model` must be a list column.")
  }

  .unicon_state$unit_alias  <- unit_alias
  .unicon_state$unit_srp    <- unit_srp
  .unicon_state$unit_models <- unit_models

  .unicon_state$using_custom <- TRUE

}

#' @title Tells user if using their own data or package data
#' @export

unicon_own_status <- function() {

  .unicon_state$using_custom

}


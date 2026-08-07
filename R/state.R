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

#' @title Tells user if using their own data or package data
#' @export

unicon_own_status <- function() {

  .unicon_state$using_custom

}


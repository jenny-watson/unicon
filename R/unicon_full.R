#' @title Universal unit conversion
#' @description Full-service/user-exposable unit conversion. Supply a vector of
#' input values with associated unit aliases and receive conversions to any
#' required output alias. Designed to handle the widest possible range of naming
#' conventions, permutations and plain mis-spellings of commonly used units.
#' This is a wrapper for unicon_lite where just looks up the alias.
#' @param value_in Numeric scalar or vector, values to convert.
#' @param unit_in Character scalar or vector, input units for \code{value_in}.
#' Must be of \code{length(1L)} or \code{length(value_in)}.
#' @param unit_out Character scalar or vector, output units for conversion. Must
#' be of \code{length(1L)} or \code{length(value_in)}. Defaults to \code{NA}; if
#' default is passed, function will return standard reference point (SRP) (which
#' is a standard index (SI) unit) conversion.
#' @param pull Logical; should the function pull out and return the converted
#' values (TRUE) or should a full table with conversion record be returned?
#' Defaults to TRUE.
#' @import dplyr purrr
#' @importFrom stringr str_replace_all str_to_lower
#' @export

unicon_full <- function(value_in,
                        unit_in,
                        unit_out = NA,
                        pull = TRUE) {
  if (!is.numeric(value_in)) stop("Argument `value_in` must be numeric.")
  if (!is.character(unit_in)) stop("Argument `unit_in` must be a character vector.")
  if (!(is.character(unit_out) || (is.logical(unit_out) && all(is.na(unit_out))))) {
    stop("Argument `unit_out` must be a character vector or `NA`.")
  }

  # check, all values must be either length 1 or consistent length
  l1 <- length(value_in)
  l2 <- length(unit_in)
  l3 <- length(unit_out)
  if (l1 == 0L) stop("Argument `value_in` must have length >= 1.")
  if (l2 != l1 && l2 != 1L) stop("Argument `unit_in` must have length 1 or length(value_in).")
  if (l3 != l1 && l3 != 1L) stop("Argument `unit_out` must have length 1 or length(value_in).")

  # message to confirm conversion output if no unit_out given
  if (all(is.na(unit_out))) {
    message("No output unit given. Converting all values to standard reference unit.")
  } else if (any(is.na(unit_out))) {
    message("Output unit missing in some cases. Converting to standard reference unit where missing.")
  }

  # compose output table
  conv_tab_pre <- tibble(
    row_id = seq_along(value_in),
    value_in = value_in,
    unit_in = unit_in,
    unit_out = unit_out,
    alias_in = str_replace_all(str_to_lower(unit_in), "\\s+", ""),
    alias_out = str_replace_all(str_to_lower(unit_out), "\\s+", "")
  ) |>
    # input id
    left_join(
      select(
        unit_alias,
        alias_in = all_of("alias"),
        id_in = all_of("id")
      ),
      by = "alias_in",
      multiple = "any"
    ) |>
    # output id if given
    left_join(
      select(
        unit_alias,
        alias_out = all_of("alias"),
        id_out = all_of("id")
      ),
      by = "alias_out",
      multiple = "any"
    )

  lite_messages <- c(
    "No output ID given. Converting all values to standard reference unit.",
    "Output ID missing in some cases. Converting to standard reference unit where missing."
  )
  lite_warnings <- c(
    "Some input unit IDs were invalid.",
    "Some output unit IDs were invalid.",
    "Some requested conversions were not valid (unit type mismatch)."
  )

  # unicon_lite() preserves row order for vectorised inputs, so row_id can be
  # used to re-attach the original alias metadata after conversion.
  conv_tab_lite <- withCallingHandlers(
    unicon_lite(
      value_in = conv_tab_pre$value_in,
      id_in = conv_tab_pre$id_in,
      id_out = conv_tab_pre$id_out,
      pull = FALSE
    ),
    message = function(cnd) {
      if (conditionMessage(cnd) %in% lite_messages) {
        invokeRestart("muffleMessage")
      }
    },
    warning = function(cnd) {
      if (conditionMessage(cnd) %in% lite_warnings) {
        invokeRestart("muffleWarning")
      }
    }
  )

  if (nrow(conv_tab_lite) != nrow(conv_tab_pre)) {
    stop(sprintf(
      "Internal error: conversion results were misaligned (expected %d rows, got %d).",
      nrow(conv_tab_pre),
      nrow(conv_tab_lite)
    ))
  }

  conv_tab <- left_join(
    select(
      conv_tab_pre,
      row_id,
      unit_in,
      unit_out,
      alias_in,
      alias_out
    ),
    mutate(
      select(
        conv_tab_lite,
        id_in,
        id_srp,
        id_out,
        error_in,
        error_srp,
        error_out,
        value_in,
        value_srp,
        value_out
      ),
      row_id = conv_tab_pre$row_id
    ),
    by = "row_id"
  ) |>
    select(-row_id)

  if (isTRUE(pull)) {
    out <- conv_tab$value_out
    if (any(is.na(out))) {
      warning("Some units failed to convert or match. Set `pull = FALSE` for detailed output.")
    }
    out
  } else {
    if (any(conv_tab$error_in)) {
      warning("Some input units failed to find matches.")
    }
    if (any(conv_tab$error_out)) {
      warning("Some output units failed to find matches.")
    }
    if (any(conv_tab$error_srp, na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }
    conv_tab
  }
}

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
#' @import dplyr
#' @importFrom stringr str_replace_all str_to_lower
#' @export

unicon_full <- function(value_in,
                        unit_in,
                        unit_out = NA,
                        pull = TRUE) {

  # checks inputted data

  # check data type
  if (!is.numeric(value_in)) {
    stop("Argument `value_in` must be numeric.")
  }

  if (!is.character(unit_in)) {
    stop("Argument `unit_in` must be a character vector.")
  }

  if (!(is.character(unit_out) || all(is.na(unit_out)))) {
    stop("Argument `unit_out` must be a character vector or `NA`.")
  }

  # check, all values must be either length 1 or consistent length
  l1 <- length(value_in)
  l2 <- length(unit_in)
  l3 <- length(unit_out)

  if (l1 == 0L) {
    stop("Argument `value_in` must have length >= 1.")
  }

  if (l2 != l1 && l2 != 1L) {
    stop("Argument `unit_in` must have length 1 or length(value_in).")
  }

  if (l3 != l1 && l3 != 1L) {
    stop("Argument `unit_out` must have length 1 or length(value_in).")
  }

  # message to confirm conversion output if no unit_out given
  if (all(is.na(unit_out))) {

    message("No output unit given. Converting all values to standard reference unit.") # nolint

  } else if (any(is.na(unit_out))) {

    message("Output unit missing in some cases. Converting to standard reference unit where missing.") # nolint

  }

  ## get aliases in right format before testing

  alias_in <- str_replace_all(str_to_lower(unit_in), "\\s+", "")
  alias_out <- str_replace_all(str_to_lower(unit_out), "\\s+", "")

  if (any(alias_in %in% .unicon_state$unit_alias$alias) || any(alias_out %in% .unicon_state$unit_alias$alias)) {

    # compose output table
    conv_tab_pre <- tibble(
      value_in = value_in,
      unit_in = unit_in,
      unit_out = unit_out,
      alias_in = alias_in,
      alias_out = alias_out
    ) |>
      # input id
      left_join(
        select(
          .unicon_state$unit_alias,
          alias_in = alias,
          id_in = id
        ),
        by = "alias_in",
        multiple = "any"
      ) |>
      # output id if given
      left_join(
        select(
          .unicon_state$unit_alias,
          alias_out = alias,
          id_out = id
        ),
        by = "alias_out",
        multiple = "any"
      )

    ## apply the unicon_lite function

    conv_tab_lite <- unicon_lite(
      value_in = conv_tab_pre$value_in,
      id_in = conv_tab_pre$id_in,
      id_out = conv_tab_pre$id_out
    )

    ## join so can present alias in output if needed
    ## binding as row order preserved and if id_out is NA, cant join
    ## no row_order as upsets else{} branch

    conv_tab <- bind_cols(
      select(conv_tab_pre, -id_out),
      select(
        conv_tab_lite,
        id_out,
        srp_in,
        error_in,
        error_srp,
        error_out,
        value_srp,
        value_out
      )
    )


  } else {

    conv_tab <- unicon_lite(
      value_in = value_in,
      id_in = unit_in,
      id_out = unit_out
    ) |>
      mutate(
        alias_in = unit_in,
        alias_out = unit_out
      )

  }

  if (isTRUE(pull)) {

    # provide brief warnings

    if (any(is.na(conv_tab$value_out))) {
      warning("Some units failed to convert or had invalid IDs. Set `pull = FALSE` for detailed output.") # nolint
    }

    # provide only value_out

    conv_tab$value_out

  } else {

    # provide detailed warnings

    if (any(conv_tab$error_in, na.rm = TRUE)) {
      warning("Some input units failed to find matches.")
    }
    if (any(conv_tab$error_out, na.rm = TRUE)) {
      warning("Some output units failed to find matches.")
    }
    if (any(conv_tab$error_srp, na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }

    # provide row level info

    conv_tab |>
      select(
        unit_in,
        unit_out,
        alias_in,
        alias_out,
        id_in,
        srp_in, # used to drive calcs, srp_out for check only
        id_out,
        error_in,
        error_srp,
        error_out,
        value_in,
        value_srp,
        value_out
      )

  }
}

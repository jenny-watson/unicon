#' @title Universal unit conversion
#' @description Full-service/user-exposable unit conversion. Supply a vector of
#' input values with associated unit aliases and receive conversions to any
#' required output alias. Designed to handle the widest possible range of naming
#' conventions, permutations and plain mis-spellings of commonly used units.
#' @param value_in Numeric scalar or vector, values to convert.
#' @param unit_in Character scalar or vector, input units for \code{value_in}.
#' Must be of \code{length(1L)} or \code{length(value_in)}.
#' @param unit_out Character scalar or vector, output units for conversion. Must
#' be of \code{length(1L)} or \code{length(value_in)}. Defaults to \code{NA}; if
#' default is passed, function will return standard index (SI) units as
#' conversion.
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
  # check, all values must be either length 1 or consistent length
  l1 <- length(value_in)
  l2 <- length(unit_in)
  l3 <- length(unit_out)
  if (l1 == 0L) stop("Length for value_in argument must be >= 1L")
  if (l2 != l1 && l2 != 1L) stop("Length for unit_in argument incompatible")
  if (l3 != l1 && l3 != 1L) stop("Length for unit_out argument incompatible")

  # compose output table
  conv_tab <- tibble(
    value_in = value_in,
    unit_in = unit_in,
    unit_out = unit_out,
    alias_in = str_replace_all(str_to_lower(.data$unit_in), "\\s+", ""),
    alias_out = str_replace_all(str_to_lower(.data$unit_out), "\\s+", "")
  ) |>
    # input id
    left_join(
      select(
        unit_alias,
        alias_in = .data$alias,
        id_in = .data$id
      ),
      by = "alias_in",
      multiple = "any"
    ) |>
    # output id if given
    left_join(
      select(
        unit_alias,
        alias_out = .data$alias,
        id_out = .data$id
      ),
      by = "alias_out",
      multiple = "any"
    ) |>
    # si unit for input id
    left_join(
      select(
        unit_si,
        id_in = .data$id,
        si_in = .data$si
      ),
      by = "id_in",
      multiple = "any"
    ) |>
    # si unit for output id, needed for checks only
    left_join(
      select(
        unit_si,
        id_out = .data$id,
        si_out = .data$si
      ),
      by = "id_out",
      multiple = "any"
    ) |>
    mutate(
      # user gave inputput units , but no matches
      error_in = is.na(.data$id_in),
      # user gave output units , but no matches
      error_out = !is.na(.data$unit_out) & is.na(.data$id_out),
      # user gave incompatible unit conversion
      # (may be NA if no conversion explicitly spec'd)
      error_si = .data$si_in != .data$si_out,
      # use input si unit as output id if none given by user
      id_out = ifelse(is.na(.data$unit_out),
        .data$si_in,
        .data$id_out
      )
    ) |>
    # model for input <--> si
    left_join(
      rename(
        unit_models,
        id_in = .data$id,
        model_in = .data$model
      ),
      by = "id_in",
      multiple = "any"
    ) |>
    # model for si <--> output
    left_join(
      rename(
        unit_models,
        id_out = .data$id,
        model_out = .data$model
      ),
      by = "id_out",
      multiple = "any"
    )

  # solve conversion models
  conv_tab <- conv_tab |>
    mutate(
      # forward model, input --> si
      value_si = map2_dbl(
        .data$value_in, .data$model_in, ~ .x * .y$slope + .y$intercept
      ),
      # reverse model, si --> output
      value_out = map2_dbl(
        .data$value_si, .data$model_out, ~ (.x - .y$intercept) * 1 / .y$slope
      ),
      # ensure no misleading results produced if unit type mismatches
      value_out = ifelse(.data$error_si %in% TRUE,
        NA_real_,
        .data$value_out
      )
    )

  if (isTRUE(pull)) {
    out <- conv_tab$value_out
    if (any(is.na(out))) {
      warning("Some units failed to convert.
              Set `pull = FALSE` for detailed output.")
    }
    conv_tab$value_out
  } else {
    # full warn on failure
    if (any(conv_tab$error_in)) {
      warning("Some input units failed to find matches.")
    }
    if (any(conv_tab$error_out)) {
      warning("Some output units failed to find matches.")
    }
    if (any(conv_tab$error_si, na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }

    select(
      conv_tab,
      .data$unit_in,
      .data$unit_out,
      .data$alias_in,
      .data$alias_out,
      .data$id_in,
      id_si = .data$si_in, # used to drive calcs, si_out for check only
      .data$id_out,
      .data$error_in,
      .data$error_si,
      .data$error_out,
      .data$value_in,
      .data$value_si,
      .data$value_out
    )
  }
}

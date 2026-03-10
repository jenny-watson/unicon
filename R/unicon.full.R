#' @title Universal unit conversion
#' @description Full-service/user-exposable unit conversion. Supply a vector of input values with
#' associated unit aliases and receive conversions to any required output alias. Designed to handle
#' the widest possible range of naming conventions, permutations and plain mis-spellings of commonly
#' used units.
#' @param value.in Numeric scalar or vector, values to convert.
#' @param unit.in Character scalar or vector, input units for \code{value.in}. Must be of
#' \code{length(1L)} or \code{length(value.in)}.
#' @param unit.out Character scalar or vector, output units for conversion. Must be of
#' \code{length(1L)} or \code{length(value.in)}. Defaults to \code{NA}; if default is passed,
#' function will return standard index (SI) units as conversion.
#' @param pull Logical; should the function pull out and return the converted values (TRUE) or
#' should a full table with conversion record be returned? Defaults to TRUE.
#' @import dplyr purrr
#' @importFrom stringr str_replace_all str_to_lower
#' @export
unicon.full <- function(value.in, unit.in, unit.out = NA, pull = TRUE) {
  # check, all values must be either length 1 or consistent length
  l1 <- length(value.in)
  l2 <- length(unit.in)
  l3 <- length(unit.out)
  if (l1 == 0L) stop("Length for value.in argument must be >= 1L")
  if (l2 != l1 && l2 != 1L) stop("Length for unit.in argument incompatible")
  if (l3 != l1 && l3 != 1L) stop("Length for unit.out argument incompatible")

  # compose output table
  conv_tab <- tibble(
    value.in = value.in,
    unit.in = unit.in,
    unit.out = unit.out,
    alias.in = str_replace_all(str_to_lower(.data$unit.in), "\\s+", ""),
    alias.out = str_replace_all(str_to_lower(.data$unit.out), "\\s+", "")
  ) %>%
    left_join(select(unit_alias, alias.in = .data$alias, id.in = .data$id), by = "alias.in", multiple = "any") %>% # input id
    left_join(select(unit_alias, alias.out = .data$alias, id.out = .data$id), by = "alias.out", multiple = "any") %>% # output id if given
    left_join(select(unit_si, id.in = .data$id, si.in = .data$si), by = "id.in", multiple = "any") %>% # si unit for input id
    left_join(select(unit_si, id.out = .data$id, si.out = .data$si), by = "id.out", multiple = "any") %>% # si unit for output id, needed for checks only
    mutate(
      error.in = is.na(.data$id.in), # user gave inputput units , but no matches
      error.out = !is.na(.data$unit.out) & is.na(.data$id.out), # user gave output units , but no matches
      error.si = .data$si.in != .data$si.out, # user gave incompatible unit conversion (may be NA if no conversion explicitly spec'd)
      id.out = ifelse(is.na(.data$unit.out), .data$si.in, .data$id.out) # use input si unit as output id if none given by user
    ) %>%
    left_join(rename(unit_models, id.in = .data$id, model.in = .data$model), by = "id.in", multiple = "any") %>% # model for input <--> si
    left_join(rename(unit_models, id.out = .data$id, model.out = .data$model), by = "id.out", multiple = "any") # model for si <--> output

  # solve conversion models
  conv_tab <- conv_tab %>%
    mutate(
      value.si = map2_dbl(.data$value.in, .data$model.in, ~ .x * .y$slope + .y$intercept), # forward model, input --> si
      value.out = map2_dbl(.data$value.si, .data$model.out, ~ (.x - .y$intercept) * 1 / .y$slope), # reverse model, si --> output
      value.out = ifelse(.data$error.si %in% TRUE, NA_real_, .data$value.out) # ensure no misleading results produced if unit type mismatches
    )

  if (isTRUE(pull)) {
    out <- conv_tab$value.out
    if (any(is.na(out))) warning("Some units failed to convert. Set `pull = FALSE` for detailed output.")
    return(conv_tab$value.out)
  } else {
    # full warn on failure
    if (any(conv_tab$error.in)) warning("Some input units failed to find matches.")
    if (any(conv_tab$error.out)) warning("Some output units failed to find matches.")
    if (any(conv_tab$error.si, na.rm = TRUE)) warning("Some requested conversions were not valid (unit type mismatch).")

    return(select(
      conv_tab,
      .data$unit.in,
      .data$unit.out,
      .data$alias.in,
      .data$alias.out,
      .data$id.in,
      id.si = .data$si.in, # used to drive calcs, si.out for check only
      .data$id.out,
      .data$error.in,
      .data$error.si,
      .data$error.out,
      .data$value.in,
      .data$value.si,
      .data$value.out
    ))
  }
}

#' @title Universal unit conversion
#' @description Lite/internal use function for unit conversion; supply a vector of input values with
#' associated unit IDs and receive conversions to any required outputs.
#' @inheritParams unicon.full
#' @param id.in Character scalar or vector, input unit ID(s) for \code{value.in}. Must be of
#' \code{length(1L)} or \code{length(value.in)}.
#' @param id.out Character scalar or vector, output unit ID(s) for conversion. Must be of
#' \code{length(1L)} or \code{length(value.in)}. Defaults to \code{NA}; if default is passed,
#' function will return standard index (SI) units as conversion.
#' @import dplyr purrr
#' @importFrom tidyr replace_na
#' @export
unicon.lite <- function(value.in, id.in, id.out = NA, pull = TRUE) {
  # check, all values must be either length 1 or consistent length
  l1 <- length(value.in)
  l2 <- length(id.in)
  l3 <- length(id.out)
  if (l1 == 0L) stop("Length for value.in argument must be >= 1L")
  if (l2 != l1 && l2 != 1L) stop("Length for id.in argument incompatible")
  if (l3 != l1 && l3 != 1L) stop("Length for id.out argument incompatible")

  # compose output table
  conv_tab <- tibble(
    value.in = value.in,
    id.in = id.in,
    id.out = id.out
  ) %>%
    left_join(select(unit_si, id.in = .data$id, si.in = .data$si), by = "id.in", multiple = "any") %>% # si unit for input id
    left_join(select(unit_si, id.out = .data$id, si.out = .data$si), by = "id.out", multiple = "any") %>% # si unit for output id, needed for checks only
    mutate(
      error.in = is.na(.data$si.in), # user gave input units, but no matches
      error.out = !is.na(.data$id.out) & is.na(.data$si.out), # user gave output units, but no matches
      error.si = .data$si.in != .data$si.out, # user gave incompatible unit conversion
      id.out = ifelse(is.na(.data$id.out), .data$si.in, .data$id.out), # use si unit as output id if none given by user
    ) %>%
    left_join(rename(unit_models, id.in = .data$id, model.in = .data$model), by = "id.in", multiple = "any") %>% # model for input <--> si
    left_join(rename(unit_models, id.out = .data$id, model.out = .data$model), by = "id.out", multiple = "any") # model for si <--> output

  # replace mis-joined models, needed in case user has provided incorrect ids
  conv_tab <- conv_tab %>%
    replace_na(list(
      model.in = list(list(slope = NA, intercept = NA)),
      model.out = list(list(slope = NA, intercept = NA))
    ))

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
    if (any(conv_tab$error.in)) warning("Some input unit IDs were invalid.")
    if (any(conv_tab$error.out)) warning("Some output unit IDs were invalid.")
    if (any(conv_tab$error.si, na.rm = TRUE)) warning("Some requested conversions were not valid (unit type mismatch).")

    return(select(
      conv_tab,
      .data$id.in,
      .data$id.out,
      id.si = .data$si.in, # used to drive calcs, si.out for check only
      .data$error.in,
      .data$error.si,
      .data$error.out,
      .data$value.in,
      .data$value.si,
      .data$value.out
    ))
  }
}

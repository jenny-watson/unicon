#' @title Universal unit conversion
#' @description Lite/internal use function for unit conversion; supply a vector
#' of input values with associated unit IDs and receive conversions to any
#' required outputs.
#' @inheritParams unicon_full
#' @param id_in Character scalar or vector, input unit ID(s) for
#' \code{value_in}. Must be of \code{length(1L)} or \code{length(value_in)}.
#' @param id_out Character scalar or vector, output unit ID(s) for conversion.
#' Must be of \code{length(1L)} or \code{length(value_in)}. Defaults to
#' \code{NA}; if default is passed, function will return standard index (SI)
#' units as conversion.
#' @import dplyr purrr
#' @importFrom tidyr replace_na
#' @export

unicon_lite <- function(value_in,
                        id_in,
                        id_out = NA,
                        pull = TRUE) {
  # check, all values must be either length 1 or consistent length
  l1 <- length(value_in)
  l2 <- length(id_in)
  l3 <- length(id_out)
  if (l1 == 0L) stop("Length for value_in argument must be >= 1L")
  if (l2 != l1 && l2 != 1L) stop("Length for id_in argument incompatible")
  if (l3 != l1 && l3 != 1L) stop("Length for id_out argument incompatible")

  # compose output table
  conv_tab <- tibble(
    value_in = value_in,
    id_in = id_in,
    id_out = id_out
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
      # user gave input units, but no matches
      error_in = is.na(.data$si_in),
      # user gave output units, but no matches
      error_out = !is.na(.data$id_out) & is.na(.data$si_out),
      # user gave incompatible unit conversion
      error_si = .data$si_in != .data$si_out,
      # use si unit as output id if none given by user
      id_out = ifelse(is.na(.data$id_out), .data$si_in, .data$id_out),
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

  # replace mis-joined models, needed in case user has provided incorrect ids
  conv_tab <- conv_tab |>
    replace_na(
      list(
        model_in = list(
          list(
            slope = NA,
            intercept = NA
          )
        ),
        model_out = list(
          list(
            slope = NA,
            intercept = NA
          )
        )
      )
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
      warning("Some input unit IDs were invalid.")
    }
    if (any(conv_tab$error_out)) {
      warning("Some output unit IDs were invalid.")
    }
    if (any(conv_tab$error_si, na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }

    select(
      conv_tab,
      .data$id_in,
      .data$id_out,
      id_si = .data$si_in, # used to drive calcs, si_out for check only
      .data$error_in,
      .data$error_si,
      .data$error_out,
      .data$value_in,
      .data$value_si,
      .data$value_out
    )
  }
}

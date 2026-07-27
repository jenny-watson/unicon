#' @title Universal unit conversion
#' @description Lite/internal use function for unit conversion; supply a vector
#' of input values with associated unit IDs and receive conversions to any
#' required outputs.
#' @inheritParams unicon_full
#' @param id_in Character scalar or vector, input unit ID(s) for
#' \code{value_in}. Must be of \code{length(1L)} or \code{length(value_in)}.
#' @param id_out Character scalar or vector, output unit ID(s) for conversion.
#' Must be of \code{length(1L)} or \code{length(value_in)}. Defaults to
#' \code{NA}; if default is passed, function will return the standard
#' reference point (SRP), which is a standard index (SI) unit measure.
#' @import dplyr purrr
#' @importFrom tidyr replace_na
#' @export

unicon_lite <- function(value_in,
                        id_in,
                        id_out = NA,
                        pull = TRUE) {

  # check data types
  if (!is.numeric(value_in)) stop("Argument `value_in` must be numeric.")
  if (!is.character(id_in)) stop("Argument `id_in` must be a character vector.")
  if (!(is.character(id_out) || (is.logical(id_out) && all(is.na(id_out))))) {
    stop("Argument `id_out` must be a character vector or `NA`.")
  }

  # check, all values must be either length 1 or consistent length
  l1 <- length(value_in)
  l2 <- length(id_in)
  l3 <- length(id_out)
  if (l1 == 0L) stop("Argument `value_in` must have length >= 1.")
  if (l2 != l1 && l2 != 1L) stop("Argument `id_in` must have length 1 or length(value_in).")
  if (l3 != l1 && l3 != 1L) stop("Argument `id_out` must have length 1 or length(value_in).")

  # message to confirm conversion output if no unit_out given
  if (all(is.na(id_out))) {
    message("No output ID given. Converting all values to standard reference unit.")
  } else if (any(is.na(id_out))) {
    message("Output ID missing in some cases. Converting to standard reference unit where missing.")
  }

  # compose output table
  conv_tab <- tibble(
    value_in = value_in,
    id_in = id_in,
    id_out = id_out
  ) |>
    # srp unit for input id
    left_join(
      select(
        unit_srp,
        id_in = all_of("id"),
        srp_in = all_of("srp")
      ),
      by = "id_in",
      multiple = "any"
    ) |>
    # srp unit for output id, needed for checks only
    left_join(
      select(
        unit_srp,
        id_out = all_of("id"),
        srp_out = all_of("srp")
      ),
      by = "id_out",
      multiple = "any"
    ) |>
    mutate(
      # user gave input units, but no matches
      error_in = is.na(.data$srp_in),
      # user gave output units, but no matches
      error_out = !is.na(.data$id_out) & is.na(.data$srp_out),
      # user gave incompatible unit conversion
      error_srp = .data$srp_in != .data$srp_out,
      # use srp unit as output id if none given by user
      id_out = ifelse(is.na(.data$id_out), .data$srp_in, .data$id_out),
    ) |>
    # model for input <--> srp
    left_join(
      rename(
        unit_models,
        id_in = all_of("id"),
        model_in = all_of("model")
      ),
      by = "id_in",
      multiple = "any"
    ) |>
    # model for srp <--> output
    left_join(
      rename(
        unit_models,
        id_out = all_of("id"),
        model_out = all_of("model")
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
      # forward model, input --> srp
      value_srp = map2_dbl(
        .data$value_in, .data$model_in, ~ .x * .y$slope + .y$intercept
      ),
      # reverse model, srp --> output
      value_out = map2_dbl(
        .data$value_srp, .data$model_out, ~ (.x - .y$intercept) * 1 / .y$slope
      ),
      # ensure no misleading results produced if unit type mismatches
      value_out = ifelse(.data$error_srp %in% TRUE,
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
    if (any(conv_tab$error_srp, na.rm = TRUE)) {
      warning("Some requested conversions were not valid (unit type mismatch).")
    }

    select(
      conv_tab,
      id_in,
      id_out,
      id_srp = srp_in, # used to drive calcs, srp_out for check only
      error_in,
      error_srp,
      error_out,
      value_in,
      value_srp,
      value_out
    )
  }
}

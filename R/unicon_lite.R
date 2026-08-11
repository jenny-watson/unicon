#' @title Universal unit conversion
#' @description The function that does the actual unit conversion and heavy
#' lifting. To only be used as part of unicon_full to ensure data checks
#' completed.
#' @param value_in Numeric scalar or vector, values to convert.
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
                        id_out = NA) {


  # compose output table

  conv_tab_tib <- tibble(
    value_in = value_in,
    id_in = id_in,
    id_out = id_out
  ) |>
    # srp unit for input id
    left_join(
      select(
        .unicon_state$unit_srp,
        id_in = id,
        srp_in = srp
      ),
      by = "id_in",
      multiple = "any"
    ) |>
    # srp unit for output id, needed for checks only
    left_join(
      select(
        .unicon_state$unit_srp,
        id_out = id,
        srp_out = srp
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
        .unicon_state$unit_models,
        id_in = id,
        model_in = model
      ),
      by = "id_in",
      multiple = "any"
    ) |>
    # model for srp <--> output
    left_join(
      rename(
        .unicon_state$unit_models,
        id_out = id,
        model_out = model
      ),
      by = "id_out",
      multiple = "any"
    )

  # replace mis-joined models, needed in case user has provided incorrect ids
  conv_tab_na <- conv_tab_tib |>
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
  conv_tab <- conv_tab_na |>
    mutate(
      # forward model, input --> srp
      value_srp = map2_dbl(
        value_in,
        model_in,
        ~ .x * .y$slope + .y$intercept
      ),
      # reverse model, srp --> output
      value_out = map2_dbl(
        value_srp,
        model_out,
        ~ (.x - .y$intercept) * 1 / .y$slope
      ),
      # ensure no misleading results produced if unit type mismatches
      value_out = ifelse(.data$error_srp %in% TRUE,
        NA_real_,
        .data$value_out
      )
    )

  conv_tab |>
    select(
      id_in,
      id_out,
      srp_in, # used to drive calcs, srp_out for check only
      error_in,
      error_srp,
      error_out,
      value_in,
      value_srp,
      value_out
    )

}

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

  ## check if aliases are used

  if(any(unit_in %in% unit_alias$alias) | any(unit_out %in% unit_alias$alias)){

    # compose output table
    conv_tab_pre <- tibble(
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
      )

    ## apply the unicon_lite function

    conv_tab_lite = unicon_lite(value_in = conv_tab_pre$value_in,
                                id_in = conv_tab_pre$id_in,
                                id_out = conv_tab_pre$id_out,
                                pull = FALSE)

    ## add in extra columns inc aliases

    conv_tab = select(
        conv_tab_lite,
        conv_tab_pre$unit_in,
        conv_tab_pre$unit_out,
        conv_tab_pre$alias_in,
        conv_tab_pre$alias_out,
        .data$id_in,
        id_srp = .data$srp_in, # used to drive calcs, srp_out for check only
        .data$id_out,
        .data$error_in,
        .data$error_srp,
        .data$error_out,
        .data$value_in,
        .data$value_srp,
        .data$value_out
      )

  } else {

    conv_tab = unicon_lite(value_in = value_in,
                           id_in = unit_in,
                           id_out = unit_out,
                           pull = FALSE)

  }

  if(pull == TRUE){

    return(conv_tab$value_out)

  } else {

    return(conv_tab)

  }


}



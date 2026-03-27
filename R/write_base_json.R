#' @title Write base unit .json files
#' @description If a user wishes to add to the package's data using their own,
#' they need to create .json files with the relevant information for base units.
#' @param category A character vector of L1. The type of measurement the unit is.
#' @param si A character vector of L1. The SI unit that the package will
#' automatically convert to.
#' @param slope A numeric vector of L1. The ratio between the unit in relation to the SI unit.
#' @param intercept A numeric vector of L1. Defaults to 0. Only use if relationship
#' betwen created metric and SI starts at different points e.g. celcius and kelvin.
#' @param alias A character vector. A list of names used to describe the unit being added.
#' @param path A file pathway and file name. Note file name will be the unit id.
#' @importFrom jsonlite write_json
#' @export

write_base_json = function(category,
                           si,
                           slope,
                           intercept = 0,
                           alias,
                           path) {


    # Basic validation
    stopifnot(is.character(category), length(category) == 1)
    stopifnot(is.character(si), length(si) == 1)
    stopifnot(is.numeric(slope), length(slope) == 1)
    stopifnot(is.numeric(intercept), length(intercept) == 1)
    stopifnot(is.character(alias))

    # Build the structure
    json_list <- list(
      category = category,
      si = si,
      model = list(
        slope = slope,
        intercept = intercept
      ),
      alias = alias
    )

    # Write JSON
    jsonlite::write_json(
      json_list,
      path = path,
      pretty = TRUE,
      auto_unbox = TRUE
    )


}

#' @title Deriving and calculating category relationship between two metrics
#' @description Uses \code{unicon_full()} to convert parent metrics to standard
#' reference point (SRP) units, calculate a derived metric, then optionally
#' convert to a requested output unit.
#' @param parent_1_unit_in Character scalar or vector, first parent units.
#' @param parent_2_unit_in Character scalar or vector, second parent units.
#' @param parent_1_value_in Numeric scalar or vector, first parent values.
#' @param parent_2_value_in Numeric scalar or vector, second parent values.
#' @param unit_out Character scalar or vector, requested output units. Defaults
#' to \code{NA}, which returns SRP units.
#' @param operator_in Optional operator override, either \code{"divide"} or
#' \code{"multiply"}, required when parent categories support more than one
#' relationship.
#' @param pull Logical; should the converted values be returned directly
#' (\code{TRUE}) or should a full working table be returned (\code{FALSE})?
#' @export

.unicon_category_relationships <- function() {
  data.frame(
    category = c(
      "amount_of_substance", "area", "force", "length", "mass", "mass",
      "mass", "volume", "volume", "area", "length", "area_density",
      "concentration", "mass_fraction", "pressure", "speed",
      "volume_density", "volume_fraction", "area", "area", "length",
      "mass", "time", "volume", "volume", "volume"
    ),
    parent_1 = c(
      "concentration", "length", "pressure", "speed", "area_density",
      "mass_fraction", "volume_density", "area", "volume_fraction",
      "volume", "area", "mass", "amount_of_substance", "mass", "force",
      "length", "mass", "volume", "mass", "force", "volume", "mass",
      "length", "amount_of_substance", "mass", "volume"
    ),
    operator = c(
      "multiply", "multiply", "multiply", "multiply", "multiply",
      "multiply", "multiply", "multiply", "multiply", "divide", "divide",
      "divide", "divide", "divide", "divide", "divide", "divide", "divide",
      "divide", "divide", "divide", "divide", "divide", "divide", "divide",
      "divide"
    ),
    parent_2 = c(
      "volume", "length", "area", "time", "area", "mass", "volume",
      "length", "volume", "length", "length", "area", "volume", "mass",
      "area", "time", "volume", "volume", "area_density", "pressure",
      "area", "mass_fraction", "speed", "concentration", "volume_density",
      "volume_fraction"
    ),
    stringsAsFactors = FALSE
  )
}

unicon_catrel <- function(parent_1_unit_in,
                          parent_2_unit_in,
                          parent_1_value_in,
                          parent_2_value_in,
                          unit_out = NA,
                          operator_in = NA,
                          pull = TRUE) {
  lens <- c(
    parent_1_unit_in = length(parent_1_unit_in),
    parent_2_unit_in = length(parent_2_unit_in),
    parent_1_value_in = length(parent_1_value_in),
    parent_2_value_in = length(parent_2_value_in),
    unit_out = length(unit_out),
    operator_in = length(operator_in)
  )

  if (lens[["parent_1_value_in"]] == 0L) {
    stop("Length for parent_1_value_in argument must be >= 1L", call. = FALSE)
  }

  if (lens[["parent_2_value_in"]] == 0L) {
    stop("Length for parent_2_value_in argument must be >= 1L", call. = FALSE)
  }

  target_length <- max(lens)

  validate_length <- function(name) {
    if (!(lens[[name]] %in% c(1L, target_length))) {
      stop(
        sprintf("Length for %s argument incompatible", name),
        call. = FALSE
      )
    }
  }

  invisible(lapply(names(lens), validate_length))

  recycle <- function(x) rep(x, length.out = target_length)
  normalize_alias <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      stringr::str_replace_all(stringr::str_to_lower(x), "\\s+", "")
    )
  }

  parent_1_unit_in <- recycle(parent_1_unit_in)
  parent_2_unit_in <- recycle(parent_2_unit_in)
  parent_1_value_in <- recycle(parent_1_value_in)
  parent_2_value_in <- recycle(parent_2_value_in)
  unit_out <- recycle(unit_out)
  operator_in <- recycle(as.character(operator_in))

  if (any(!is.na(operator_in) & !(operator_in %in% c("divide", "multiply")))) {
    stop("operator_in must be one of 'divide' or 'multiply'", call. = FALSE)
  }

  alias_lookup <- unit_alias |>
    left_join(
      select(
        unit_srp,
        id,
        category,
        srp
      ),
      by = "id",
      multiple = "any"
    )

  base <- tibble(
    row_id = seq_len(target_length),
    parent_1_unit_in = parent_1_unit_in,
    parent_2_unit_in = parent_2_unit_in,
    parent_1_value_in = parent_1_value_in,
    parent_2_value_in = parent_2_value_in,
    unit_out = unit_out,
    operator_in = operator_in,
    parent_1_alias = normalize_alias(parent_1_unit_in),
    parent_2_alias = normalize_alias(parent_2_unit_in),
    unit_out_alias = normalize_alias(unit_out)
  ) |>
    left_join(
      alias_lookup |>
        select(
          parent_1_alias = .data$alias,
          parent_1_id = .data$id,
          parent_1_category = .data$category,
          parent_1_srp = .data$srp
        ),
      by = "parent_1_alias",
      multiple = "any"
    ) |>
    left_join(
      alias_lookup |>
        select(
          parent_2_alias = .data$alias,
          parent_2_id = .data$id,
          parent_2_category = .data$category,
          parent_2_srp = .data$srp
        ),
      by = "parent_2_alias",
      multiple = "any"
    )

  if (any(is.na(base$parent_1_id))) {
    stop("Some parent_1_unit_in values failed to find matches", call. = FALSE)
  }

  if (any(is.na(base$parent_2_id))) {
    stop("Some parent_2_unit_in values failed to find matches", call. = FALSE)
  }

  base <- base |>
    mutate(
      parent_1_srp_value = suppressWarnings(suppressMessages(unicon_full(
        value_in = .data$parent_1_value_in,
        unit_in = .data$parent_1_unit_in,
        unit_out = NA,
        pull = TRUE
      ))),
      parent_2_srp_value = suppressWarnings(suppressMessages(unicon_full(
        value_in = .data$parent_2_value_in,
        unit_in = .data$parent_2_unit_in,
        unit_out = NA,
        pull = TRUE
      )))
    )

  category_relationships <- .unicon_category_relationships()

  resolve_relationships <- function(data) {
    data |>
      left_join(
        category_relationships,
        by = c(
          "parent_1_category" = "parent_1",
          "parent_2_category" = "parent_2"
        )
      )
  }

  relationship_check <- resolve_relationships(base) |>
    filter(!is.na(.data$category))

  missing_rows <- setdiff(base$row_id, relationship_check$row_id)

  if (length(missing_rows) > 0L) {
    swapped_base <- base |>
      filter(.data$row_id %in% missing_rows)

    swapped <- tibble(
      row_id = swapped_base$row_id,
      parent_1_unit_in = swapped_base$parent_2_unit_in,
      parent_2_unit_in = swapped_base$parent_1_unit_in,
      parent_1_value_in = swapped_base$parent_2_value_in,
      parent_2_value_in = swapped_base$parent_1_value_in,
      unit_out = swapped_base$unit_out,
      operator_in = swapped_base$operator_in,
      parent_1_alias = swapped_base$parent_2_alias,
      parent_2_alias = swapped_base$parent_1_alias,
      unit_out_alias = swapped_base$unit_out_alias,
      parent_1_id = swapped_base$parent_2_id,
      parent_2_id = swapped_base$parent_1_id,
      parent_1_category = swapped_base$parent_2_category,
      parent_2_category = swapped_base$parent_1_category,
      parent_1_srp = swapped_base$parent_2_srp,
      parent_2_srp = swapped_base$parent_1_srp,
      parent_1_srp_value = swapped_base$parent_2_srp_value,
      parent_2_srp_value = swapped_base$parent_1_srp_value
    ) |>
      resolve_relationships() |>
      filter(!is.na(.data$category))

    relationship_check <- bind_rows(relationship_check, swapped)
  }

  if (!all(base$row_id %in% relationship_check$row_id)) {
    stop("There is no recorded relationship between parent units", call. = FALSE)
  }

  relationship_check <- relationship_check |>
    filter(is.na(.data$operator_in) | .data$operator == .data$operator_in)

  if (any(!is.na(base$operator_in)) &&
      !all(base$row_id[!is.na(base$operator_in)] %in% relationship_check$row_id)) {
    stop(
      "operator_in does not match the relationship derived between parent units",
      call. = FALSE
    )
  }

  ambiguous <- relationship_check |>
    count(.data$row_id) |>
    filter(.data$n > 1L)

  if (nrow(ambiguous) > 0L) {
    stop("Please specify operator_in", call. = FALSE)
  }

  relationship_check <- relationship_check |>
    left_join(
      alias_lookup |>
        select(
          unit_out_alias = .data$alias,
          unit_out_id = .data$id,
          unit_out_category = .data$category
        ),
      by = "unit_out_alias",
      multiple = "any"
    )

  if (any(!is.na(relationship_check$unit_out_alias) &
          is.na(relationship_check$unit_out_id))) {
    stop("Some unit_out values failed to find matches", call. = FALSE)
  }

  if (any(!is.na(relationship_check$unit_out_alias) &
          relationship_check$unit_out_category != relationship_check$category,
        na.rm = TRUE)) {
    stop(
      "unit_out does not exist for the relationship derived between parent units",
      call. = FALSE
    )
  }

  workings <- relationship_check |>
    left_join(
      unit_srp |>
        distinct(
          category = .data$category,
          srp_unit_out = .data$srp
        ),
      by = "category",
      multiple = "any"
    ) |>
    mutate(
      srp_value_out = case_when(
        .data$operator == "divide" ~ .data$parent_1_srp_value / .data$parent_2_srp_value,
        .data$operator == "multiply" ~ .data$parent_1_srp_value * .data$parent_2_srp_value
      ),
      unit_out = if_else(
        is.na(.data$unit_out),
        .data$srp_unit_out,
        .data$unit_out
      )
    )

  value_out <- suppressWarnings(suppressMessages(unicon_full(
    value_in = workings$srp_value_out,
    unit_in = workings$srp_unit_out,
    unit_out = workings$unit_out,
    pull = TRUE
  )))

  final <- workings |>
    mutate(value_out = value_out) |>
    arrange(.data$row_id) |>
    select(
      parent_1_unit_in = .data$parent_1_unit_in,
      parent_1_value_in = .data$parent_1_value_in,
      parent_1_category = .data$parent_1_category,
      parent_1_srp_unit = .data$parent_1_srp,
      parent_1_srp_value = .data$parent_1_srp_value,
      parent_2_unit_in = .data$parent_2_unit_in,
      parent_2_value_in = .data$parent_2_value_in,
      parent_2_category = .data$parent_2_category,
      parent_2_srp_unit = .data$parent_2_srp,
      parent_2_srp_value = .data$parent_2_srp_value,
      operator = .data$operator,
      category_out = .data$category,
      srp_unit_out = .data$srp_unit_out,
      srp_value_out = .data$srp_value_out,
      unit_out = .data$unit_out,
      value_out = .data$value_out
    )

  if (isTRUE(pull)) {
    final$value_out
  } else {
    final
  }
}

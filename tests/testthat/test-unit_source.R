test_that("All base unit schemas have expected attributes", {
  # generate checks
  fields <- c(
    "category",
    "srp",
    "model",
    "alias"
  )

  checks <- purrr::imap(base, ~ list(
    full = all(fields %in% names(.x)),
    clean = all(names(.x) %in% fields),
    id = .y
  ))

  # summarise
  missing <- purrr::map_chr(
    purrr::discard(
      checks,
      "full"
    ),
    "id"
  )

  messy <- purrr::map_chr(
    purrr::discard(
      checks,
      "clean"
    ),
    "id"
  )

  # expect nothing missing
  expect_true(
    length(missing) == 0L,
    info = paste0(
      "Expected entries not present in unit schemas for
                  [", stringr::str_c(missing, collapse = "; "), "]"
    )
  )

  # expect nothing additional
  expect_true(
    length(messy) == 0L,
    info = paste0(
      "Unexpected fields in unit schema for
                  [", stringr::str_c(messy, collapse = "; "), "]"
    )
  )
})

################################################################################

test_that("All srp units are present in base units for aliases and models", {
  srp_covered <- srp %in% names(base)

  missing <- stringr::str_c(srp[!srp_covered],
    collapse = ", "
  )

  expect_true(all(srp_covered),
    info = paste0(
      "srp units missing for ",
      missing
    )
  )
})

################################################################################

test_that("srp models are as expected", {

  srp_models <- purrr::map(srp, ~ base[[.x]]$model)

  purrr::iwalk(srp_models, function(model, cat) {

    expect_true(model$slope == 1,
      info = paste0(
        "Model incorrect for ",
        cat,
        " SRP unit"
      )
    )

    expect_true(model$intercept == 0,
      info = paste0(
        "Model incorrect for ",
        cat,
        " SRP unit"
      )
    )
  })
})

################################################################################

test_that("SRP units are covered and expected", {

  srp_base <- purrr::map_chr(base, "srp")

  srp_covered <- srp_base %in% srp

  srp_missing <- stringr::str_c(unique(srp_base[!srp_covered]),
    collapse = "; "
  )

  expect_true(all(srp_covered),
    info = paste0(
      "The following SRP units aren't expected: ",
      srp_missing
    )
  )
})

################################################################################

test_that("No unit aliases are duplicated across IDs", {
  dupes <- unit_alias |>
    dplyr::group_by(alias) |>
    dplyr::summarise(
      n = dplyr::n(),
      ids = stringr::str_c(id,
        sep = ", "
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(n > 1L)

  msg_content <- stringr::str_c(
    paste0(
      dupes$alias,
      " (", dupes$ids, ")"
    ),
    collapse = "; "
  )

  expect_true(nrow(dupes) == 0L,
    info = paste0(
      "The following aliases are duplicated between IDs: ",
      msg_content
    )
  )
})

################################################################################

# explicitly removed from inputs so aliases guaranteed to mismatch if present

test_that("Unit aliases do not contain spaces or uppercase characters", {
  has_space <- unit_alias$alias[stringr::str_detect(
    unit_alias$alias,
    "\\s"
  )]

  has_uppercase <- unit_alias$alias[stringr::str_detect(
    unit_alias$alias,
    "[:upper:]"
  )]

  testthat::expect_true(
    length(has_space) == 0,
    info = paste0(
      "The following unit aliases contain whitespace :",
      stringr::str_c(has_space,
        sep = "; "
      )
    )
  )

  testthat::expect_true(
    length(has_uppercase) == 0,
    info = paste0(
      "The following unit aliases contain uppercase characters :",
      stringr::str_c(has_uppercase,
        sep = "; "
      )
    )
  )
})

################################################################################

test_that("Package data has expected null/NA structure", {
  # unit_srp should have no NAs
  expect_true(
    !anyNA(unit_srp),
    info = "unit_srp contains NA values"
  )

  # unit_alias should have no NAs
  expect_true(
    !anyNA(unit_alias),
    info = "unit_alias contains NA values"
  )

  # unit_models should have exactly 1 row of all NAs
  na_rows <- unit_models |>
    dplyr::mutate(
      all_na = is.na(id) &
        purrr::map_lgl(model, ~ is.na(.x$slope)) &
        purrr::map_lgl(model, ~ is.na(.x$intercept))
    ) |>
    dplyr::filter(all_na) |>
    nrow()

  expect_true(
    na_rows == 1L,
    info = paste0(
      "unit_models should have exactly 1 row of all NAs, found ",
      na_rows
    )
  )
})

################################################################################

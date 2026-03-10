test_that("All base unit schemas have expected attributes", {
  # generate checks
  fields <- c("category", "si", "model", "alias")
  checks <- imap(base, ~ list(
    full = all(fields %in% names(.x)),
    clean = all(names(.x) %in% fields),
    id = .y
  ))

  # summarise
  missing <- map_chr(discard(checks, "full"), "id")
  messy <- map_chr(discard(checks, "clean"), "id")

  # expect nothing missing
  expect_true(
    length(missing) == 0L,
    info = paste0("Expected entries not present in unit schemas for [", stringr::str_c(missing, collapse = "; "), "]")
  )

  # expect nothing additional
  expect_true(
    length(messy) == 0L,
    info = paste0("Unexpected fields in unit schema for [", stringr::str_c(messy, collapse = "; "), "]")
  )
})

####################################################################################################

test_that("All SI units are present in base units for aliases and models", {
  si_covered <- si %in% names(base)
  missing <- stringr::str_c(si[!si_covered], collapse = ", ")
  expect_true(all(si_covered), info = paste0("SI units missing for ", missing))
})

####################################################################################################

test_that("SI models are as expected", {
  si_models <- map(si, ~ base[[.x]]$model)
  iwalk(si_models, function(model, cat) {
    expect_true(model$slope == 1, info = paste0("Model incorrect for ", cat, " SI unit"))
    expect_true(model$intercept == 0, info = paste0("Model incorrect for ", cat, " SI unit"))
  })
})

####################################################################################################

test_that("SI units are covered and expected", {
  si_base <- map_chr(base, "si")
  si_covered <- si_base %in% si
  si_missing <- stringr::str_c(unique(si_base[!si_covered]), collapse = "; ")
  expect_true(all(si_covered), info = paste0("The following SI units aren't expected: ", si_missing))
})

####################################################################################################

test_that("No unit aliases are duplicated across IDs", {
  dupes <- unit_alias %>%
    group_by(alias) %>%
    summarise(n = n(), ids = stringr::str_c(id, sep = ", "), .groups = "drop") %>%
    filter(n > 1L)

  msg_content <- stringr::str_c(paste0(dupes$alias, " (", dupes$ids, ")"), collapse = "; ")
  expect_true(nrow(dupes) == 0L, info = paste0("The following aliases are duplicated between IDs: ", msg_content))
})

####################################################################################################

test_that("Unit aliases do not contain spaces or uppercase characters", { # explicitly removed from inputs so aliases guaranteed to mismatch if present

  has_space <- unit_alias$alias[stringr::str_detect(unit_alias$alias, "\\s")]
  has_uppercase <- unit_alias$alias[stringr::str_detect(unit_alias$alias, "[:upper:]")]

  testthat::expect_true(
    length(has_space) == 0,
    info = paste0("The following unit aliases contain whitespace :", stringr::str_c(has_space, sep = "; "))
  )

  testthat::expect_true(
    length(has_uppercase) == 0,
    info = paste0("The following unit aliases contain uppercase characters :", stringr::str_c(has_uppercase, sep = "; "))
  )
})

####################################################################################################

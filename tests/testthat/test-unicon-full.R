## Tests for unicon_full -------------------------------------------------------
## Covers argument validation, scalar/vector recycling, alias normalisation,
## pull = TRUE / pull = FALSE output, and snapshot regression via inst/input/
## examples.

# helper: read an inst/input/unicon_full example JSON and run unicon_full ------
run_full_example <- function(file, pull = TRUE) {
  path <- system.file(
    "input", "unicon_full", file,
    package = "unicon",
    lib.loc = .libPaths()
  )
  ex <- jsonlite::read_json(path, simplifyVector = TRUE)
  unit_out_val <- if (is.null(ex$unit_out)) NA else ex$unit_out
  pull_val     <- if (is.null(ex$pull)) pull else isTRUE(ex$pull)

  unicon_full(
    value_in = ex$value_in,
    unit_in  = ex$unit_in,
    unit_out = unit_out_val,
    pull     = pull_val
  )
}

## ---- argument validation ----------------------------------------------------

test_that("unicon_full rejects non-numeric value_in", {
  expect_error(
    unicon_full("1", "m", "cm"),
    "Argument `value_in` must be numeric\\."
  )
})

test_that("unicon_full rejects non-character unit_in", {
  expect_error(
    unicon_full(1, 2, "cm"),
    "Argument `unit_in` must be a character vector\\."
  )
})

test_that("unicon_full rejects wrong-length unit_in", {
  expect_error(
    unicon_full(1:2, c("m", "cm", "km"), "cm"),
    "Argument `unit_in` must have length 1 or length\\(value_in\\)\\."
  )
})

test_that("unicon_full rejects non-character unit_out", {
  expect_error(
    unicon_full(1, "m", TRUE),
    "Argument `unit_out` must be a character vector or `NA`\\."
  )
})

test_that("unicon_full rejects wrong-length unit_out", {
  expect_error(
    unicon_full(1:3, "m", c("cm", "mm")),
    "Argument `unit_out` must have length 1 or length\\(value_in\\)\\."
  )
})

test_that("unicon_full rejects zero-length value_in", {
  expect_error(
    unicon_full(numeric(), "m", "cm"),
    "Argument `value_in` must have length >= 1\\."
  )
})

## ---- basic conversions ------------------------------------------------------

test_that("unicon_full scalar recycling works correctly", {
  expect_equal(unicon_full(c(1, 2), "m", "cm"), c(100, 200))
})

test_that("unicon_full normalises whitespace and case in aliases", {
  expect_equal(
    unicon_full(
      c(1, 2),
      c(" Metres ", " CENTImEtres "),
      c(" CEntiMetres ", "  M  ")
    ),
    c(100, 0.02)
  )
})

test_that("unicon_full returns full table with pull = FALSE", {
  out <- unicon_full(c(1, 2), "m", "cm", pull = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_named(out, c(
    "unit_in", "unit_out", "alias_in", "alias_out", "id_in", "srp_in",
    "id_out", "error_in", "error_srp", "error_out", "value_in",
    "value_srp", "value_out"
  ))
})

## ---- error / NA propagation -------------------------------------------------

test_that("unicon_full warns and returns NA for unknown unit", {
  expect_warning(
    out <- unicon_full(1, "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\."
  )
  expect_true(out$error_in)
  expect_true(is.na(out$value_out))
})

test_that("unicon_full warns and returns NA for mismatched unit types", {
  expect_warning(
    out <- unicon_full(1, "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\."
  )
  expect_true(out$error_srp)
  expect_true(is.na(out$value_out))
})

test_that("unicon_full falls back to SRP when unit_out is entirely missing", {
  msgs <- capture.output(
    out <- unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE),
    type = "message"
  )
  expect_match(
    paste(msgs, collapse = "\n"),
    "No output unit given. Converting all values to standard reference unit."
  )
  expect_equal(out$id_out, c("m", "g"))
  expect_equal(out$value_out, c(1, 1000))
})

test_that("unicon_full falls back to SRP for partially missing unit_out", {
  msgs <- capture.output(
    out <- unicon_full(c(100, 1), c("cm", "kg"), c(NA, "g"), pull = FALSE),
    type = "message"
  )
  expect_match(
    paste(msgs, collapse = "\n"),
    "Output unit missing in some cases. Converting to standard reference unit where missing."
  )
  expect_equal(out$id_out, c("m", "g"))
  expect_equal(out$value_out, c(1, 1000))
})

## ---- inst/input snapshot tests ----------------------------------------------

test_that("unicon_full length_conversion snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_snapshot(run_full_example("length_conversion.json"))
})

test_that("unicon_full temperature_conversion snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_snapshot(run_full_example("temperature_conversion.json"))
})

test_that("unicon_full mass_conversion snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_snapshot(run_full_example("mass_conversion.json"))
})

test_that("unicon_full mixed_units_full_table snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_snapshot(run_full_example("mixed_units_full_table.json"))
})

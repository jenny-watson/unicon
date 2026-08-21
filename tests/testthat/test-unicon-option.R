## Tests for unicon_option -------------------------------------------------------
## Covers argument validation, same-category conversion (via unicon_full),
## cross-category conversion (via unicon_advance), vector inputs, and
## unknown/invalid category names in extras.

## ---- argument validation ----------------------------------------------------

test_that("unicon_option rejects unit_out with length > 1", {
  expect_error(
    unicon_option(
      value_in = 1,
      unit_in = "m",
      unit_out = c("cm", "km"),
      extras = list()
    ),
    "Argument `unit_out` must have length 1\\."
  )
})

test_that("unicon_option stops on unrecognised category name in extras", {
  expect_error(
    unicon_option(
      value_in = 1,
      unit_in = "m",
      unit_out = "cm",
      extras = list(not_a_category = list(value = 10, unit = "s"))
    ),
    "Please use category names used in unicon"
  )
})

## ---- same-category conversion (delegates to unicon_full) --------------------

test_that("unicon_option converts same-category units (length)", {
  out <- unicon_option(
    value_in = 1,
    unit_in = "m",
    unit_out = "cm",
    extras = list()
  )

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_equal(out, 100, tolerance = 1e-8)
})

test_that("unicon_option converts same-category units (mass)", {
  out <- unicon_option(
    value_in = 1,
    unit_in = "kg",
    unit_out = "g",
    extras = list()
  )

  expect_equal(out, 1000, tolerance = 1e-8)
})

test_that("unicon_option scalar recycling works across a vector of values", {
  out <- unicon_option(
    value_in = c(1, 2, 3),
    unit_in = "m",
    unit_out = "cm",
    extras = list()
  )

  expect_equal(out, c(100, 200, 300), tolerance = 1e-8)
})

test_that("unicon_option handles element-wise different unit_in values", {
  out <- unicon_option(
    value_in = c(1, 100),
    unit_in = c("m", "cm"),
    unit_out = "m",
    extras = list()
  )

  expect_equal(out, c(1, 1), tolerance = 1e-8)
})

## ---- cross-category conversion (delegates to unicon_advance) ----------------

test_that("unicon_option converts cross-category: speed from distance + time", {
  out <- unicon_option(
    value_in = 100,
    unit_in = "m",
    unit_out = "m/sec",
    extras = list(time = list(value = 10, unit = "sec"))
  )

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_equal(out, 10, tolerance = 1e-8)
})

test_that("unicon_option cross-category result matches unicon_advance directly", {
  opt_out <- unicon_option(
    value_in = 100,
    unit_in = "miles",
    unit_out = "km/hour",
    extras = list(time = list(value = 2, unit = "hour"))
  )

  adv_out <- unicon_advance(
    x_value_in = 100,
    x_unit_in = "miles",
    y_value_in = 2,
    y_unit_in = "hour",
    unit_out = "km/hour"
  )

  expect_equal(opt_out, adv_out, tolerance = 1e-8)
})

## ---- return type ------------------------------------------------------------

test_that("unicon_option returns a numeric vector", {
  out <- unicon_option(
    value_in = c(1, 2),
    unit_in = "km",
    unit_out = "m",
    extras = list()
  )

  expect_type(out, "double")
  expect_length(out, 2L)
})

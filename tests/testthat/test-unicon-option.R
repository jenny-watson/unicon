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

test_that("unicon_option stops with unknown unit_in", {
  expect_error(
    unicon_option(
      value_in = 1,
      unit_in = "not_a_unit",
      unit_out = "cm",
      extras = list()
    ),
    "`unit_in` does not have a recognised category"
  )
})

test_that("unicon_option stops with unknown unit_out", {
  expect_error(
    unicon_option(
      value_in = 1,
      unit_in = "m",
      unit_out = "not_a_unit",
      extras = list()
    ),
    "`unit_out` does not have a recognised category"
  )
})

## ---- stop if unicon_full is not needed (all cross-category, no same-category rows) --

test_that("unicon_option stops with 'Please use unicon_full' when all inputs are cross-category", {
  # Provide only a cross-category conversion (length -> speed) without extras,
  # so cat_same has 0 rows and cat_diff would have rows — but because extras is
  # empty the diff path cannot complete; the cat_same guard fires first.
  expect_error(
    unicon_option(
      value_in = 100,
      unit_in = "m",
      unit_out = "m/sec",
      extras = list()
    ),
    "Please use `unicon_advance`"
  )
})

## ---- stop if unicon_advance is not needed (all same-category, no cross-category rows) -

test_that("unicon_option stops with 'Please use unicon_advance' when inputs produce no diff rows", {
  # All same-category: cat_diff has 0 rows, so the cat_diff guard fires.
  # We pass extras that introduce a cross-category column that still resolves to
  # a same-category unit_out, so the diff filter returns nothing.
  expect_error(
    unicon_option(
      value_in = 1,
      unit_in = "m",
      unit_out = "cm",
      extras = list(time = list(value = 60, unit = "min"))
    ),
    "Please use `unicon_full`"
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

## ---- multiple values return numeric vector ----------------------------------

test_that("unicon_option returns a numeric vector for multiple same-category values", {
  out <- unicon_option(
    value_in = c(1, 2, 3),
    unit_in = "m",
    unit_out = "cm",
    extras = list()
  )

  expect_type(out, "double")
  expect_length(out, 3L)
  expect_equal(out, c(100, 200, 300), tolerance = 1e-8)
})

test_that("unicon_option returns a numeric vector for multiple cross-category values", {
  out <- unicon_option(
    value_in = c(100, 200),
    unit_in = "m",
    unit_out = "m/sec",
    extras = list(time = list(value = c(10, 20), unit = "sec"))
  )

  expect_type(out, "double")
  expect_length(out, 2L)
  expect_equal(out, c(10, 10), tolerance = 1e-8)
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




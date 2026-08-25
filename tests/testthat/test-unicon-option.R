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




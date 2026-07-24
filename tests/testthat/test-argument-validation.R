test_that("unicon_full validates argument types and lengths", {
  expect_error(
    unicon_full("1", "m", "cm"),
    "Argument `value_in` must be numeric\\."
  )
  expect_error(
    unicon_full(1, 2, "cm"),
    "Argument `unit_in` must be a character vector\\."
  )
  expect_error(
    unicon_full(1:2, c("m", "cm", "km"), "cm"),
    "Argument `unit_in` must have length 1 or length\\(value_in\\)\\."
  )
})

test_that("unicon_full preserves scalar recycling", {
  expect_equal(unicon_full(c(1, 2), "m", "cm"), c(100, 200))
})

test_that("unicon_lite validates argument types and lengths", {
  expect_error(
    unicon_lite("1", "m", "cm"),
    "Argument `value_in` must be numeric\\."
  )
  expect_error(
    unicon_lite(1, 2, "cm"),
    "Argument `id_in` must be a character vector\\."
  )
  expect_error(
    unicon_lite(1, "m", 2),
    "Argument `id_out` must be a character vector or `NA`\\."
  )
  expect_error(
    unicon_lite(1:2, c("m", "cm", "km"), "cm"),
    "Argument `id_in` must have length 1 or length\\(value_in\\)\\."
  )
})

test_that("unicon_lite preserves scalar recycling", {
  expect_equal(unicon_lite(c(1, 2), "m", "cm"), c(100, 200))
})

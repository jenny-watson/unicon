## Tests for unicon_lite -------------------------------------------------------
## Covers conversion-table output, error flags, and NA propagation.

## ---- basic output structure -------------------------------------------------

test_that("unicon_lite returns the correct column set", {
  out <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_named(out, c(
    "id_in", "id_out", "srp_in", "error_in", "error_srp", "error_out",
    "value_in", "value_srp", "value_out"
  ))
})

test_that("unicon_lite correct values for simple mass conversion", {
  out <- unicon_lite(c(1, 2.5), "kg", "g")
  expect_equal(out$value_out, c(1000, 2500))
})

## ---- SRP fallback -----------------------------------------------------------

test_that("unicon_lite falls back to SRP when id_out is missing", {
  out <- unicon_lite(c(100, 1), c("cm", "kg"))
  expect_equal(out$id_out, c("m", "g"))
  expect_equal(out$value_out, c(1, 1000))
  expect_false(any(out$error_out))
})

test_that("unicon_lite falls back to SRP for partially missing id_out", {
  out <- unicon_lite(c(100, 1), c("cm", "kg"), c(NA, "g"))
  expect_equal(out$id_out, c("m", "g"))
  expect_equal(out$value_out, c(1, 1000))
})

## ---- error / NA propagation -------------------------------------------------

test_that("unicon_lite sets error_in and NA value_out for unknown id_in", {
  out <- unicon_lite(1, "not-a-unit", "cm")
  expect_true(out$error_in)
  expect_true(is.na(out$value_out))
})

test_that("unicon_lite sets error_srp and NA value_out for mismatched unit types", {
  out <- unicon_lite(1, "m", "g")
  expect_true(out$error_srp)
  expect_true(is.na(out$value_out))
})

## ---- alignment with unicon_full ---------------------------------------------

test_that("unicon_lite and unicon_full produce identical numeric outputs when IDs provided directly", {
  full_out <- unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  lite_out <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))

  expect_equal(full_out$value_in, lite_out$value_in)
  expect_equal(full_out$value_srp, lite_out$value_srp)
  expect_equal(full_out$value_out, lite_out$value_out)
  expect_equal(full_out$error_in, lite_out$error_in)
  expect_equal(full_out$error_srp, lite_out$error_srp)
  expect_equal(full_out$error_out, lite_out$error_out)
})

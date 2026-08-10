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
  expect_error(
    unicon_full(1, "m", TRUE),
    "Argument `unit_out` must be a character vector or `NA`\\."
  )
  expect_error(
    unicon_full(1:3, "m", c("cm", "mm")),
    "Argument `unit_out` must have length 1 or length\\(value_in\\)\\."
  )
  expect_error(
    unicon_full(numeric(), "m", "cm"),
    "Argument `value_in` must have length >= 1\\."
  )
})

test_that("unicon_full preserves scalar recycling", {
  expect_equal(unicon_full(c(1, 2), "m", "cm"), c(100, 200))
})

test_that("unicon_full normalises messy aliases", {
  expect_equal(
    unicon_full(
      c(1, 2),
      c(" Metres ", " CENTImEtres "),
      c(" CEntiMetres ", "  M  ")
    ),
    c(100, 0.02)
  )
})

test_that("unicon_full returns the full conversion table with pull = FALSE", {
  out <- unicon_full(c(1, 2), "m", "cm", pull = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_named(out, c(
    "unit_in", "unit_out", "alias_in", "alias_out", "id_in", "srp_in",
    "id_out", "error_in", "error_srp", "error_out", "value_in",
    "value_srp", "value_out"
  ))
})

test_that("unicon_lite returns the conversion table columns", {
  out <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_named(out, c(
    "id_in", "id_out", "srp_in", "error_in", "error_srp", "error_out",
    "value_in", "value_srp", "value_out"
  ))
})

test_that("missing output targets fall back to the SRP where needed", {
  full_msgs <- capture.output(
    full_tbl <- unicon_full(c(100, 1), c("cm", "kg"), c(NA, "g"), pull = FALSE),
    type = "message"
  )

  expect_match(
    paste(full_msgs, collapse = "\n"),
    "Output unit missing in some cases. Converting to standard reference unit where missing."
  )

  expect_equal(full_tbl$id_out, c("m", "g"))

  expect_equal(full_tbl$value_out, c(1, 1000))

  expect_false(any(full_tbl$error_out))

  lite_tbl <- unicon_lite(c(100, 1), c("cm", "kg"), c(NA, "g"))
  expect_equal(lite_tbl$id_out, c("m", "g"))
  expect_equal(lite_tbl$value_out, c(1, 1000))
  expect_false(any(lite_tbl$error_out))
})

test_that("missing unit_out entirely falls back to SRP for unicon_full and unicon_lite", {
  full_msgs <- capture.output(
    full_tbl <- unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE),
    type = "message"
  )

  expect_match(
    paste(full_msgs, collapse = "\n"),
    "No output unit given. Converting all values to standard reference unit."
  )
  expect_equal(full_tbl$id_out, c("m", "g"))
  expect_equal(full_tbl$value_out, c(1, 1000))

  lite_tbl <- unicon_lite(c(100, 1), c("cm", "kg"))
  expect_equal(lite_tbl$id_out, c("m", "g"))
  expect_equal(lite_tbl$value_out, c(1, 1000))
})

test_that("incompatible unit types warn and return NA output in unicon_full", {
  expect_warning(
    full_tbl <- unicon_full(1, "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\."
  )

  expect_true(full_tbl$error_srp)
  expect_true(is.na(full_tbl$value_out))

  lite_tbl <- unicon_lite(1, "m", "g")
  expect_true(lite_tbl$error_srp)
  expect_true(is.na(lite_tbl$value_out))
})

test_that("unknown units warn and return NA instead of erroring", {
  expect_warning(
    full_tbl <- unicon_full(1, "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\."
  )

  expect_true(full_tbl$error_in)
  expect_true(is.na(full_tbl$value_out))

  lite_tbl <- unicon_lite(1, "not-a-unit", "cm")
  expect_true(lite_tbl$error_in)
  expect_true(is.na(lite_tbl$value_out))
})

test_that("unicon_full and unicon_lite align when IDs are provided directly", {
  full_tbl <- unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  lite_tbl <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))

  expect_equal(full_tbl$id_in, lite_tbl$id_in)
  expect_equal(full_tbl$id_out, lite_tbl$id_out)
  expect_equal(full_tbl$srp_in, lite_tbl$srp_in)
  expect_equal(full_tbl$error_in, lite_tbl$error_in)
  expect_equal(full_tbl$error_srp, lite_tbl$error_srp)
  expect_equal(full_tbl$error_out, lite_tbl$error_out)
  expect_equal(full_tbl$value_in, lite_tbl$value_in)
  expect_equal(full_tbl$value_srp, lite_tbl$value_srp)
  expect_equal(full_tbl$value_out, lite_tbl$value_out)
})

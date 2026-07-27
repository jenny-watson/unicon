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
    "unit_in", "unit_out", "alias_in", "alias_out", "id_in", "id_srp",
    "id_out", "error_in", "error_srp", "error_out", "value_in",
    "value_srp", "value_out"
  ))
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
  expect_error(
    unicon_lite(1:3, "m", c("cm", "mm")),
    "Argument `id_out` must have length 1 or length\\(value_in\\)\\."
  )
  expect_error(
    unicon_lite(numeric(), "m", "cm"),
    "Argument `value_in` must have length >= 1\\."
  )
})

test_that("unicon_lite preserves scalar recycling", {
  expect_equal(unicon_lite(c(1, 2), "m", "cm"), c(100, 200))
})

test_that("unicon_lite returns the full conversion table with pull = FALSE", {
  out <- unicon_lite(c(1, 2), "m", "cm", pull = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_named(out, c(
    "id_in", "id_out", "id_srp", "error_in", "error_srp", "error_out",
    "value_in", "value_srp", "value_out"
  ))
})

test_that("missing output targets fall back to the SRP where needed", {
  full_msgs <- capture.output(
    full_tbl <- unicon_full(c(100, 1), c("cm", "kg"), c(NA, "g"), pull = FALSE),
    type = "message"
  )
  lite_msgs <- capture.output(
    lite_tbl <- unicon_lite(c(100, 1), c("cm", "kg"), c(NA, "g"), pull = FALSE),
    type = "message"
  )

  expect_identical(full_msgs, "Output unit missing in some cases. Converting to standard reference unit where missing.")
  expect_identical(lite_msgs, "Output ID missing in some cases. Converting to standard reference unit where missing.")

  expect_equal(full_tbl$id_out, c("m", "g"))
  expect_equal(full_tbl$value_out, c(1, 1000))
  expect_false(any(full_tbl$error_out))

  expect_equal(lite_tbl$id_out, c("m", "g"))
  expect_equal(lite_tbl$value_out, c(1, 1000))
  expect_false(any(lite_tbl$error_out))
})

test_that("incompatible unit types warn and return NA output", {
  expect_warning(
    full_tbl <- unicon_full(1, "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\."
  )
  expect_warning(
    lite_tbl <- unicon_lite(1, "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\."
  )

  expect_true(full_tbl$error_srp)
  expect_true(is.na(full_tbl$value_out))

  expect_true(lite_tbl$error_srp)
  expect_true(is.na(lite_tbl$value_out))
})

test_that("unknown units warn and return NA instead of erroring", {
  expect_warning(
    full_tbl <- unicon_full(1, "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\."
  )
  expect_warning(
    lite_tbl <- unicon_lite(1, "not-a-unit", "cm", pull = FALSE),
    "Some input unit IDs were invalid\\."
  )

  expect_true(full_tbl$error_in)
  expect_true(is.na(full_tbl$value_out))

  expect_true(lite_tbl$error_in)
  expect_true(is.na(lite_tbl$value_out))
})

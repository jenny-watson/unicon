test_that("unicon_full validates argument types and lengths", {
  expect_error(
    unicon_full("1", "m", "cm"),
    "Argument `value_in` must be numeric\\.",
    info = "value_in_type=character, unit_in=m, unit_out=cm"
  )
  expect_error(
    unicon_full(1, 2, "cm"),
    "Argument `unit_in` must be a character vector\\.",
    info = "value_in=1, unit_in_type=double, unit_out=cm"
  )
  expect_error(
    unicon_full(1:2, c("m", "cm", "km"), "cm"),
    "Argument `unit_in` must have length 1 or length\\(value_in\\)\\.",
    info = "value_in_length=2, unit_in_length=3, unit_out=cm"
  )
  expect_error(
    unicon_full(1, "m", TRUE),
    "Argument `unit_out` must be a character vector or `NA`\\.",
    info = "value_in=1, unit_in=m, unit_out_type=logical"
  )
  expect_error(
    unicon_full(1:3, "m", c("cm", "mm")),
    "Argument `unit_out` must have length 1 or length\\(value_in\\)\\.",
    info = "value_in_length=3, unit_in=m, unit_out_length=2"
  )
  expect_error(
    unicon_full(numeric(), "m", "cm"),
    "Argument `value_in` must have length >= 1\\.",
    info = "value_in_length=0, unit_in=m, unit_out=cm"
  )
})

test_that("unicon_full preserves scalar recycling", {
  expect_equal(
    unicon_full(c(1, 2), "m", "cm"),
    c(100, 200),
    info = "value_in=1,2, unit_in=m, unit_out=cm"
  )
})

test_that("unicon_full normalises messy aliases", {
  expect_equal(
    unicon_full(
      c(1, 2),
      c(" Metres ", " CENTImEtres "),
      c(" CEntiMetres ", "  M  ")
    ),
    c(100, 0.02),
    info = "unit_in=Metres|CENTImEtres, unit_out=CEntiMetres|M"
  )
})

test_that("unicon_full returns the full conversion table with pull = FALSE", {
  out <- unicon_full(c(1, 2), "m", "cm", pull = FALSE)

  expect_s3_class(out, "data.frame", info = "value_in=1,2, unit_in=m, unit_out=cm")
  expect_equal(nrow(out), 2L, info = "value_in_length=2, unit_in=m, unit_out=cm")
  expect_named(out, c(
    "unit_in", "unit_out", "alias_in", "alias_out", "id_in", "srp_in",
    "id_out", "error_in", "error_srp", "error_out", "value_in",
    "value_srp", "value_out"
  ), info = "dataset=unicon_full_pull_false_columns")
})

test_that("unicon_lite returns the conversion table columns", {
  out <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))

  expect_s3_class(out, "data.frame", info = "id_in=m|kg, id_out=cm|g")
  expect_equal(nrow(out), 2L, info = "value_in_length=2, id_in=m|kg, id_out=cm|g")
  expect_named(out, c(
    "id_in", "id_out", "srp_in", "error_in", "error_srp", "error_out",
    "value_in", "value_srp", "value_out"
  ), info = "dataset=unicon_lite_columns")
})

test_that("missing output targets fall back to the SRP where needed", {
  full_msgs <- capture.output(
    full_tbl <- unicon_full(c(100, 1), c("cm", "kg"), c(NA, "g"), pull = FALSE),
    type = "message"
  )

  expect_match(
    paste(full_msgs, collapse = "\n"),
    "Output unit missing in some cases. Converting to standard reference unit where missing.",
    info = "value_in=100|1, unit_in=cm|kg, unit_out=NA|g"
  )

  expect_equal(full_tbl$id_out, c("m", "g"), info = "dataset=full_tbl, id_out_expected=m|g")

  expect_equal(full_tbl$value_out, c(1, 1000), info = "dataset=full_tbl, value_out_expected=1|1000")

  expect_false(any(full_tbl$error_out), info = "dataset=full_tbl, error_out_expected=FALSE")

  lite_tbl <- unicon_lite(c(100, 1), c("cm", "kg"), c(NA, "g"))
  expect_equal(lite_tbl$id_out, c("m", "g"), info = "dataset=lite_tbl, id_out_expected=m|g")
  expect_equal(lite_tbl$value_out, c(1, 1000), info = "dataset=lite_tbl, value_out_expected=1|1000")
  expect_false(any(lite_tbl$error_out), info = "dataset=lite_tbl, error_out_expected=FALSE")
})

test_that("missing unit_out entirely falls back to SRP for unicon_full and unicon_lite", {
  full_msgs <- capture.output(
    full_tbl <- unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE),
    type = "message"
  )

  expect_match(
    paste(full_msgs, collapse = "\n"),
    "No output unit given. Converting all values to standard reference unit.",
    info = "value_in=100|1, unit_in=cm|kg, unit_out=missing"
  )
  expect_equal(full_tbl$id_out, c("m", "g"), info = "dataset=full_tbl, id_out_expected=m|g")
  expect_equal(full_tbl$value_out, c(1, 1000), info = "dataset=full_tbl, value_out_expected=1|1000")

  lite_tbl <- unicon_lite(c(100, 1), c("cm", "kg"))
  expect_equal(lite_tbl$id_out, c("m", "g"), info = "dataset=lite_tbl, id_out_expected=m|g")
  expect_equal(lite_tbl$value_out, c(1, 1000), info = "dataset=lite_tbl, value_out_expected=1|1000")
})

test_that("incompatible unit types warn and return NA output in unicon_full", {
  expect_warning(
    full_tbl <- unicon_full(1, "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\.",
    info = "value_in=1, unit_in=m, unit_out=g"
  )

  expect_true(full_tbl$error_srp, info = "dataset=full_tbl, error_srp_expected=TRUE")
  expect_true(is.na(full_tbl$value_out), info = "dataset=full_tbl, value_out_expected=NA")

  lite_tbl <- unicon_lite(1, "m", "g")
  expect_true(lite_tbl$error_srp, info = "dataset=lite_tbl, error_srp_expected=TRUE")
  expect_true(is.na(lite_tbl$value_out), info = "dataset=lite_tbl, value_out_expected=NA")
})

test_that("unknown units warn and return NA instead of erroring", {
  expect_warning(
    full_tbl <- unicon_full(1, "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\.",
    info = "value_in=1, unit_in=not-a-unit, unit_out=cm"
  )

  expect_true(full_tbl$error_in, info = "dataset=full_tbl, error_in_expected=TRUE")
  expect_true(is.na(full_tbl$value_out), info = "dataset=full_tbl, value_out_expected=NA")

  lite_tbl <- unicon_lite(1, "not-a-unit", "cm")
  expect_true(lite_tbl$error_in, info = "dataset=lite_tbl, error_in_expected=TRUE")
  expect_true(is.na(lite_tbl$value_out), info = "dataset=lite_tbl, value_out_expected=NA")
})

test_that("unicon_full and unicon_lite align when IDs are provided directly", {
  full_tbl <- unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  lite_tbl <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))

  expect_equal(full_tbl$id_in, lite_tbl$id_in, info = "dataset_compare=id_in, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$id_out, lite_tbl$id_out, info = "dataset_compare=id_out, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$srp_in, lite_tbl$srp_in, info = "dataset_compare=srp_in, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$error_in, lite_tbl$error_in, info = "dataset_compare=error_in, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$error_srp, lite_tbl$error_srp, info = "dataset_compare=error_srp, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$error_out, lite_tbl$error_out, info = "dataset_compare=error_out, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$value_in, lite_tbl$value_in, info = "dataset_compare=value_in, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$value_srp, lite_tbl$value_srp, info = "dataset_compare=value_srp, id_in=m|kg, id_out=cm|g")
  expect_equal(full_tbl$value_out, lite_tbl$value_out, info = "dataset_compare=value_out, id_in=m|kg, id_out=cm|g")
})

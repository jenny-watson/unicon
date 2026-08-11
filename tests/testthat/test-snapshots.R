test_that("unicon_full pull = TRUE snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_km_mi <- unicon_full(1, "kilometres", "mi", pull = TRUE)
  out_c_f <- unicon_full(c(0, 100, -40), "celsius", "fahrenheit", pull = TRUE)
  out_kg_g <- unicon_full(c(1, 2.5), "kg", "g", pull = TRUE)

  expect_type(out_km_mi, "double", info = "snapshot=unicon_full_pull_true, value_in=1, unit_in=kilometres, unit_out=mi")
  expect_length(out_km_mi, 1L, info = "snapshot=unicon_full_pull_true, value_in=1, unit_in=kilometres, unit_out=mi")
  expect_length(out_c_f, 3L, info = "snapshot=unicon_full_pull_true, value_in=0|100|-40, unit_in=celsius, unit_out=fahrenheit")
  expect_length(out_kg_g, 2L, info = "snapshot=unicon_full_pull_true, value_in=1|2.5, unit_in=kg, unit_out=g")

  expect_snapshot({
    unicon_full(1, "kilometres", "mi", pull = TRUE)
    unicon_full(c(0, 100, -40), "celsius", "fahrenheit", pull = TRUE)
    unicon_full(c(1, 2.5), "kg", "g", pull = TRUE)
  })
})

test_that("unicon_full pull = FALSE snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)

  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_full_pull_false, unit_in=m|kg, unit_out=cm|g")
  expect_equal(nrow(out_tbl), 2L, info = "snapshot=unicon_full_pull_false, value_in=1|2, unit_in=m|kg, unit_out=cm|g")

  expect_snapshot(
    unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  )
})

test_that("unicon_full missing unit_out snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE)
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_full_missing_unit_out, unit_in=cm|kg, unit_out=missing")
  expect_equal(nrow(out_tbl), 2L, info = "snapshot=unicon_full_missing_unit_out, value_in=100|1, unit_in=cm|kg")

  expect_snapshot(
    unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE)
  )
})

test_that("unicon_full unrecognised unit snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- suppressMessages(unicon_full(1, "not_a_unit", "km", pull = FALSE))
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_full_unrecognised, unit_in=not_a_unit, unit_out=km")
  expect_true(all(out_tbl$error_in), info = "snapshot=unicon_full_unrecognised, unit_in=not_a_unit, unit_out=km")

  expect_snapshot(
    suppressMessages(unicon_full(1, "not_a_unit", "km", pull = FALSE))
  )
})

test_that("unicon_full mismatched unit types snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- suppressMessages(unicon_full(1, "m", "g", pull = FALSE))
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_full_mismatched_types, unit_in=m, unit_out=g")
  expect_true(all(out_tbl$error_srp), info = "snapshot=unicon_full_mismatched_types, unit_in=m, unit_out=g")

  expect_snapshot(
    suppressMessages(unicon_full(1, "m", "g", pull = FALSE))
  )
})

test_that("unicon_lite conversion table snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_lite_conversion_table, id_in=m|kg, id_out=cm|g")
  expect_equal(nrow(out_tbl), 2L, info = "snapshot=unicon_lite_conversion_table, value_in=1|2, id_in=m|kg, id_out=cm|g")

  expect_snapshot(
    unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
  )
})

test_that("unicon_lite missing id_out snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- unicon_lite(c(100, 1), c("cm", "kg"))
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_lite_missing_id_out, id_in=cm|kg, id_out=missing")
  expect_equal(nrow(out_tbl), 2L, info = "snapshot=unicon_lite_missing_id_out, value_in=100|1, id_in=cm|kg")

  expect_snapshot(
    unicon_lite(c(100, 1), c("cm", "kg"))
  )
})

test_that("unicon_lite unrecognised id snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- unicon_lite(1, "not_a_unit", "km")
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_lite_unrecognised_id, id_in=not_a_unit, id_out=km")
  expect_true(all(out_tbl$error_in), info = "snapshot=unicon_lite_unrecognised_id, id_in=not_a_unit, id_out=km")

  expect_snapshot(
    unicon_lite(1, "not_a_unit", "km")
  )
})

test_that("unicon_lite mismatched unit types snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  out_tbl <- unicon_lite(1, "m", "g")
  expect_s3_class(out_tbl, "data.frame", info = "snapshot=unicon_lite_mismatched_types, id_in=m, id_out=g")
  expect_true(all(out_tbl$error_srp), info = "snapshot=unicon_lite_mismatched_types, id_in=m, id_out=g")

  expect_snapshot(
    unicon_lite(1, "m", "g")
  )
})

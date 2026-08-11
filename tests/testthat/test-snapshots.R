test_that("unicon_full pull = TRUE snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  case_count <- 3L
  unit_in <- c("kilometres", "celsius", "kg")
  unit_out <- c("mi", "fahrenheit", "g")

  expect_true(length(unit_in) == length(unit_out), info = "snapshot=unicon_full_pull_true, unit_in_count=3, unit_out_count=3")
  expect_true(all(nzchar(unit_in)), info = paste0("snapshot=unicon_full_pull_true, unit_in=", paste(unit_in, collapse = "|")))
  expect_true(all(nzchar(unit_out)), info = paste0("snapshot=unicon_full_pull_true, unit_out=", paste(unit_out, collapse = "|")))
  expect_equal(case_count, 3L, info = "snapshot=unicon_full_pull_true, case_count=3")

  expect_snapshot({
    unicon_full(1, "kilometres", "mi", pull = TRUE)
    unicon_full(c(0, 100, -40), "celsius", "fahrenheit", pull = TRUE)
    unicon_full(c(1, 2.5), "kg", "g", pull = TRUE)
  })
})

test_that("unicon_full pull = FALSE snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  value_in <- c(1, 2)
  unit_in <- c("m", "kg")
  unit_out <- c("cm", "g")

  expect_equal(length(value_in), 2L, info = "snapshot=unicon_full_pull_false, value_in_count=2")
  expect_equal(length(unit_in), 2L, info = "snapshot=unicon_full_pull_false, unit_in_count=2")
  expect_equal(length(unit_out), 2L, info = "snapshot=unicon_full_pull_false, unit_out_count=2")

  expect_snapshot(
    unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  )
})

test_that("unicon_full missing unit_out snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  value_in <- c(100, 1)
  unit_in <- c("cm", "kg")
  expect_equal(length(value_in), length(unit_in), info = "snapshot=unicon_full_missing_unit_out, value_in_count=2, unit_in_count=2")
  expect_true(all(nzchar(unit_in)), info = paste0("snapshot=unicon_full_missing_unit_out, unit_in=", paste(unit_in, collapse = "|")))

  expect_snapshot(
    unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE)
  )
})

test_that("unicon_full unrecognised unit snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_true(nzchar("not_a_unit"), info = "snapshot=unicon_full_unrecognised, unit_in=not_a_unit, unit_out=km")

  expect_snapshot(
    suppressMessages(unicon_full(1, "not_a_unit", "km", pull = FALSE))
  )
})

test_that("unicon_full mismatched unit types snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_gt(nchar("m"), 0L, info = "snapshot=unicon_full_mismatched_types, unit_in=m, unit_out=g")

  expect_snapshot(
    suppressMessages(unicon_full(1, "m", "g", pull = FALSE))
  )
})

test_that("unicon_lite conversion table snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  id_in <- c("m", "kg")
  id_out <- c("cm", "g")
  expect_equal(length(id_in), length(id_out), info = "snapshot=unicon_lite_conversion_table, id_in_count=2, id_out_count=2")
  expect_true(all(nzchar(id_in)), info = paste0("snapshot=unicon_lite_conversion_table, id_in=", paste(id_in, collapse = "|")))
  expect_true(all(nzchar(id_out)), info = paste0("snapshot=unicon_lite_conversion_table, id_out=", paste(id_out, collapse = "|")))

  expect_snapshot(
    unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
  )
})

test_that("unicon_lite missing id_out snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  id_in <- c("cm", "kg")
  expect_equal(length(id_in), 2L, info = "snapshot=unicon_lite_missing_id_out, id_in_count=2")
  expect_true(all(nzchar(id_in)), info = paste0("snapshot=unicon_lite_missing_id_out, id_in=", paste(id_in, collapse = "|")))

  expect_snapshot(
    unicon_lite(c(100, 1), c("cm", "kg"))
  )
})

test_that("unicon_lite unrecognised id snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_true(nzchar("not_a_unit"), info = "snapshot=unicon_lite_unrecognised_id, id_in=not_a_unit, id_out=km")

  expect_snapshot(
    unicon_lite(1, "not_a_unit", "km")
  )
})

test_that("unicon_lite mismatched unit types snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_gt(nchar("m"), 0L, info = "snapshot=unicon_lite_mismatched_types, id_in=m, id_out=g")

  expect_snapshot(
    unicon_lite(1, "m", "g")
  )
})

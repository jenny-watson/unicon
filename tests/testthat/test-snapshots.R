test_that("unicon_full pull = TRUE snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot({
    unicon_full(1, "kilometres", "mi", pull = TRUE)
    unicon_full(c(0, 100, -40), "celsius", "fahrenheit", pull = TRUE)
    unicon_full(c(1, 2.5), "kg", "g", pull = TRUE)
  })
})

test_that("unicon_full pull = FALSE snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  )
})

test_that("unicon_full missing unit_out snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE)
  )
})

test_that("unicon_full unrecognised unit snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    suppressMessages(unicon_full(1, "not_a_unit", "km", pull = FALSE))
  )
})

test_that("unicon_full mismatched unit types snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    suppressMessages(unicon_full(1, "m", "g", pull = FALSE))
  )
})

test_that("unicon_lite pull = TRUE snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot({
    unicon_lite(1, "km", "mile", pull = TRUE)
    unicon_lite(c(0, 100, -40), "C", "fahrenheit", pull = TRUE)
    unicon_lite(c(1, 2.5), "kg", "g", pull = TRUE)
  })
})

test_that("unicon_lite pull = FALSE snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  )
})

test_that("unicon_lite missing id_out snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    unicon_lite(c(100, 1), c("cm", "kg"), pull = FALSE)
  )
})

test_that("unicon_lite unrecognised id snapshots", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    suppressMessages(unicon_lite(1, "not_a_unit", "km", pull = FALSE))
  )
})

test_that("unicon_lite mismatched unit types snapshot", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  expect_snapshot(
    suppressMessages(unicon_lite(1, "m", "g", pull = FALSE))
  )
})

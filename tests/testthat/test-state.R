## state management -----------------------------------------------------------

## ---- default behaviour unchanged --------------------------------------------

test_that(".unicon_state$unit_alias contains package data by default", {
  unicon_reset_units()
  ua <- unicon:::.unicon_state$unit_alias
  expect_true(is.data.frame(ua))
  expect_true(all(c("id", "alias") %in% names(ua)))
  expect_gt(nrow(ua), 0L)
})

test_that(".unicon_state$unit_srp contains package data by default", {
  unicon_reset_units()
  us <- unicon:::.unicon_state$unit_srp
  expect_true(is.data.frame(us))
  expect_true(all(c("id", "srp") %in% names(us)))
  expect_gt(nrow(us), 0L)
})

test_that(".unicon_state$unit_models contains package data by default", {
  unicon_reset_units()
  um <- unicon:::.unicon_state$unit_models
  expect_true(is.data.frame(um))
  expect_true(all(c("id", "model") %in% names(um)))
  expect_gt(nrow(um), 0L)
})

test_that("unicon_own_status is FALSE by default", {
  unicon_reset_units()
  expect_false(unicon_own_status())
})

## ---- unicon_full uses active state ------------------------------------------

test_that("unicon_full default behaviour unchanged after reset", {
  unicon_reset_units()
  result <- unicon_full(c(1, 2), c("kg", "g"), "kg", pull = TRUE)
  expect_equal(result, c(1, 0.002))
})

test_that("unicon_full uses custom alias data when set", {
  on.exit(unicon_reset_units(), add = TRUE)

  ## Build full custom datasets the same way unicon_own does
  expect_warning(
    unicon_own(
      base_id    = c("lp", "lp"),
      base_alias = c("lp", "largepackage"),
      category   = c("mass", "mass"),
      srp        = c("g", "g"),
      slope      = c(12500, 12500),
      intercept  = c(0, 0)
    ),
    "Derived data is not just made of base units. Please check all units are present in base data."
  )

  result <- unicon_full(
    value_in = 1,
    unit_in  = "largepackage",
    unit_out = "g",
    pull     = TRUE
  )
  expect_equal(result, 12500)
})

test_that("unicon_full reverts to package data after reset", {
  on.exit(unicon_reset_units(), add = TRUE)

  expect_warning(
    unicon_own(
      base_id    = c("lp", "lp"),
      base_alias = c("lp", "largepackage"),
      category   = c("mass", "mass"),
      srp        = c("g", "g"),
      slope      = c(12500, 12500),
      intercept  = c(0, 0)
    ), "Derived data is not just made of base units. Please check all units are present in base data." # nolint
  )

  unicon_reset_units()

  expect_warning(
    result <- unicon_full(1, "largepackage", "g", pull = TRUE),
    "Some units failed to convert"
  )
  expect_true(all(is.na(result)))
})

## ---- unicon_help reflects active state --------------------------------------

test_that("unicon_help returns package data by default", {
  unicon_reset_units()
  h <- unicon_help()
  expect_true(is.data.frame(h))
  expect_true(all(c("id", "alias") %in% names(h)))
  expect_false("lp" %in% h$id)
})

test_that("unicon_help shows custom units after unicon_own", {
  on.exit(unicon_reset_units(), add = TRUE)

  expect_warning(
    unicon_own(
      base_id    = c("lp", "lp"),
      base_alias = c("lp", "largepackage"),
      category   = c("mass", "mass"),
      srp        = c("g", "g"),
      slope      = c(12500, 12500),
      intercept  = c(0, 0)
    ), "Derived data is not just made of base units. Please check all units are present in base data." # nolint
  )

  h <- unicon_help()
  expect_true("lp" %in% h$id)
  expect_true("largepackage" %in% h$alias)
})

test_that("unicon_help reverts to package data after reset", {
  on.exit(unicon_reset_units(), add = TRUE)

  expect_warning(
    unicon_own(
      base_id    = c("lp", "lp"),
      base_alias = c("lp", "largepackage"),
      category   = c("mass", "mass"),
      srp        = c("g", "g"),
      slope      = c(12500, 12500),
      intercept  = c(0, 0)
    ), "Derived data is not just made of base units. Please check all units are present in base data." # nolint
  )

  unicon_reset_units()

  h <- unicon_help()
  expect_false("lp" %in% h$id)
})

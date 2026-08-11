## state management -----------------------------------------------------------

## ---- default behaviour unchanged --------------------------------------------

test_that(".unicon_state$unit_alias contains package data by default", {
  unicon_reset_units()
  ua <- unicon:::.unicon_state$unit_alias
  expect_true(is.data.frame(ua), info = "dataset=unit_alias")
  expect_true(all(c("id", "alias") %in% names(ua)), info = "dataset=unit_alias, expected_cols=id|alias")
  expect_gt(nrow(ua), 0L, info = "dataset=unit_alias")
})

test_that(".unicon_state$unit_srp contains package data by default", {
  unicon_reset_units()
  us <- unicon:::.unicon_state$unit_srp
  expect_true(is.data.frame(us), info = "dataset=unit_srp")
  expect_true(all(c("id", "srp") %in% names(us)), info = "dataset=unit_srp, expected_cols=id|srp")
  expect_gt(nrow(us), 0L, info = "dataset=unit_srp")
})

test_that(".unicon_state$unit_models contains package data by default", {
  unicon_reset_units()
  um <- unicon:::.unicon_state$unit_models
  expect_true(is.data.frame(um), info = "dataset=unit_models")
  expect_true(all(c("id", "model") %in% names(um)), info = "dataset=unit_models, expected_cols=id|model")
  expect_gt(nrow(um), 0L, info = "dataset=unit_models")
})

test_that("unicon_own_status is FALSE by default", {
  unicon_reset_units()
  expect_false(unicon_own_status(), info = "state=package_defaults_expected")
})

## ---- unicon_full uses active state ------------------------------------------

test_that("unicon_full default behaviour unchanged after reset", {
  unicon_reset_units()
  result <- unicon_full(c(1, 2), c("kg", "g"), "kg", pull = TRUE)
  expect_equal(result, c(1, 0.001), info = "value_in=1|2, unit_in=kg|g, unit_out=kg")
})

test_that("unicon_full uses custom alias data when set", {
  on.exit(unicon_reset_units(), add = TRUE)

  ## Build full custom datasets the same way unicon_own does
  unicon_own(
    base_id    = c("lp", "lp"),
    base_alias = c("lp", "largepackage"),
    category   = c("mass", "mass"),
    srp        = c("g", "g"),
    slope      = c(12500, 12500),
    intercept  = c(0, 0)
  )

  result <- unicon_full(
    value_in = 1,
    unit_in  = "largepackage",
    unit_out = "g",
    pull     = TRUE
  )
  expect_equal(result, 12500, info = "unit_in=largepackage, unit_out=g, id=lp")
})

test_that("unicon_full reverts to package data after reset", {
  on.exit(unicon_reset_units(), add = TRUE)

  unicon_own(
    base_id    = c("lp", "lp"),
    base_alias = c("lp", "largepackage"),
    category   = c("mass", "mass"),
    srp        = c("g", "g"),
    slope      = c(12500, 12500),
    intercept  = c(0, 0)
  )
  unicon_reset_units()

  result <- unicon_full(1, "largepackage", "g", pull = TRUE)
  expect_true(is.na(result), info = "unit_in=largepackage, unit_out=g, expected_after_reset=NA")
})

## ---- unicon_help reflects active state --------------------------------------

test_that("unicon_help returns package data by default", {
  unicon_reset_units()
  h <- unicon_help()
  expect_true(is.data.frame(h), info = "dataset=unicon_help")
  expect_true(all(c("id", "alias") %in% names(h)), info = "dataset=unicon_help, expected_cols=id|alias")
  expect_false("lp" %in% h$id, info = "id=lp should not exist in package defaults")
})

test_that("unicon_help shows custom units after unicon_own", {
  on.exit(unicon_reset_units(), add = TRUE)

  unicon_own(
    base_id    = c("lp", "lp"),
    base_alias = c("lp", "largepackage"),
    category   = c("mass", "mass"),
    srp        = c("g", "g"),
    slope      = c(12500, 12500),
    intercept  = c(0, 0)
  )

  h <- unicon_help()
  expect_true("lp" %in% h$id, info = "id=lp expected_in_custom_state")
  expect_true("largepackage" %in% h$alias, info = "alias=largepackage expected_in_custom_state")
})

test_that("unicon_help reverts to package data after reset", {
  on.exit(unicon_reset_units(), add = TRUE)

  unicon_own(
    base_id    = c("lp", "lp"),
    base_alias = c("lp", "largepackage"),
    category   = c("mass", "mass"),
    srp        = c("g", "g"),
    slope      = c(12500, 12500),
    intercept  = c(0, 0)
  )
  unicon_reset_units()

  h <- unicon_help()
  expect_false("lp" %in% h$id, info = "id=lp should be removed after reset")
})

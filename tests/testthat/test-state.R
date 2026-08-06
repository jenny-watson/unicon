## state management -----------------------------------------------------------

## Helpers: minimal valid custom datasets

make_custom_alias <- function() {
  tibble::tibble(
    id    = c("lp", "sp"),
    alias = c("largepackage", "smallpackage")
  )
}

make_custom_srp <- function() {
  tibble::tibble(
    id  = c("lp", "sp"),
    srp = c("g", "g")
  )
}

make_custom_models <- function() {
  tibble::tibble(
    id    = c("lp", "sp"),
    model = list(
      list(slope = 12500, intercept = 0),
      list(slope = 6250,  intercept = 0)
    )
  )
}

## ---- default behaviour unchanged --------------------------------------------

test_that("get_unit_alias returns package data by default", {
  unicon_reset_units()
  ua <- unicon:::get_unit_alias()
  expect_true(is.data.frame(ua))
  expect_true(all(c("id", "alias") %in% names(ua)))
  expect_gt(nrow(ua), 0L)
})

test_that("get_unit_srp returns package data by default", {
  unicon_reset_units()
  us <- unicon:::get_unit_srp()
  expect_true(is.data.frame(us))
  expect_true(all(c("id", "srp") %in% names(us)))
  expect_gt(nrow(us), 0L)
})

test_that("get_unit_models returns package data by default", {
  unicon_reset_units()
  um <- unicon:::get_unit_models()
  expect_true(is.data.frame(um))
  expect_true(all(c("id", "model") %in% names(um)))
  expect_gt(nrow(um), 0L)
})

test_that("unicon_own_status is FALSE by default", {
  unicon_reset_units()
  expect_false(unicon_own_status())
})

## ---- set_unicon_data validation ---------------------------------------------

test_that("set_unicon_data rejects non-data-frame unit_alias", {
  expect_error(
    set_unicon_data("not_a_df", make_custom_srp(), make_custom_models()),
    "`unit_alias` must be a data frame."
  )
})

test_that("set_unicon_data rejects unit_alias missing required columns", {
  bad <- tibble::tibble(x = "a", y = "b")
  expect_error(
    set_unicon_data(bad, make_custom_srp(), make_custom_models()),
    "`unit_alias` must contain columns `id` and `alias`."
  )
})

test_that("set_unicon_data rejects unit_alias with non-character columns", {
  bad <- tibble::tibble(id = 1L, alias = "a")
  expect_error(
    set_unicon_data(bad, make_custom_srp(), make_custom_models()),
    "`unit_alias[$]id` and `unit_alias[$]alias` must be character vectors."
  )
})

test_that("set_unicon_data rejects non-data-frame unit_srp", {
  expect_error(
    set_unicon_data(make_custom_alias(), list(), make_custom_models()),
    "`unit_srp` must be a data frame."
  )
})

test_that("set_unicon_data rejects unit_srp missing required columns", {
  bad <- tibble::tibble(x = "a", y = "b")
  expect_error(
    set_unicon_data(make_custom_alias(), bad, make_custom_models()),
    "`unit_srp` must contain columns `id` and `srp`."
  )
})

test_that("set_unicon_data rejects non-data-frame unit_models", {
  expect_error(
    set_unicon_data(make_custom_alias(), make_custom_srp(), list()),
    "`unit_models` must be a data frame."
  )
})

test_that("set_unicon_data rejects unit_models missing required columns", {
  bad <- tibble::tibble(x = "a", y = list())
  expect_error(
    set_unicon_data(make_custom_alias(), make_custom_srp(), bad),
    "`unit_models` must contain columns `id` and `model`."
  )
})

test_that("set_unicon_data rejects unit_models with non-list model column", {
  bad <- tibble::tibble(id = "lp", model = "not_a_list")
  expect_error(
    set_unicon_data(make_custom_alias(), make_custom_srp(), bad),
    "`unit_models[$]model` must be a list column."
  )
})

## ---- set_unicon_data happy path + reset -------------------------------------

test_that("set_unicon_data stores custom data and flags using_custom", {
  on.exit(unicon_reset_units(), add = TRUE)

  set_unicon_data(make_custom_alias(), make_custom_srp(), make_custom_models())
  expect_true(unicon_own_status())

  ua <- unicon:::get_unit_alias()
  expect_equal(nrow(ua), 2L)
  expect_true("largepackage" %in% ua$alias)
})

test_that("unicon_reset_units restores package data", {
  on.exit(unicon_reset_units(), add = TRUE)

  set_unicon_data(make_custom_alias(), make_custom_srp(), make_custom_models())
  unicon_reset_units()

  expect_false(unicon_own_status())
  ua <- unicon:::get_unit_alias()
  expect_false("largepackage" %in% ua$alias)
})

## ---- unicon_full uses active state ------------------------------------------

test_that("unicon_full default behaviour unchanged after reset", {
  unicon_reset_units()
  result <- unicon_full(c(1, 2), c("kg", "g"), "kg", pull = TRUE)
  expect_equal(result, c(1, 0.001))
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
  expect_equal(result, 12500)
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
  expect_true(is.na(result))
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

  unicon_own(
    base_id    = c("lp", "lp"),
    base_alias = c("lp", "largepackage"),
    category   = c("mass", "mass"),
    srp        = c("g", "g"),
    slope      = c(12500, 12500),
    intercept  = c(0, 0)
  )

  h <- unicon_help()
  expect_true("lp" %in% h$id)
  expect_true("largepackage" %in% h$alias)
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
  expect_false("lp" %in% h$id)
})

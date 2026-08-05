## unicon_make_own_base_data ----------------------------------------------------

test_that("unicon_make_own_base_data rejects numeric id/alias/category/srp", {
  expect_error(
    unicon_make_own_base_data(1, "a", "cat", "srp", 1, 0),
    "`id`, `alias`, `category` and `srp` need to be characters"
  )
  expect_error(
    unicon_make_own_base_data("id", 2, "cat", "srp", 1, 0),
    "`id`, `alias`, `category` and `srp` need to be characters"
  )
})

test_that("unicon_make_own_base_data rejects character slope/intercept", {
  expect_error(
    unicon_make_own_base_data("id", "a", "cat", "srp", "one", 0),
    "`slope` and `intercept` need to be numeric"
  )
  expect_error(
    unicon_make_own_base_data("id", "a", "cat", "srp", 1, "zero"),
    "`slope` and `intercept` need to be numeric"
  )
})

test_that("unicon_make_own_base_data warns on non-zero intercept", {
  expect_warning(
    unicon_make_own_base_data("id", "a", "cat", "srp", 1, 5),
    "`intercept` is not zero, please check this is correct"
  )
})

test_that("unicon_make_own_base_data stops on mismatched vector lengths", {
  expect_error(
    unicon_make_own_base_data(
      c("id1", "id2"), "a", "cat", "srp", 1, 0
    ),
    "All vectors supplied to `unicon_make_own_base_data` must be the same length"
  )
  expect_error(
    unicon_make_own_base_data(
      "id", c("a", "b"), "cat", "srp", 1, 0
    ),
    "All vectors supplied to `unicon_make_own_base_data` must be the same length"
  )
})

test_that("unicon_make_own_base_data returns a tibble with correct columns", {
  result <- unicon_make_own_base_data("id", "a", "cat", "srp", 1, 0)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("id", "alias", "category", "srp", "slope", "intercept"))
  expect_equal(nrow(result), 1L)
})

## unicon_make_own_derived_data -------------------------------------------------

test_that("unicon_make_own_derived_data rejects numeric arguments", {
  expect_error(
    unicon_make_own_derived_data(1, "x", "y", "op"),
    "`id`, `x`, `y`, `operator` need to be characters"
  )
  expect_error(
    unicon_make_own_derived_data("id", 2, "y", "op"),
    "`id`, `x`, `y`, `operator` need to be characters"
  )
})

test_that("unicon_make_own_derived_data stops on mismatched vector lengths", {
  expect_error(
    unicon_make_own_derived_data(
      c("id1", "id2"), "x", "y", "op"
    ),
    "All vectors supplied to `unicon_make_own_derived_data` must be the same length"
  )
  expect_error(
    unicon_make_own_derived_data(
      "id", c("x1", "x2"), "y", "op"
    ),
    "All vectors supplied to `unicon_make_own_derived_data` must be the same length"
  )
})

test_that("unicon_make_own_derived_data returns a tibble with correct columns", {
  result <- unicon_make_own_derived_data("id", "x", "y", "op")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("id", "x", "y", "operator"))
  expect_equal(nrow(result), 1L)
})

## unicon_make_own_operators_data -----------------------------------------------

test_that("unicon_make_own_operators_data rejects numeric arguments", {
  expect_error(
    unicon_make_own_operators_data(1, "id", "fun", "alias"),
    "`operator`, `id`, `fun` and `alias` need to be characters"
  )
  expect_error(
    unicon_make_own_operators_data("op", 2, "fun", "alias"),
    "`operator`, `id`, `fun` and `alias` need to be characters"
  )
})

test_that("unicon_make_own_operators_data stops on mismatched vector lengths", {
  expect_error(
    unicon_make_own_operators_data(
      c("op1", "op2"), "id", "fun", "alias"
    ),
    "All vectors supplied to `unicon_make_own_operators_data` must be the same length"
  )
  expect_error(
    unicon_make_own_operators_data(
      "op", "id", c("fun1", "fun2"), "alias"
    ),
    "All vectors supplied to `unicon_make_own_operators_data` must be the same length"
  )
})

test_that("unicon_make_own_operators_data returns a tibble with correct columns", {
  result <- unicon_make_own_operators_data("op", "id", "fun", "alias")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("operator", "id", "fun", "alias"))
  expect_equal(nrow(result), 1L)
})

## unicon_own -------------------------------------------------------------------

test_that("unicon_own stops when base data fields are missing", {
  expect_error(
    unicon_own(),
    "Not enough data provided to create a dataset"
  )
  expect_error(
    unicon_own(base_id = "id"),
    "Not enough data provided to create a dataset"
  )
})

test_that("unicon_own warns when derived data is provided without operator data", {
  expect_warning(
    unicon_own(
      base_id    = "density",
      base_alias = "density",
      category   = "density",
      srp        = "density",
      slope      = 1,
      intercept  = 0,
      derived_id = "myvol",
      x          = "mass",
      y          = "length",
      operator   = "per"
    ),
    "no operator dataset has been provided"
  )
})

## unicon_make_own_base_data ----------------------------------------------------

test_that("unicon_make_own_base_data rejects numeric id/alias/category/srp", {
  expect_error(
    unicon_make_own_base_data(1, "a", "cat", "srp", 1, 0),
    "`id`, `alias`, `category` and `srp` need to be characters",
    info = "id_type=numeric, alias=a, category=cat, srp=srp"
  )
  expect_error(
    unicon_make_own_base_data("id", 2, "cat", "srp", 1, 0),
    "`id`, `alias`, `category` and `srp` need to be characters",
    info = "id=id, alias_type=numeric, category=cat, srp=srp"
  )
})

test_that("unicon_make_own_base_data rejects character slope/intercept", {
  expect_error(
    unicon_make_own_base_data("id", "a", "cat", "srp", "one", 0),
    "`slope` and `intercept` need to be numeric",
    info = "id=id, alias=a, category=cat, srp=srp, slope_type=character, intercept=0"
  )
  expect_error(
    unicon_make_own_base_data("id", "a", "cat", "srp", 1, "zero"),
    "`slope` and `intercept` need to be numeric",
    info = "id=id, alias=a, category=cat, srp=srp, slope=1, intercept_type=character"
  )
})

test_that("unicon_make_own_base_data warns on non-zero intercept", {
  expect_warning(
    unicon_make_own_base_data("id", "a", "cat", "srp", 1, 5),
    "`intercept` is not zero, please check this is correct",
    info = "id=id, alias=a, category=cat, srp=srp, slope=1, intercept=5"
  )
})

test_that("unicon_make_own_base_data stops on mismatched vector lengths", {
  expect_error(
    unicon_make_own_base_data(
      c("id1", "id2"), "a", "cat", "srp", 1, 0
    ),
    "All vectors supplied to `unicon_make_own_base_data` must be the same length",
    info = paste0(
      "id_length=2, alias_length=1, category_length=1, ",
      "srp_length=1, slope_length=1, intercept_length=1"
    )
  )
  expect_error(
    unicon_make_own_base_data(
      "id", c("a", "b"), "cat", "srp", 1, 0
    ),
    "All vectors supplied to `unicon_make_own_base_data` must be the same length",
    info = paste0(
      "id_length=1, alias_length=2, category_length=1, ",
      "srp_length=1, slope_length=1, intercept_length=1"
    )
  )
})

test_that("unicon_make_own_base_data returns a tibble with correct columns", {
  result <- unicon_make_own_base_data("id", "a", "cat", "srp", 1, 0)
  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c("id", "alias", "category", "srp", "slope", "intercept"),
    info = "dataset=base_data_columns"
  )
  expect_equal(nrow(result), 1L, info = "dataset=base_data, expected_rows=1")
})

## unicon_make_own_derived_data -------------------------------------------------

test_that("unicon_make_own_derived_data rejects numeric arguments", {
  expect_error(
    unicon_make_own_derived_data(1, "x", "y", "op"),
    "`id`, `x`, `y`, `operator` need to be characters",
    info = "id_type=numeric, x=x, y=y, operator=op"
  )
  expect_error(
    unicon_make_own_derived_data("id", 2, "y", "op"),
    "`id`, `x`, `y`, `operator` need to be characters",
    info = "id=id, x_type=numeric, y=y, operator=op"
  )
})

test_that("unicon_make_own_derived_data stops on mismatched vector lengths", {
  expect_error(
    unicon_make_own_derived_data(
      c("id1", "id2"), "x", "y", "op"
    ),
    "All vectors supplied to `unicon_make_own_derived_data` must be the same length",
    info = "id_length=2, x_length=1, y_length=1, operator_length=1"
  )
  expect_error(
    unicon_make_own_derived_data(
      "id", c("x1", "x2"), "y", "op"
    ),
    "All vectors supplied to `unicon_make_own_derived_data` must be the same length",
    info = "id_length=1, x_length=2, y_length=1, operator_length=1"
  )
})

test_that("unicon_make_own_derived_data returns a tibble with correct columns", {
  result <- unicon_make_own_derived_data("id", "x", "y", "op")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("id", "x", "y", "operator"), info = "dataset=derived_data_columns")
  expect_equal(nrow(result), 1L, info = "dataset=derived_data, expected_rows=1")
})

## unicon_make_own_operators_data -----------------------------------------------

test_that("unicon_make_own_operators_data rejects numeric arguments", {
  expect_error(
    unicon_make_own_operators_data(1, "id", "fun", "alias"),
    "`operator`, `id`, `fun` and `alias` need to be characters",
    info = "operator_type=numeric, id=id, fun=fun, alias=alias"
  )
  expect_error(
    unicon_make_own_operators_data("op", 2, "fun", "alias"),
    "`operator`, `id`, `fun` and `alias` need to be characters",
    info = "operator=op, id_type=numeric, fun=fun, alias=alias"
  )
})

test_that("unicon_make_own_operators_data stops on mismatched vector lengths", {
  expect_error(
    unicon_make_own_operators_data(
      c("op1", "op2"), "id", "fun", "alias"
    ),
    "All vectors supplied to `unicon_make_own_operators_data` must be the same length",
    info = "operator_length=2, id_length=1, fun_length=1, alias_length=1"
  )
  expect_error(
    unicon_make_own_operators_data(
      "op", "id", c("fun1", "fun2"), "alias"
    ),
    "All vectors supplied to `unicon_make_own_operators_data` must be the same length",
    info = "operator_length=1, id_length=1, fun_length=2, alias_length=1"
  )
})

test_that("unicon_make_own_operators_data returns a tibble with correct columns", {
  result <- unicon_make_own_operators_data("op", "id", "fun", "alias")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("operator", "id", "fun", "alias"), info = "dataset=operators_data_columns")
  expect_equal(nrow(result), 1L, info = "dataset=operators_data, expected_rows=1")
})

## unicon_own -------------------------------------------------------------------

test_that("unicon_own stops when base data fields are missing", {
  expect_error(
    unicon_own(),
    "Not enough data provided to create a dataset",
    info = "base_id=NULL, base_alias=NULL, category=NULL, srp=NULL"
  )
  expect_error(
    unicon_own(base_id = "id"),
    "Not enough data provided to create a dataset",
    info = "base_id=id, base_alias=NULL, category=NULL, srp=NULL"
  )
})

test_that("unicon_own stops when derived data x/y categories are not in base data", {
  expect_error(
    unicon_own(
      base_id    = "density",
      base_alias = "density",
      category   = "density",
      srp        = "density",
      slope      = 1,
      intercept  = 0,
      derived_id = "myvol",
      x          = "not_a_real_category",
      y          = "length",
      operator   = "per"
    ),
    "do not exist in the base data",
    info = "derived_id=myvol, x=not_a_real_category, y=length, operator=per"
  )
})

test_that("unicon_own stops when derived data operator is not in operator data", {
  expect_error(
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
      operator   = "not_a_real_operator"
    ),
    "do not exist in the operator data",
    info = "derived_id=myvol, x=mass, y=length, operator=not_a_real_operator"
  )
})

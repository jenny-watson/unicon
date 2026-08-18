## Tests for vignette examples -------------------------------------------------
## Verifies that the examples documented in the package vignettes continue to
## produce the expected results and that partial-failure cases are handled
## consistently between unicon_full and unicon_lite.

test_that("documented unicon_full and unicon_lite examples stay aligned", {
  raw_values <- c(54.21, 71.24, 55.81, 11.33, 70.59)
  raw_units <- c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "g/m2")
  raw_ids <- c("t__ha", "st__acre", "t__ha", "kg__ha", "g__m2")
  unit_out <- "tonnes / ha"
  id_out <- "t__ha"

  expect_equal(
    unicon_full(
      value_in = raw_values,
      unit_in  = raw_units,
      unit_out = unit_out,
    ),
    unicon_lite(
      value_in = raw_values,
      id_in    = raw_ids,
      id_out   = id_out
    )$value_out
  )
})

test_that("documented invalid unit example keeps valid rows and flags the bad one", {
  raw_values <- c(54.21, 71.24, 55.81, 11.33, 70.59)
  raw_units <- c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "nonsense per fiction")
  valid_rows <- seq_len(4L)
  valid_ids <- c("t__ha", "st__acre", "t__ha", "kg__ha")
  unit_out <- "tonnes / ha"
  id_out <- "t__ha"

  expect_warning(
    full_tbl <- unicon_full(
      value_in = raw_values,
      unit_in  = raw_units,
      unit_out = unit_out,
      pull     = FALSE
    ),
    "Some input units failed to find matches\\."
  )

  lite_out <- unicon_lite(
    value_in = raw_values[valid_rows],
    id_in    = valid_ids,
    id_out   = id_out
  )

  expect_false(any(full_tbl$error_in[valid_rows]))
  expect_true(full_tbl$error_in[[length(raw_values)]])
  expect_equal(full_tbl$value_out[valid_rows], lite_out$value_out)
  expect_true(is.na(full_tbl$value_out[[length(raw_values)]]))
})

## ---- unicon_help vignette examples ------------------------------------------

test_that("vignette unicon_help example returns a complete table with expected columns", {
  unicon_reset_units()
  h <- unicon_help()

  expect_true(is.data.frame(h))
  expect_gt(nrow(h), 0L)
  expect_true(all(c("id", "alias", "type", "category", "srp", "model") %in% names(h)))
})

test_that("vignette unicon_help output contains no missing values", {
  unicon_reset_units()
  h <- unicon_help()

  expect_false(anyNA(h[c("id", "alias", "type", "category", "srp")]))
})

test_that("vignette unicon_help has a one-to-one mapping between srp and category", {
  unicon_reset_units()
  h <- unicon_help()

  categories_per_srp <- tapply(h$category, h$srp, function(x) length(unique(x)))
  srps_per_category <- tapply(h$srp, h$category, function(x) length(unique(x)))
  expect_true(all(categories_per_srp == 1L))
  expect_true(all(srps_per_category == 1L))
})

## ---- unicon_own vignette example --------------------------------------------

test_that("vignette unicon_own example: custom units appear in unicon_help and convert correctly", {
  unicon_reset_units()
  on.exit(unicon_reset_units(), add = TRUE)

  unicon_own(
    base_id    = c("lp", "lp"),
    base_alias = c("lp", "largepackage"),
    category   = c("mass", "mass"),
    srp        = c("g", "g"),
    slope      = c(12500, 12500),
    intercept  = c(0, 0)
  )

  expect_true(unicon_own_status())

  h <- unicon_help()
  expect_true("lp" %in% h$id)
  expect_true("largepackage" %in% h$alias)
  expect_false(anyNA(h[c("id", "alias", "type", "category", "srp")]))

  result <- unicon_full(
    value_in = 1,
    unit_in  = "largepackage",
    unit_out = "g",
    pull     = TRUE
  )
  expect_equal(result, 12500)
})

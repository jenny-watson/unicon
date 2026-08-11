## Snapshot tests for package data and function errors/messages ---------------
##
## Package data: unit_alias, unit_srp, unit_models, relationships
## Function messages/errors: unicon_full, unicon_lite, unicon_advance,
##                            unicon_own (make helpers), state functions

# tests/testthat/setup.R

unit_alias <- get("unit_alias", envir = asNamespace("unicon"))
unit_srp <- get("unit_srp", envir = asNamespace("unicon"))
unit_models <- get("unit_models", envir = asNamespace("unicon"))
relationships <- get("relationships", envir = asNamespace("unicon"))

## ---- package data: unit_alias -----------------------------------------------

test_that("unit_alias snapshot: dimensions and column names", {
  expect_snapshot({
    cat("nrow:", nrow(unit_alias), "\n")
    cat("ncol:", ncol(unit_alias), "\n")
    cat("names:", paste(sort(names(unit_alias)), collapse = ", "), "\n")
  })
})

test_that("unit_alias snapshot: class and types", {
  expect_snapshot({
    cat("class:", class(unit_alias), "\n")
    cat("id type:", class(unit_alias$id), "\n")
    cat("alias type:", class(unit_alias$alias), "\n")
  })
})

test_that("unit_alias snapshot: sample of known unit entries", {
  # Metre and kilogram should always be present with canonical aliases
  m_row <- unit_alias[unit_alias$id == "m" & unit_alias$alias == "m", ]
  kg_row <- unit_alias[unit_alias$id == "kg" & unit_alias$alias == "kg", ]
  expect_snapshot({
    cat("m row present:", nrow(m_row) == 1L, "\n")
    cat("kg row present:", nrow(kg_row) == 1L, "\n")
  })
})

## ---- package data: unit_srp -------------------------------------------------

test_that("unit_srp snapshot: dimensions and column names", {
  expect_snapshot({
    cat("nrow:", nrow(unit_srp), "\n")
    cat("ncol:", ncol(unit_srp), "\n")
    cat("names:", paste(sort(names(unit_srp)), collapse = ", "), "\n")
  })
})

test_that("unit_srp snapshot: class and types", {
  expect_snapshot({
    cat("class:", class(unit_srp), "\n")
    cat("id type:", class(unit_srp$id), "\n")
    cat("srp type:", class(unit_srp$srp), "\n")
  })
})

test_that("unit_srp snapshot: known SRP entries", {
  # Length SRP is metre, mass SRP is kilogram, temperature SRP is Celsius
  expect_snapshot({
    cat("length SRP:", unit_srp$srp[unit_srp$id == "m"], "\n")
    cat("mass SRP:", unit_srp$srp[unit_srp$id == "kg"], "\n")
    cat("temperature SRP:", unit_srp$srp[unit_srp$id == "C"], "\n")
  })
})

## ---- package data: unit_models ----------------------------------------------

test_that("unit_models snapshot: dimensions and column names", {
  expect_snapshot({
    cat("nrow:", nrow(unit_models), "\n")
    cat("ncol:", ncol(unit_models), "\n")
    cat("names:", paste(sort(names(unit_models)), collapse = ", "), "\n")
  })
})

test_that("unit_models snapshot: class and types", {
  expect_snapshot({
    cat("class:", class(unit_models), "\n")
    cat("id type:", class(unit_models$id), "\n")
    cat("model type:", class(unit_models$model), "\n")
  })
})

test_that("unit_models snapshot: SRP units have slope=1 intercept=0", {
  # Metre: slope 1, intercept 0
  m_model <- unit_models$model[unit_models$id == "m"][[1L]]
  kg_model <- unit_models$model[unit_models$id == "kg"][[1L]]
  expect_snapshot({
    cat("m slope:", m_model$slope, "\n")
    cat("m intercept:", m_model$intercept, "\n")
    cat("kg slope:", kg_model$slope, "\n")
    cat("kg intercept:", kg_model$intercept, "\n")
  })
})

test_that("unit_models snapshot: temperature SRP (Celsius) has slope=1 intercept=0", {
  c_model <- unit_models$model[unit_models$id == "C"][[1L]]
  expect_snapshot({
    cat("C slope:", c_model$slope, "\n")
    cat("C intercept:", c_model$intercept, "\n")
  })
})

## ---- package data: relationships --------------------------------------------

test_that("relationships snapshot: dimensions and column names", {
  expect_snapshot({
    cat("nrow:", nrow(relationships), "\n")
    cat("ncol:", ncol(relationships), "\n")
    cat("names:", paste(sort(names(relationships)), collapse = ", "), "\n")
  })
})

test_that("relationships snapshot: class", {
  expect_snapshot({
    cat("class:", class(relationships), "\n")
  })
})

test_that("relationships snapshot: known relationships present", {
  # length/time = speed, mass/area = area_density, amount/volume = concentration
  cats <- sort(unique(relationships$id))
  expect_snapshot(cat(paste(cats, collapse = "\n"), "\n"))
})

## ---- unicon_full: error messages --------------------------------------------

test_that("unicon_full error snapshot: non-numeric value_in", {
  expect_snapshot(
    unicon_full("1", "m", "cm"),
    error = TRUE
  )
})

test_that("unicon_full error snapshot: non-character unit_in", {
  expect_snapshot(
    unicon_full(1, 2, "cm"),
    error = TRUE
  )
})

test_that("unicon_full error snapshot: non-character unit_out", {
  expect_snapshot(
    unicon_full(1, "m", TRUE),
    error = TRUE
  )
})

test_that("unicon_full error snapshot: zero-length value_in", {
  expect_snapshot(
    unicon_full(numeric(0), "m", "cm"),
    error = TRUE
  )
})

test_that("unicon_full error snapshot: wrong-length unit_in", {
  expect_snapshot(
    unicon_full(1:2, c("m", "cm", "km"), "cm"),
    error = TRUE
  )
})

test_that("unicon_full error snapshot: wrong-length unit_out", {
  expect_snapshot(
    unicon_full(1:3, "m", c("cm", "mm")),
    error = TRUE
  )
})

test_that("unicon_full message snapshot: no unit_out given", {
  expect_snapshot(
    unicon_full(1, "m")
  )
})

test_that("unicon_full message snapshot: partially missing unit_out", {
  expect_snapshot(
    unicon_full(c(1, 2), "m", c("cm", NA))
  )
})

test_that("unicon_full warning snapshot: unknown unit_in", {
  expect_snapshot(
    unicon_full(1, "not_a_unit", "cm")
  )
})

test_that("unicon_full warning snapshot: unknown unit_out", {
  expect_snapshot(
    unicon_full(1, "m", "not_a_unit")
  )
})

test_that("unicon_full warning snapshot: mismatched unit types", {
  expect_snapshot(
    unicon_full(1, "m", "kg")
  )
})

## ---- unicon_lite: error messages --------------------------------------------

test_that("unicon_lite warning snapshot: unknown id_in", {
  expect_snapshot(
    unicon_lite(1, "not_a_unit", "cm")
  )
})

test_that("unicon_lite warning snapshot: mismatched unit types", {
  expect_snapshot(
    unicon_lite(1, "m", "kg")
  )
})

## ---- unicon_advance: error messages -----------------------------------------

test_that("unicon_advance error snapshot: mismatched value lengths", {
  expect_snapshot(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1),
      unit_out = "km/hour"
    ),
    error = TRUE
  )
})

test_that("unicon_advance error snapshot: no recorded relationship", {
  expect_snapshot(
    unicon_advance(
      x_unit_in = "m",
      y_unit_in = "kg",
      x_value_in = 1,
      y_value_in = 1,
      unit_out = NA
    ),
    error = TRUE
  )
})

test_that("unicon_advance error snapshot: operator mismatch", {
  expect_snapshot(
    unicon_advance(
      x_unit_in = "kg",
      y_unit_in = "ha",
      x_value_in = 10,
      y_value_in = 2,
      unit_out = NA,
      operator_in = "multiply"
    ),
    error = TRUE
  )
})

## ---- unicon_make_own_base_data: error and warning messages ------------------

test_that("unicon_make_own_base_data error snapshot: non-character id", {
  expect_snapshot(
    unicon_make_own_base_data(1, "alias", "cat", "srp", 1, 0),
    error = TRUE
  )
})

test_that("unicon_make_own_base_data error snapshot: non-numeric slope", {
  expect_snapshot(
    unicon_make_own_base_data("id", "alias", "cat", "srp", "one", 0),
    error = TRUE
  )
})

test_that("unicon_make_own_base_data warning snapshot: non-zero intercept", {
  expect_snapshot(
    unicon_make_own_base_data("id", "alias", "cat", "srp", 1, 5)
  )
})

test_that("unicon_make_own_base_data error snapshot: mismatched vector lengths", {
  expect_snapshot(
    unicon_make_own_base_data(c("id1", "id2"), "alias", "cat", "srp", 1, 0),
    error = TRUE
  )
})

## ---- state functions: messages ----------------------------------------------

test_that("unicon_reset_units snapshot: resets state silently", {
  expect_snapshot(unicon_reset_units())
})

test_that("unicon_own_status snapshot: returns FALSE after reset", {
  unicon_reset_units()
  expect_snapshot(unicon_own_status())
})

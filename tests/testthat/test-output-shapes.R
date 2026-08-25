## Snapshot tests for function output shapes ------------------------------------
##
## Validates that the column names and column types of the data-frame outputs
## of every public function remain stable across changes.
##
## Functions covered:
##   unicon_full(pull = FALSE)
##   unicon_lite()
##   unicon_advance(pull = FALSE)
##   unicon_make_own_base_data()
##   unicon_make_own_derived_data()
##   unicon_make_own_operators_data()

## ---- helpers ----------------------------------------------------------------

col_types <- function(df) {
  vapply(df, function(col) {
    if (is.list(col)) {
      "list"
    } else {
      paste(class(col), collapse = "/")
    }
  }, character(1L))
}

snap_shape <- function(df) {
  types <- col_types(df)
  cat("nrow:", nrow(df), "\n")
  cat("ncol:", ncol(df), "\n")
  for (nm in names(types)) {
    cat(" ", nm, ":", types[[nm]], "\n")
  }
}

## ---- unicon_full(pull = FALSE) ----------------------------------------------

test_that("unicon_full pull=FALSE snapshot: column names", {
  out <- suppressMessages(
    unicon_full(c(1, 2), "m", "cm", pull = FALSE)
  )
  expect_snapshot(cat(paste(names(out), collapse = "\n"), "\n"))
})

test_that("unicon_full pull=FALSE snapshot: column types", {
  out <- suppressMessages(
    unicon_full(c(1, 2), "m", "cm", pull = FALSE)
  )
  expect_snapshot({
    types <- col_types(out)
    for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
  })
})

test_that("unicon_full pull=FALSE snapshot: full shape for length conversion", {
  out <- suppressMessages(
    unicon_full(c(1, 1000), "m", "km", pull = FALSE)
  )
  expect_snapshot(snap_shape(out))
})

test_that("unicon_full pull=FALSE snapshot: full shape for temperature conversion", {
  out <- suppressMessages(
    unicon_full(c(0, 100), "celsius", "fahrenheit", pull = FALSE)
  )
  expect_snapshot(snap_shape(out))
})

test_that("unicon_full pull=FALSE snapshot: shape consistent across categories", {
  length_out <- suppressMessages(
    unicon_full(1, "m", "cm", pull = FALSE)
  )
  mass_out <- suppressMessages(
    unicon_full(1, "kg", "g", pull = FALSE)
  )
  temp_out <- suppressMessages(
    unicon_full(1, "celsius", "fahrenheit", pull = FALSE)
  )
  expect_snapshot({
    cat("length names match mass:", identical(names(length_out), names(mass_out)), "\n")
    cat("length names match temp:", identical(names(length_out), names(temp_out)), "\n")
    cat("length types match mass:", identical(col_types(length_out), col_types(mass_out)), "\n")
    cat("length types match temp:", identical(col_types(length_out), col_types(temp_out)), "\n")
  })
})

## ---- unicon_lite() ----------------------------------------------------------

test_that("unicon_lite snapshot: column names", {
  out <- suppressMessages(
    unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
  )
  expect_snapshot(cat(paste(names(out), collapse = "\n"), "\n"))
})

test_that("unicon_lite snapshot: column types", {
  out <- suppressMessages(
    unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
  )
  expect_snapshot({
    types <- col_types(out)
    for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
  })
})

test_that("unicon_lite snapshot: full shape for mass conversion", {
  out <- suppressMessages(
    unicon_lite(c(1, 2.5), "kg", "g")
  )
  expect_snapshot(snap_shape(out))
})

test_that("unicon_lite snapshot: shape consistent across unit categories", {
  length_out <- suppressMessages(unicon_lite(1, "m", "cm"))
  mass_out <- suppressMessages(unicon_lite(1, "kg", "g"))
  temp_out <- suppressMessages(unicon_lite(1, "celsius", "fahrenheit"))
  expect_snapshot({
    cat("length names match mass:", identical(names(length_out), names(mass_out)), "\n")
    cat("length names match temp:", identical(names(length_out), names(temp_out)), "\n")
    cat("length types match mass:", identical(col_types(length_out), col_types(mass_out)), "\n")
    cat("length types match temp:", identical(col_types(length_out), col_types(temp_out)), "\n")
  })
})

test_that("unicon_lite and unicon_full share overlapping column names and types", {
  lite_out <- suppressMessages(
    unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
  )
  full_out <- suppressMessages(
    unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
  )
  shared_cols <- intersect(names(lite_out), names(full_out))
  expect_snapshot({
    cat("shared columns:", paste(shared_cols, collapse = ", "), "\n")
    lite_types <- col_types(lite_out[shared_cols])
    full_types <- col_types(full_out[shared_cols])
    cat("types match:", identical(lite_types, full_types), "\n")
  })
})

## ---- unicon_advance(pull = FALSE) -------------------------------------------

test_that("unicon_advance pull=FALSE snapshot: column names", {
  out <- suppressWarnings(suppressMessages(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = c(100, 50),
      y_value_in = c(2, 1),
      unit_out = "km/hour",
      pull = FALSE
    )
  ))
  expect_snapshot(cat(paste(names(out), collapse = "\n"), "\n"))
})

test_that("unicon_advance pull=FALSE snapshot: column types", {
  out <- suppressWarnings(suppressMessages(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = c(100, 50),
      y_value_in = c(2, 1),
      unit_out = "km/hour",
      pull = FALSE
    )
  ))
  expect_snapshot({
    types <- col_types(out)
    for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
  })
})

test_that("unicon_advance pull=FALSE snapshot: shape for speed conversion", {
  out <- suppressWarnings(suppressMessages(
    unicon_advance(
      x_unit_in = "km",
      y_unit_in = "hour",
      x_value_in = c(100, 200),
      y_value_in = c(2, 4),
      unit_out = "km/hour",
      pull = FALSE
    )
  ))
  expect_snapshot(snap_shape(out))
})

test_that("unicon_advance pull=FALSE snapshot: shape for area_density conversion", {
  out <- suppressWarnings(suppressMessages(
    unicon_advance(
      x_unit_in = "kg",
      y_unit_in = "ha",
      x_value_in = c(10, 20),
      y_value_in = c(2, 4),
      unit_out = NA,
      operator_in = "divide",
      pull = FALSE
    )
  ))
  expect_snapshot(snap_shape(out))
})

test_that("unicon_advance pull=FALSE shape is consistent across derived categories", {
  speed_out <- suppressWarnings(suppressMessages(
    unicon_advance(
      x_unit_in = "km",
      y_unit_in = "hour",
      x_value_in = 100,
      y_value_in = 2,
      unit_out = "km/hour",
      pull = FALSE
    )
  ))
  density_out <- suppressWarnings(suppressMessages(
    unicon_advance(
      x_unit_in = "kg",
      y_unit_in = "ha",
      x_value_in = 10,
      y_value_in = 2,
      unit_out = NA,
      operator_in = "divide",
      pull = FALSE
    )
  ))
  expect_snapshot({
    cat("speed names match density:", identical(names(speed_out), names(density_out)), "\n")
    cat("speed types match density:", identical(col_types(speed_out), col_types(density_out)), "\n")
  })
})

## ---- unicon_make_own_base_data() --------------------------------------------

test_that("unicon_make_own_base_data snapshot: column names", {
  out <- suppressWarnings(
    unicon_make_own_base_data("test_id", "test_alias", "test_cat", "srp_id", 2, 0)
  )
  expect_snapshot(cat(paste(names(out), collapse = "\n"), "\n"))
})

test_that("unicon_make_own_base_data snapshot: column types", {
  out <- suppressWarnings(
    unicon_make_own_base_data("test_id", "test_alias", "test_cat", "srp_id", 2, 0)
  )
  expect_snapshot({
    types <- col_types(out)
    for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
  })
})

## ---- unicon_make_own_derived_data() -----------------------------------------

test_that("unicon_make_own_derived_data snapshot: column names", {
  out <- unicon_make_own_derived_data(
    id = "speed", x = "length", y = "time", operator = "divide"
  )
  expect_snapshot(cat(paste(names(out), collapse = "\n"), "\n"))
})

test_that("unicon_make_own_derived_data snapshot: column types", {
  out <- unicon_make_own_derived_data(
    id = "speed", x = "length", y = "time", operator = "divide"
  )
  expect_snapshot({
    types <- col_types(out)
    for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
  })
})

## ---- unicon_make_own_operators_data() ---------------------------------------

test_that("unicon_make_own_operators_data snapshot: column names", {
  out <- unicon_make_own_operators_data(
    operator = "divide", id = "_", fun = "/", alias = "per"
  )
  expect_snapshot(cat(paste(names(out), collapse = "\n"), "\n"))
})

test_that("unicon_make_own_operators_data snapshot: column types", {
  out <- unicon_make_own_operators_data(
    operator = "divide", id = "_", fun = "/", alias = "per"
  )
  expect_snapshot({
    types <- col_types(out)
    for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
  })
})

## Snapshot tests for all inst/units JSON files --------------------------------
##
## Validates the schema (required keys, value types, structural consistency)
## of every JSON file shipped with the package:
##   - inst/units/base/**/*.json     — base unit definitions
##   - inst/units/derived/*.json     — derived unit relationships
##   - inst/units/operators/*.json   — operator definitions
##   - inst/units/srp.json           — standard reference point index

## ---- helpers ----------------------------------------------------------------

read_pkg_json <- function(...) {
  jsonlite::read_json(
    system.file(..., package = "unicon"),
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE
  )
}

list_pkg_jsons <- function(subdir) {
  dir <- system.file(subdir, package = "unicon")
  list.files(dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
}

## ---- srp.json ---------------------------------------------------------------

test_that("srp.json snapshot: top-level keys and value types", {
  srp <- read_pkg_json("units", "srp.json")
  expect_snapshot({
    cat("keys:", paste(sort(names(srp)), collapse = ", "), "\n")
    cat("all values character:", all(vapply(srp, is.character, logical(1L))), "\n")
    cat("n categories:", length(srp), "\n")
  })
})

test_that("srp.json snapshot: known category SRP mappings", {
  srp <- read_pkg_json("units", "srp.json")
  expect_snapshot({
    cat("length ->", srp[["length"]], "\n")
    cat("mass ->", srp[["mass"]], "\n")
    cat("temperature ->", srp[["temperature"]], "\n")
    cat("time ->", srp[["time"]], "\n")
    cat("volume ->", srp[["volume"]], "\n")
    cat("area ->", srp[["area"]], "\n")
  })
})

## ---- base unit JSON files ---------------------------------------------------

test_that("all base unit JSON files have exactly the required top-level keys", {
  paths <- list_pkg_jsons("units/base")
  required_keys <- c("category", "srp", "model", "alias")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    all(required_keys %in% names(dat)) && all(names(dat) %in% required_keys)
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("total base JSONs:", length(paths), "\n")
    cat("all pass schema:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("all base unit JSON files have a numeric model with slope and intercept", {
  paths <- list_pkg_jsons("units/base")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    m <- dat$model
    is.list(m) &&
      all(c("slope", "intercept") %in% names(m)) &&
      is.numeric(m$slope) &&
      is.numeric(m$intercept)
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("all have valid model:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("all base unit JSON files have a non-empty character alias vector", {
  paths <- list_pkg_jsons("units/base")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    is.character(dat$alias) && length(dat$alias) >= 1L
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("all have valid alias:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("all base unit JSON files have character category and srp fields", {
  paths <- list_pkg_jsons("units/base")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    is.character(dat$category) && length(dat$category) == 1L &&
      is.character(dat$srp) && length(dat$srp) == 1L
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("all have valid category+srp:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("base unit JSON snapshot: known units have expected category", {
  m    <- read_pkg_json("units", "base", "length", "m.json")
  kg   <- read_pkg_json("units", "base", "mass", "kg.json")
  c_   <- read_pkg_json("units", "base", "temperature", "C.json")
  l_   <- read_pkg_json("units", "base", "volume", "l.json")
  expect_snapshot({
    cat("m category:", m$category, "\n")
    cat("kg category:", kg$category, "\n")
    cat("C category:", c_$category, "\n")
    cat("l category:", l_$category, "\n")
  })
})

test_that("base unit JSON snapshot: SRP units have slope=1 and intercept=0", {
  m    <- read_pkg_json("units", "base", "length", "m.json")
  kg   <- read_pkg_json("units", "base", "mass", "kg.json")
  c_   <- read_pkg_json("units", "base", "temperature", "C.json")
  expect_snapshot({
    cat("m slope:", m$model$slope, "intercept:", m$model$intercept, "\n")
    cat("kg slope:", kg$model$slope, "intercept:", kg$model$intercept, "\n")
    cat("C slope:", c_$model$slope, "intercept:", c_$model$intercept, "\n")
  })
})

test_that("base unit JSON snapshot: Fahrenheit has expected model coefficients", {
  f <- read_pkg_json("units", "base", "temperature", "fahrenheit.json")
  expect_snapshot({
    cat("fahrenheit category:", f$category, "\n")
    cat("fahrenheit srp:", f$srp, "\n")
    cat("fahrenheit slope:", round(f$model$slope, 4L), "\n")
    cat("fahrenheit intercept:", round(f$model$intercept, 4L), "\n")
  })
})

test_that("base unit JSON snapshot: Kelvin has expected model coefficients", {
  k <- read_pkg_json("units", "base", "temperature", "kelvin.json")
  expect_snapshot({
    cat("kelvin category:", k$category, "\n")
    cat("kelvin srp:", k$srp, "\n")
    cat("kelvin slope:", k$model$slope, "\n")
    cat("kelvin intercept:", k$model$intercept, "\n")
  })
})

## ---- derived unit JSON files ------------------------------------------------

test_that("all derived unit JSON files have exactly the required top-level keys", {
  paths <- list_pkg_jsons("units/derived")
  required_keys <- c("x", "y", "operator")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    all(required_keys %in% names(dat)) && all(names(dat) %in% required_keys)
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("total derived JSONs:", length(paths), "\n")
    cat("all pass schema:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("all derived unit JSON files have character x, y, and operator fields", {
  paths <- list_pkg_jsons("units/derived")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    is.character(dat$x) && is.character(dat$y) && is.character(dat$operator)
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("all have valid x/y/operator:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("derived unit JSON snapshot: known derived relationships", {
  speed <- read_pkg_json("units", "derived", "speed.json")
  area_density <- read_pkg_json("units", "derived", "area_density.json")
  concentration <- read_pkg_json("units", "derived", "concentration.json")
  expect_snapshot({
    cat("speed: x=", speed$x, " y=", speed$y, " op=", speed$operator, "\n")
    cat("area_density: x=", area_density$x, " y=", area_density$y,
        " op=", area_density$operator, "\n")
    cat("concentration: x=", concentration$x, " y=", concentration$y,
        " op=", concentration$operator, "\n")
  })
})

test_that("derived unit JSON snapshot: all operators are 'divide' or 'multiply'", {
  paths <- list_pkg_jsons("units/derived")
  operators <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    dat$operator
  }, character(1L))
  valid <- c("divide", "multiply")
  expect_snapshot({
    cat("unique operators:", paste(sort(unique(operators)), collapse = ", "), "\n")
    cat("all valid:", all(operators %in% valid), "\n")
  })
})

## ---- operator JSON files ----------------------------------------------------

test_that("all operator JSON files have exactly the required top-level keys", {
  paths <- list_pkg_jsons("units/operators")
  required_keys <- c("id", "fun", "alias")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    all(required_keys %in% names(dat)) && all(names(dat) %in% required_keys)
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("total operator JSONs:", length(paths), "\n")
    cat("all pass schema:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

test_that("operator JSON snapshot: divide operator definition", {
  divide <- read_pkg_json("units", "operators", "divide.json")
  expect_snapshot({
    cat("id:", divide$id, "\n")
    cat("fun:", divide$fun, "\n")
    cat("aliases:", paste(divide$alias, collapse = ", "), "\n")
  })
})

test_that("operator JSON snapshot: multiply operator definition", {
  multiply <- read_pkg_json("units", "operators", "multiply.json")
  expect_snapshot({
    cat("id:", multiply$id, "\n")
    cat("fun:", multiply$fun, "\n")
    cat("aliases:", paste(multiply$alias, collapse = ", "), "\n")
  })
})

test_that("all operator JSON files have character id, fun, and alias fields", {
  paths <- list_pkg_jsons("units/operators")

  results <- vapply(paths, function(p) {
    dat <- jsonlite::read_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    is.character(dat$id) && length(dat$id) == 1L &&
      is.character(dat$fun) && length(dat$fun) == 1L &&
      is.character(dat$alias) && length(dat$alias) >= 1L
  }, logical(1L))

  bad <- basename(paths)[!results]
  expect_snapshot({
    cat("all have valid id/fun/alias:", all(results), "\n")
    if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")
  })
})

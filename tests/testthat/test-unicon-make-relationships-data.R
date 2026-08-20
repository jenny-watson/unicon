## Tests for unicon_make_relationships_data -------------------------------------

## ---- shared test fixtures ---------------------------------------------------

make_divide_derived <- function() {
  tibble::tibble(
    id       = "speed",
    x        = "length",
    y        = "time",
    operator = "divide"
  )
}

make_multiply_derived <- function() {
  tibble::tibble(
    id       = "energy",
    x        = "force",
    y        = "length",
    operator = "multiply"
  )
}

make_mixed_derived <- function() {
  dplyr::bind_rows(
    make_divide_derived(),
    make_multiply_derived()
  )
}

## ---- return type & schema ---------------------------------------------------

test_that("unicon_make_relationships_data returns a data frame", {
  result <- unicon_make_relationships_data(make_divide_derived())
  expect_s3_class(result, "data.frame")
})

test_that("unicon_make_relationships_data output has columns id, x, y, operator", {
  result <- unicon_make_relationships_data(make_divide_derived())
  expect_named(result, c("id", "x", "y", "operator"), info = "correct column names")
})

## ---- divide relationships ---------------------------------------------------

test_that("divide input generates 4 distinct rows for one relationship", {
  result <- unicon_make_relationships_data(make_divide_derived())
  expect_equal(nrow(result), 4L, info = "4 rows from one divide relationship")
})

test_that("original divide row is preserved in output", {
  input  <- make_divide_derived()
  result <- unicon_make_relationships_data(input)
  expect_true(
    any(result$id == "speed" & result$x == "length" &
          result$y == "time" & result$operator == "divide"),
    info = "original divide row present"
  )
})

test_that("divide: rearranged rows include inverse and swapped forms", {
  result <- unicon_make_relationships_data(make_divide_derived())

  # id<->y swap, still divide
  expect_true(
    any(result$id == "time" & result$x == "length" &
          result$y == "speed" & result$operator == "divide"),
    info = "id/y swapped divide row present"
  )

  # id<->x swap, operator becomes multiply
  expect_true(
    any(result$id == "length" & result$operator == "multiply"),
    info = "id/x swapped becomes multiply"
  )

  # x/y both swapped, operator becomes multiply
  expect_true(
    any(result$operator == "multiply"),
    info = "at least one multiply row from divide input"
  )
})

## ---- multiply relationships -------------------------------------------------

test_that("multiply input generates 4 distinct rows for one relationship", {
  result <- unicon_make_relationships_data(make_multiply_derived())
  expect_equal(nrow(result), 4L, info = "4 rows from one multiply relationship")
})

test_that("original multiply row is preserved in output", {
  input  <- make_multiply_derived()
  result <- unicon_make_relationships_data(input)
  expect_true(
    any(result$id == "energy" & result$x == "force" &
          result$y == "length" & result$operator == "multiply"),
    info = "original multiply row present"
  )
})

test_that("multiply: x and y swap is present in output", {
  result <- unicon_make_relationships_data(make_multiply_derived())
  expect_true(
    any(result$id == "energy" & result$x == "length" &
          result$y == "force" & result$operator == "multiply"),
    info = "x/y swapped multiply row present"
  )
})

test_that("multiply: derived divide forms are present in output", {
  result <- unicon_make_relationships_data(make_multiply_derived())
  expect_true(
    any(result$operator == "divide"),
    info = "at least one divide row from multiply input"
  )
})

## ---- mixed inputs -----------------------------------------------------------

test_that("mixed divide+multiply input returns distinct rows from both", {
  result <- unicon_make_relationships_data(make_mixed_derived())
  # 4 from divide + 4 from multiply = 8 distinct
  expect_equal(nrow(result), 8L, info = "8 distinct rows from two relationships")
})

test_that("output contains no duplicate rows", {
  result <- unicon_make_relationships_data(make_mixed_derived())
  expect_equal(nrow(result), nrow(dplyr::distinct(result)), info = "no duplicate rows")
})

## ---- unsupported operator warning -------------------------------------------

test_that("a warning is issued for unsupported operator types", {
  bad_operator <- tibble::tibble(
    id       = "odd",
    x        = "length",
    y        = "time",
    operator = "modulo"
  )
  expect_warning(
    unicon_make_relationships_data(bad_operator),
    "Relationship could not be derived as not a multiply or divide operator"
  )
})

## ---- empty input ------------------------------------------------------------

test_that("empty derived input returns an empty data frame with correct columns", {
  empty_input <- tibble::tibble(
    id       = character(0),
    x        = character(0),
    y        = character(0),
    operator = character(0)
  )
  result <- unicon_make_relationships_data(empty_input)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L, info = "empty input yields empty output")
  expect_named(result, c("id", "x", "y", "operator"))
})

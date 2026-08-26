## Tests for unicon_full -------------------------------------------------------
## Covers argument validation, scalar/vector recycling, alias normalisation,
## pull = TRUE / pull = FALSE output.

## ---- argument validation ----------------------------------------------------

test_that("unicon_full rejects non-numeric value_in", {
  expect_error(
    unicon_full("1", "m", "cm"),
    "Argument `value_in` must be numeric\\."
  )
})

test_that("unicon_full rejects non-character unit_in", {
  expect_error(
    unicon_full(1, 2, "cm"),
    "Argument `unit_in` must be a character vector\\."
  )
})

test_that("unicon_full rejects wrong-length unit_in", {
  expect_error(
    unicon_full(1:2, c("m", "cm", "km"), "cm"),
    "Argument `unit_in` must have length 1 or length\\(value_in\\)\\."
  )
})

test_that("unicon_full rejects non-character unit_out", {
  expect_error(
    unicon_full(1, "m", TRUE),
    "Argument `unit_out` must be a character vector or `NA`\\."
  )
})

test_that("unicon_full rejects wrong-length unit_out", {
  expect_error(
    unicon_full(1:3, "m", c("cm", "mm")),
    "Argument `unit_out` must have length 1 or length\\(value_in\\)\\."
  )
})

test_that("unicon_full rejects zero-length value_in", {
  expect_error(
    unicon_full(numeric(), "m", "cm"),
    "Argument `value_in` must have length >= 1\\."
  )
})

## ---- basic conversions ------------------------------------------------------

test_that("unicon_full scalar recycling works correctly", {
  expect_equal(unicon_full(c(1, 2), "m", "cm"), c(100, 200))
})

test_that("unicon_full normalises whitespace and case in aliases", {
  expect_equal(
    unicon_full(
      c(1, 2),
      c(" Metres ", " CENTImEtres "),
      c(" CEntiMetres ", "  M  ")
    ),
    c(100, 0.02)
  )
})

test_that("unicon_full returns full table with pull = FALSE", {
  out <- unicon_full(c(1, 2), "m", "cm", pull = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_named(out, c(
    "unit_in", "unit_out", "alias_in", "alias_out", "id_in", "srp_in",
    "id_out", "error_in", "error_srp", "error_out", "value_in",
    "value_srp", "value_out"
  ))
})

## ---- error / NA propagation -------------------------------------------------

test_that("unicon_full warns and returns NA for unknown unit", {
  expect_warning(
    out <- unicon_full(1, "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\."
  )
  expect_true(out$error_in)
  expect_true(is.na(out$value_out))
})

test_that("unicon_full warns and returns NA for mismatched unit types", {
  expect_warning(
    out <- unicon_full(1, "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\."
  )
  expect_true(out$error_srp)
  expect_true(is.na(out$value_out))
})

test_that("unicon_full falls back to SRP when unit_out is entirely missing", {
  msgs <- capture.output(
    out <- unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE),
    type = "message"
  )
  expect_match(
    paste(msgs, collapse = "\n"),
    "No output unit given. Converting all values to standard reference unit."
  )
  expect_equal(out$id_out, c("m", "g"))
  expect_equal(out$value_out, c(1, 1000))
})

test_that("unicon_full falls back to SRP for partially missing unit_out", {
  msgs <- capture.output(
    out <- unicon_full(c(100, 1), c("cm", "kg"), c(NA, "g"), pull = FALSE),
    type = "message"
  )
  expect_match(
    paste(msgs, collapse = "\n"),
    "Output unit missing in some cases. Converting to standard reference unit where missing."
  )
  expect_equal(out$id_out, c("m", "g"))
  expect_equal(out$value_out, c(1, 1000))
})

## ---- temperature / intercept conversions ------------------------------------

test_that("unicon_full celsius to fahrenheit uses intercept correctly", {
  # 0°C = 32°F, 100°C = 212°F, -40°C = -40°F (crossover point)
  out <- unicon_full(c(0, 100, -40), "celsius", "fahrenheit")
  expect_equal(out, c(32, 212, -40), tolerance = 0.01)
})

test_that("unicon_full fahrenheit to celsius uses intercept correctly", {
  # 32°F = 0°C, 212°F = 100°C, -40°F = -40°C
  out <- unicon_full(c(32, 212, -40), "fahrenheit", "celsius")
  expect_equal(out, c(0, 100, -40), tolerance = 0.01)
})

test_that("unicon_full celsius to kelvin uses intercept correctly", {
  # 0°C = 273.15 K, 100°C = 373.15 K, -273.15°C = 0 K (absolute zero)
  out <- unicon_full(c(0, 100, -273.15), "celsius", "kelvin")
  expect_equal(out, c(273.15, 373.15, 0), tolerance = 0.001)
})

test_that("unicon_full kelvin to celsius uses intercept correctly", {
  # 273.15 K = 0°C, 373.15 K = 100°C
  out <- unicon_full(c(273.15, 373.15), "kelvin", "celsius")
  expect_equal(out, c(0, 100), tolerance = 0.001)
})

test_that("unicon_full fahrenheit to kelvin chain uses intercept correctly", {
  # 32°F = 273.15 K, 212°F = 373.15 K
  out <- unicon_full(c(32, 212), "fahrenheit", "kelvin")
  expect_equal(out, c(273.15, 373.15), tolerance = 0.01)
})

test_that("unicon_full temperature round-trip celsius->fahrenheit->celsius", {
  original <- c(0, 37, 100, -40)
  via_f <- unicon_full(original, "celsius", "fahrenheit")
  back <- unicon_full(via_f, "fahrenheit", "celsius")
  expect_equal(back, original, tolerance = 0.01)
})

test_that("unicon_full temperature round-trip celsius->kelvin->celsius", {
  original <- c(0, 37, 100)
  via_k <- unicon_full(original, "celsius", "kelvin")
  back <- unicon_full(via_k, "kelvin", "celsius")
  expect_equal(back, original, tolerance = 0.001)
})

test_that("unicon_full temperature SRP fallback returns celsius values", {
  # When no unit_out given, SRP for temperature is Celsius
  msgs <- capture.output(
    out <- unicon_full(c(32, 212), "fahrenheit", pull = FALSE),
    type = "message"
  )
  expect_equal(out$id_out, c("C", "C"))
  expect_equal(out$value_out, c(0, 100), tolerance = 0.01)
})


test_that("unicon_full fahrenheit to celsius uses intercept correctly", {
  # 32°F = 0°C, 212°F = 100°C, -40°F = -40°C
  out <- unicon_full(c(32, 212, -40), "fahrenheit", "celsius")
  expect_equal(out, c(0, 100, -40), tolerance = 0.01)
})

test_that("unicon_full celsius to kelvin uses intercept correctly", {
  # 0°C = 273.15 K, 100°C = 373.15 K, -273.15°C = 0 K (absolute zero)
  out <- unicon_full(c(0, 100, -273.15), "celsius", "kelvin")
  expect_equal(out, c(273.15, 373.15, 0), tolerance = 0.001)
})

test_that("unicon_full kelvin to celsius uses intercept correctly", {
  # 273.15 K = 0°C, 373.15 K = 100°C
  out <- unicon_full(c(273.15, 373.15), "kelvin", "celsius")
  expect_equal(out, c(0, 100), tolerance = 0.001)
})

test_that("unicon_full fahrenheit to kelvin chain uses intercept correctly", {
  # 32°F = 273.15 K, 212°F = 373.15 K
  out <- unicon_full(c(32, 212), "fahrenheit", "kelvin")
  expect_equal(out, c(273.15, 373.15), tolerance = 0.01)
})

test_that("unicon_full temperature round-trip celsius->fahrenheit->celsius", {
  original <- c(0, 37, 100, -40)
  via_f <- unicon_full(original, "celsius", "fahrenheit")
  back <- unicon_full(via_f, "fahrenheit", "celsius")
  expect_equal(back, original, tolerance = 0.01)
})

test_that("unicon_full temperature round-trip celsius->kelvin->celsius", {
  original <- c(0, 37, 100)
  via_k <- unicon_full(original, "celsius", "kelvin")
  back <- unicon_full(via_k, "kelvin", "celsius")
  expect_equal(back, original, tolerance = 0.001)
})

test_that("unicon_full temperature SRP fallback returns celsius values", {
  # When no unit_out given, SRP for temperature is Celsius
  msgs <- capture.output(
    out <- unicon_full(c(32, 212), "fahrenheit", pull = FALSE),
    type = "message"
  )
  expect_equal(out$id_out, c("C", "C"))
  expect_equal(out$value_out, c(0, 100), tolerance = 0.01)
})

## ---- row-count preservation and duplicate rows ------------------------------

test_that("unicon_full output row count equals input length (valid units)", {
  values <- c(1, 2, 3, 4, 5)
  out <- unicon_full(values, "m", "cm", pull = FALSE)
  expect_equal(nrow(out), length(values))
})

test_that("unicon_full output row count equals input length with unknown unit", {
  expect_warning(
    out <- unicon_full(c(1, 2, 3), "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\."
  )
  expect_equal(nrow(out), 3L)
})

test_that("unicon_full returns NA for unrecognised unit, not fewer rows", {
  expect_warning(
    out <- unicon_full(c(1, 2), "not-a-unit", "cm", pull = FALSE),
    "Some input units failed to find matches\\."
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(is.na(out$value_out)))
})

test_that("unicon_full returns NA for mismatched unit type, not fewer rows", {
  expect_warning(
    out <- unicon_full(c(1, 2), "m", "g", pull = FALSE),
    "Some requested conversions were not valid \\(unit type mismatch\\)\\."
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(is.na(out$value_out)))
})

test_that("unicon_full mixed valid/invalid rows returns NA not fewer rows", {
  expect_warning(
    out <- unicon_full(
      c(1, 2, 3),
      c("m", "not-a-unit", "km"),
      "m",
      pull = FALSE
    ),
    "Some input units failed to find matches\\."
  )
  expect_equal(nrow(out), 3L)
  expect_false(is.na(out$value_out[1]))
  expect_true(is.na(out$value_out[2]))
  expect_false(is.na(out$value_out[3]))
})

test_that("unicon_full handles duplicated rows correctly", {
  values <- c(1, 1, 2, 2)
  out <- unicon_full(values, "m", "cm", pull = FALSE)
  expect_equal(nrow(out), 4L)
  expect_equal(out$value_out, c(100, 100, 200, 200))
})

test_that("unicon_full all-duplicate inputs returns same number of rows", {
  values <- rep(5, 10)
  out <- unicon_full(values, "km", "m", pull = FALSE)
  expect_equal(nrow(out), 10L)
  expect_true(all(out$value_out == 5000))
})

test_that("unicon_full pull = TRUE returns vector of same length as input", {
  values <- c(1, 2, 3)
  out <- unicon_full(values, "m", "cm")
  expect_equal(length(out), length(values))
  expect_type(out, "double")
})

test_that("unicon_full pull = TRUE same length with duplicate values", {
  values <- c(1, 1, 1)
  out <- unicon_full(values, "m", "cm")
  expect_equal(length(out), 3L)
  expect_equal(out, c(100, 100, 100))
})

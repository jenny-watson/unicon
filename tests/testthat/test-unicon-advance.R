advance_srp <- function(value, unit) {
  suppressWarnings(suppressMessages(
    unicon_full(value_in = value, unit_in = unit, unit_out = NA)
  ))
}

advance_expected <- function(value_x, unit_x, value_y, unit_y, operator) {
  srp_x <- advance_srp(value_x, unit_x)
  srp_y <- advance_srp(value_y, unit_y)

  if (identical(operator, "divide")) {
    srp_x / srp_y
  } else {
    srp_x * srp_y
  }
}

test_that("basic functionality works for scalar and vectorized speed calculations", {
  scalar <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = "mile/hour"
  )

  expect_type(scalar, "double")
  expect_length(scalar, 1L)
  expect_equal(scalar, 50, tolerance = 1e-8)

  vectorized <- unicon_advance(
    x_unit_in = c("miles", "km", "m"),
    y_unit_in = c("hour", "hour", "sec"),
    x_value_in = c(100, 10, 5),
    y_value_in = c(2, 0.5, 2),
    unit_out = c("km/hour", "km/hour", "m/sec")
  )

  expect_type(vectorized, "double")
  expect_equal(
    vectorized,
    c(
      suppressWarnings(suppressMessages(unicon_full(50, "mile/hour", "km/hour"))),
      20,
      2.5
    ),
    tolerance = 1e-8
  )
})

test_that("pull controls whether values or full workings are returned", {
  pulled <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = c(10, 20),
    y_value_in = c(2, 4),
    unit_out = "km/day",
    pull = TRUE
  )

  full <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = c(10, 20),
    y_value_in = c(2, 4),
    unit_out = "km/day",
    pull = FALSE
  )

  expect_type(pulled, "double")
  expect_s3_class(full, "data.frame")

  # Check the key columns the output must contain
  expect_true(all(c(
    "x_category", "x_unit_in", "x_value_in", "x_value_srp",
    "y_category", "y_unit_in", "y_value_in", "y_value_srp",
    "operator_in", "category", "unit_out", "value_out"
  ) %in% names(full)))

  expect_equal(full$value_out, pulled)
})

test_that("x_value_in and y_value_in must have the same length", {
  expect_error(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1),
      unit_out = "km/hour"
    ),
    "Argument `x_value_in` and `y_value_in` must have same length"
  )
})

test_that("documented relationships resolve to the correct derived categories", {
  cases <- list(
    list(10, 2, "kg", "ha", "area_density", "divide"),
    list(10, 2, "l", "m", "area", "divide"),
    list(2, 4, "mol", "l", "molar_concentration", "divide"),
    list(4, 2, "ha", "m", "length", "divide"),
    list(2, 2, "kg", "kg", "mass_fraction", "divide"),
    list(10, 2, "N", "ha", "pressure", "divide"),
    list(10, 2, "kg", "l", "volume_density", "divide"),
    list(2, 4, "l", "l", "volume_fraction", "divide"),
    list(100, 2, "miles", "hour", "speed", "divide")
  )

  for (case in cases) {
    result <- unicon_advance(
      x_value_in = case[[1]],
      y_value_in = case[[2]],
      x_unit_in = case[[3]],
      y_unit_in = case[[4]],
      unit_out = NA,
      operator_in = case[[6]],
      pull = FALSE
    )

    # id holds the derived category
    expect_equal(result$category, case[[5]])

    # value_in is the srp_value_out (the intermediate SRP value before final conversion)
    expect_equal(
      result$value_in,
      advance_expected(case[[1]], case[[3]], case[[2]], case[[4]], case[[6]]),
      tolerance = 1e-8
    )
  }
})

test_that("invalid parent relationships are handled", {
  expect_error(
    unicon_advance(
      x_unit_in = "kg",
      y_unit_in = "celsius",
      x_value_in = 1,
      y_value_in = 1,
      unit_out = NA
    ),
    "There is no recorded relationship between parent units"
  )
})

test_that("multiply, divide, ambiguity and invalid operator cases are covered", {
  multiply_result <- unicon_advance(
    x_unit_in = "ha",
    y_unit_in = "m",
    x_value_in = 2,
    y_value_in = 3,
    unit_out = "l",
    operator_in = "multiply",
    pull = FALSE
  )

  divide_result <- unicon_advance(
    x_unit_in = "ha",
    y_unit_in = "m",
    x_value_in = 2,
    y_value_in = 4,
    unit_out = NA,
    operator_in = "divide",
    pull = FALSE
  )

  expect_equal(multiply_result$category, "volume")
  expect_equal(multiply_result$value_out, 6, tolerance = 1e-8)
  expect_equal(divide_result$category, "length")

  # value_in holds the derived SRP value
  expect_equal(divide_result$value_in, 0.5, tolerance = 1e-8)

  expect_error(
    unicon_advance(
      x_unit_in = "ha",
      y_unit_in = "m",
      x_value_in = 2,
      y_value_in = 4,
      unit_out = NA
    ),
    "Please specify `operator_in`"
  )

  expect_error(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = 100,
      y_value_in = 2,
      unit_out = "km/hour",
      operator_in = "multiply"
    ),
    "`operator_in` does not match the relationship derived between parent units"
  )
})

test_that("unit_out validation and alias normalization behave as expected", {
  converted <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = "km/hour"
  )

  expect_equal(
    converted,
    suppressWarnings(suppressMessages(unicon_full(50, "mile/hour", "km/hour"))),
    tolerance = 1e-8
  )

  expect_error(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = 100,
      y_value_in = 2,
      unit_out = "km"
    ),
    "`unit_out` does not exist for the relationship derived between parent units"
  )

  # When unit_out = NA, the output value equals the intermediate SRP value
  srp_default <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = NA,
    pull = FALSE
  )

  expect_equal(srp_default$value_out, srp_default$value_in, tolerance = 1e-8)

  alias_result <- unicon_advance(
    x_unit_in = " Miles ",
    y_unit_in = " HOUR ",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = " km / hour "
  )

  expect_equal(alias_result, converted, tolerance = 1e-8)
})

test_that("calculation accuracy matches known values and vignette examples", {
  speed_mph <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = "mile/hour"
  )

  speed_kmh <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = "km/hour"
  )

  workings <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = "km/hour",
    pull = FALSE
  )

  vignette_example <- unicon_advance(
    x_unit_in = rep("miles", 6),
    y_unit_in = rep("hour", 6),
    x_value_in = 1:6,
    y_value_in = c(9, 8, 7, 5, 4, 2),
    unit_out = rep("km/day", 6)
  )

  expected_vignette <- suppressWarnings(suppressMessages(unicon_full(
    value_in = (1:6) / c(9, 8, 7, 5, 4, 2),
    unit_in = rep("mile/hour", 6),
    unit_out = rep("km/day", 6)
  )))

  # mass_fraction requires operator_in = "divide" to disambiguate
  mass_fraction <- unicon_advance(
    x_unit_in = "kg",
    y_unit_in = "kg",
    x_value_in = 7,
    y_value_in = 7,
    unit_out = NA,
    operator_in = "divide"
  )

  round_trip <- suppressWarnings(suppressMessages(unicon_full(
    value_in = speed_kmh,
    unit_in = "km/hour",
    unit_out = "mile/hour"
  )))

  expect_equal(speed_mph, 50, tolerance = 1e-8)
  expect_equal(speed_kmh, 80.4672, tolerance = 1e-3)
  expect_equal(workings$x_value_srp, advance_srp(100, "miles"), tolerance = 1e-8)
  expect_equal(workings$y_value_srp, advance_srp(2, "hour"), tolerance = 1e-8)
  expect_equal(vignette_example, expected_vignette, tolerance = 1e-8)
  expect_equal(mass_fraction, 1, tolerance = 1e-8)
  expect_equal(round_trip, speed_mph, tolerance = 1e-8)
})

test_that("edge cases and integration paths are covered", {
  zero_numerator <- unicon_advance(0, 5, "m", "sec", unit_out = NA)
  zero_denominator <- unicon_advance(5, 0, "m", "sec", unit_out = NA)
  negative_values <- unicon_advance(-10, 2, "m", "sec", unit_out = NA)
  large_values <- unicon_advance(1e12, 1e-6, "m", "sec", unit_out = NA)
  small_values <- unicon_advance(1e-12, 1e6, "m", "sec", unit_out = NA)
  single_value <- unicon_advance(4, 2, "m", "sec", unit_out = NA)

  expect_equal(zero_numerator, 0, tolerance = 1e-8)
  expect_true(is.infinite(zero_denominator))
  expect_equal(negative_values, -5, tolerance = 1e-8)
  expect_equal(large_values, 1e18, tolerance = 1e-8)
  expect_equal(small_values, 1e-18, tolerance = 1e-30)
  expect_error(unicon_advance(
    c(1, NA, NaN),
    c(1, 1, 1),
    "m",
    "sec",
    unit_out = NA
  ))
  expect_equal(single_value, 2, tolerance = 1e-8)

  # value_in in pull=FALSE output holds the intermediate SRP value
  direct_srp <- advance_expected(100, "miles", 2, "hour", "divide")
  advance_result <- unicon_advance(
    100,
    2,
    "miles",
    "hour",
    unit_out = NA,
    pull = FALSE
  )
  expect_equal(advance_result$value_in, direct_srp, tolerance = 1e-8)

  mixed_systems <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "sec",
    x_value_in = 1,
    y_value_in = 60,
    unit_out = "km/hour"
  )

  expect_equal(
    mixed_systems,
    suppressWarnings(suppressMessages(unicon_full(1 / 60, "mile/sec", "km/hour"))),
    tolerance = 1e-8
  )
})

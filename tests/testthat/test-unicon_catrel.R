catrel_srp <- function(value, unit) {
  suppressMessages(unicon_full(value_in = value, unit_in = unit, unit_out = NA))
}

catrel_expected <- function(value_1, unit_1, value_2, unit_2, operator) {
  srp_1 <- catrel_srp(value_1, unit_1)
  srp_2 <- catrel_srp(value_2, unit_2)

  if (identical(operator, "divide")) {
    srp_1 / srp_2
  } else {
    srp_1 * srp_2
  }
}

test_that("basic functionality works for scalar and vectorized speed calculations", {
  scalar <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = "mile/hour"
  )

  expect_type(scalar, "double")
  expect_length(scalar, 1L)
  expect_equal(scalar, 50, tolerance = 1e-8)

  vectorised <- unicon_catrel(
    parent_1_unit_in = c("miles", "km", "m"),
    parent_2_unit_in = c("hour", "hour", "sec"),
    parent_1_value_in = c(100, 10, 5),
    parent_2_value_in = c(2, 0.5, 2),
    unit_out = c("km/hour", "km/hour", "m/sec")
  )

  expect_type(vectorised, "double")
  expect_equal(
    vectorised,
    c(
      suppressMessages(unicon_full(50, "mile/hour", "km/hour")),
      20,
      2.5
    ),
    tolerance = 1e-8
  )
})

test_that("pull controls whether values or full workings are returned", {
  pulled <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = c(10, 20),
    parent_2_value_in = c(2, 4),
    unit_out = "km/day",
    pull = TRUE
  )

  full <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = c(10, 20),
    parent_2_value_in = c(2, 4),
    unit_out = "km/day",
    pull = FALSE
  )

  expect_type(pulled, "double")
  expect_s3_class(full, "data.frame")
  expect_named(full, c(
    "parent_1_unit_in", "parent_1_value_in", "parent_1_category",
    "parent_1_srp_unit", "parent_1_srp_value", "parent_2_unit_in",
    "parent_2_value_in", "parent_2_category", "parent_2_srp_unit",
    "parent_2_srp_value", "operator", "category_out", "srp_unit_out",
    "srp_value_out", "unit_out", "value_out"
  ))
  expect_equal(full$value_out, pulled)
})

test_that("argument lengths are validated and scalar inputs are recycled", {
  expect_error(
    unicon_catrel(
      parent_1_unit_in = c("miles", "km"),
      parent_2_unit_in = c("hour", "hour", "hour"),
      parent_1_value_in = c(1, 2, 3),
      parent_2_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for parent_1_unit_in argument incompatible"
  )

  expect_error(
    unicon_catrel(
      parent_1_unit_in = c("miles", "miles", "miles"),
      parent_2_unit_in = c("hour", "hour"),
      parent_1_value_in = c(1, 2, 3),
      parent_2_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for parent_2_unit_in argument incompatible"
  )

  expect_error(
    unicon_catrel(
      parent_1_unit_in = c("miles", "miles", "miles"),
      parent_2_unit_in = c("hour", "hour", "hour"),
      parent_1_value_in = c(1, 2),
      parent_2_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for parent_1_value_in argument incompatible"
  )

  expect_error(
    unicon_catrel(
      parent_1_unit_in = c("miles", "miles", "miles"),
      parent_2_unit_in = c("hour", "hour", "hour"),
      parent_1_value_in = c(1, 2, 3),
      parent_2_value_in = c(1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for parent_2_value_in argument incompatible"
  )

  expect_error(
    unicon_catrel(
      parent_1_unit_in = c("miles", "miles", "miles"),
      parent_2_unit_in = c("hour", "hour", "hour"),
      parent_1_value_in = c(1, 2, 3),
      parent_2_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour")
    ),
    "Length for unit_out argument incompatible"
  )

  expect_error(
    unicon_catrel("miles", "hour", numeric(), 1, "km/hour"),
    "Length for parent_1_value_in argument must be >= 1L"
  )

  expect_error(
    unicon_catrel("miles", "hour", 1, numeric(), "km/hour"),
    "Length for parent_2_value_in argument must be >= 1L"
  )

  recycled <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = c(100, 200, 300),
    parent_2_value_in = 2,
    unit_out = "km/hour"
  )

  expect_equal(
    recycled,
    c(
      suppressMessages(unicon_full(50, "mile/hour", "km/hour")),
      suppressMessages(unicon_full(100, "mile/hour", "km/hour")),
      suppressMessages(unicon_full(150, "mile/hour", "km/hour"))
    ),
    tolerance = 1e-8
  )
})

test_that("documented relationships resolve to the correct derived categories", {
  cases <- list(
    list("kg", "ha", 10, 2, "area_density", "divide"),
    list("l", "m", 10, 2, "area", "divide"),
    list("mol", "l", 2, 4, "concentration", "divide"),
    list("ha", "m", 4, 2, "length", "divide"),
    list("kg", "kg", 2, 2, "mass_fraction", "divide"),
    list("N", "ha", 10, 2, "pressure", "divide"),
    list("kg", "l", 10, 2, "volume_density", "divide"),
    list("l", "l", 2, 4, "volume_fraction", "divide"),
    list("miles", "hour", 100, 2, "speed", "divide")
  )

  srp_lookup <- get("unit_srp", envir = asNamespace("unicon"))

  for (case in cases) {
    result <- unicon_catrel(
      parent_1_unit_in = case[[1]],
      parent_2_unit_in = case[[2]],
      parent_1_value_in = case[[3]],
      parent_2_value_in = case[[4]],
      unit_out = NA,
      operator_in = case[[6]],
      pull = FALSE
    )

    expect_equal(result$category_out, case[[5]])
    expect_equal(
      result$srp_value_out,
      catrel_expected(case[[3]], case[[1]], case[[4]], case[[2]], case[[6]]),
      tolerance = 1e-8
    )
    expect_equal(
      result$srp_unit_out,
      unique(srp_lookup$srp[srp_lookup$category == case[[5]]])[1]
    )
  }
})

test_that("invalid and swapped parent relationships are handled", {
  expect_error(
    unicon_catrel(
      parent_1_unit_in = "kg",
      parent_2_unit_in = "celsius",
      parent_1_value_in = 1,
      parent_2_value_in = 1,
      unit_out = NA
    ),
    "There is no recorded relationship between parent units"
  )

  normal <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = "km/hour"
  )

  swapped <- unicon_catrel(
    parent_1_unit_in = "hour",
    parent_2_unit_in = "miles",
    parent_1_value_in = 2,
    parent_2_value_in = 100,
    unit_out = "km/hour"
  )

  expect_equal(swapped, normal, tolerance = 1e-8)
})

test_that("multiply, divide, ambiguity and invalid operator cases are covered", {
  multiply_result <- unicon_catrel(
    parent_1_unit_in = "ha",
    parent_2_unit_in = "m",
    parent_1_value_in = 2,
    parent_2_value_in = 3,
    unit_out = "l",
    operator_in = "multiply",
    pull = FALSE
  )

  divide_result <- unicon_catrel(
    parent_1_unit_in = "ha",
    parent_2_unit_in = "m",
    parent_1_value_in = 2,
    parent_2_value_in = 4,
    unit_out = NA,
    operator_in = "divide",
    pull = FALSE
  )

  expect_equal(multiply_result$category_out, "volume")
  expect_equal(multiply_result$value_out, 60000, tolerance = 1e-8)
  expect_equal(divide_result$category_out, "length")
  expect_equal(divide_result$srp_value_out, 0.5, tolerance = 1e-8)

  expect_error(
    unicon_catrel(
      parent_1_unit_in = "ha",
      parent_2_unit_in = "m",
      parent_1_value_in = 2,
      parent_2_value_in = 4,
      unit_out = NA
    ),
    "Please specify operator_in"
  )

  expect_error(
    unicon_catrel(
      parent_1_unit_in = "miles",
      parent_2_unit_in = "hour",
      parent_1_value_in = 100,
      parent_2_value_in = 2,
      unit_out = "km/hour",
      operator_in = "multiply"
    ),
    "operator_in does not match the relationship derived between parent units"
  )
})

test_that("unit_out validation and alias normalization behave as expected", {
  converted <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = "km/hour"
  )

  expect_equal(
    converted,
    suppressMessages(unicon_full(50, "mile/hour", "km/hour")),
    tolerance = 1e-8
  )

  expect_error(
    unicon_catrel(
      parent_1_unit_in = "miles",
      parent_2_unit_in = "hour",
      parent_1_value_in = 100,
      parent_2_value_in = 2,
      unit_out = "km"
    ),
    "unit_out does not exist for the relationship derived between parent units"
  )

  srp_default <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = NA,
    pull = FALSE
  )

  expect_equal(srp_default$unit_out, srp_default$srp_unit_out)
  expect_equal(srp_default$value_out, srp_default$srp_value_out, tolerance = 1e-8)

  alias_result <- unicon_catrel(
    parent_1_unit_in = " Miles ",
    parent_2_unit_in = " HOUR ",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = " km / hour "
  )

  expect_equal(alias_result, converted, tolerance = 1e-8)
})

test_that("calculation accuracy matches known values and vignette examples", {
  speed_mph <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = "mile/hour"
  )

  speed_kmh <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = "km/hour"
  )

  workings <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "hour",
    parent_1_value_in = 100,
    parent_2_value_in = 2,
    unit_out = "km/hour",
    pull = FALSE
  )

  vignette_example <- unicon_catrel(
    parent_1_unit_in = rep("miles", 6),
    parent_2_unit_in = rep("hour", 6),
    parent_1_value_in = 1:6,
    parent_2_value_in = c(9, 8, 7, 5, 4, 2),
    unit_out = rep("km/day", 6)
  )

  expected_vignette <- suppressMessages(unicon_full(
    value_in = (1:6) / c(9, 8, 7, 5, 4, 2),
    unit_in = rep("mile/hour", 6),
    unit_out = rep("km/day", 6)
  ))

  mass_fraction <- unicon_catrel(
    parent_1_unit_in = "kg",
    parent_2_unit_in = "kg",
    parent_1_value_in = 7,
    parent_2_value_in = 7,
    unit_out = NA
  )

  round_trip <- suppressMessages(unicon_full(
    value_in = speed_kmh,
    unit_in = "km/hour",
    unit_out = "mile/hour"
  ))

  expect_equal(speed_mph, 50, tolerance = 1e-8)
  expect_equal(speed_kmh, 80.4672, tolerance = 1e-3)
  expect_equal(workings$parent_1_srp_value, catrel_srp(100, "miles"), tolerance = 1e-8)
  expect_equal(workings$parent_2_srp_value, catrel_srp(2, "hour"), tolerance = 1e-8)
  expect_equal(vignette_example, expected_vignette, tolerance = 1e-8)
  expect_equal(mass_fraction, 1, tolerance = 1e-8)
  expect_equal(round_trip, speed_mph, tolerance = 1e-8)
})

test_that("edge cases and integration paths are covered", {
  zero_numerator <- unicon_catrel("m", "sec", 0, 5, unit_out = NA)
  zero_denominator <- unicon_catrel("m", "sec", 5, 0, unit_out = NA)
  negative_values <- unicon_catrel("m", "sec", -10, 2, unit_out = NA)
  large_values <- unicon_catrel("m", "sec", 1e12, 1e-6, unit_out = NA)
  small_values <- unicon_catrel("m", "sec", 1e-12, 1e6, unit_out = NA)
  missing_values <- unicon_catrel(
    "m",
    "sec",
    c(1, NA, NaN),
    c(1, 1, 1),
    unit_out = NA
  )
  single_value <- unicon_catrel("m", "sec", 4, 2, unit_out = NA)

  expect_equal(zero_numerator, 0, tolerance = 1e-8)
  expect_true(is.infinite(zero_denominator))
  expect_equal(negative_values, -5, tolerance = 1e-8)
  expect_equal(large_values, 1e18, tolerance = 1e-8)
  expect_equal(small_values, 1e-18, tolerance = 1e-30)
  expect_true(is.na(missing_values[2]))
  expect_true(is.nan(missing_values[3]))
  expect_equal(single_value, 2, tolerance = 1e-8)

  expect_error(
    unicon_catrel("not_a_unit", "sec", 1, 1, unit_out = NA),
    "Some parent_1_unit_in values failed to find matches"
  )

  expect_error(
    unicon_catrel("m", "not_a_unit", 1, 1, unit_out = NA),
    "Some parent_2_unit_in values failed to find matches"
  )

  direct_srp <- catrel_expected(100, "miles", 2, "hour", "divide")
  catrel_srp_value <- unicon_catrel(
    "miles",
    "hour",
    100,
    2,
    unit_out = NA,
    pull = FALSE
  )
  expect_equal(catrel_srp_value$srp_value_out, direct_srp, tolerance = 1e-8)

  area <- unicon_catrel(
    parent_1_unit_in = "l",
    parent_2_unit_in = "m",
    parent_1_value_in = 12,
    parent_2_value_in = 3,
    unit_out = "l__m",
    operator_in = "divide"
  )
  volume <- unicon_catrel(
    parent_1_unit_in = "l__m",
    parent_2_unit_in = "m",
    parent_1_value_in = area,
    parent_2_value_in = 3,
    unit_out = "l",
    operator_in = "multiply"
  )
  mixed_systems <- unicon_catrel(
    parent_1_unit_in = "miles",
    parent_2_unit_in = "sec",
    parent_1_value_in = 1,
    parent_2_value_in = 60,
    unit_out = "km/hour"
  )

  expect_equal(volume, 12, tolerance = 1e-8)
  expect_equal(
    mixed_systems,
    suppressMessages(unicon_full(1 / 60, "mile/sec", "km/hour")),
    tolerance = 1e-8
  )
})

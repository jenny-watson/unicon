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
    y_value_in = y,
    unit_out = "mile/hour"
  )

  expect_type(scalar, "double", info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=mile/hour")
  expect_length(scalar, 1L, info = "x_unit_in=miles, y_unit_in=hour, unit_out=mile/hour")
  expect_equal(scalar, 50, tolerance = 1e-8, info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=mile/hour")

  vectorized <- unicon_advance(
    x_unit_in = c("miles", "km", "m"),
    y_unit_in = c("hour", "hour", "sec"),
    x_value_in = c(100, 10, 5),
    y_value_in = c(2, 0.5, 2),
    unit_out = c("km/hour", "km/hour", "m/sec")
  )

  expect_type(
    vectorized,
    "double",
    info = "x_unit_in=miles|km|m, y_unit_in=hour|hour|sec, unit_out=km/hour|km/hour|m/sec"
  )
  expect_equal(
    vectorized,
    c(
      suppressWarnings(suppressMessages(unicon_full(50, "mile/hour", "km/hour"))),
      20,
      2.5
    ),
    tolerance = 1e-8,
    info = "x_unit_in=miles|km|m, y_unit_in=hour|hour|sec, x_value_in=100|10|5, y_value_in=2|0.5|2, unit_out=km/hour|km/hour|m/sec"
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

  expect_type(pulled, "double", info = "pull=TRUE, x_unit_in=miles, y_unit_in=hour, unit_out=km/day")
  expect_s3_class(full, "data.frame", info = "pull=FALSE, x_unit_in=miles, y_unit_in=hour, unit_out=km/day")
  expect_named(full, c(
    "x_unit_in", "x_value_in", "x_category",
    "x_srp_unit", "x_srp_value", "y_unit_in",
    "y_value_in", "y_category", "y_srp_unit",
    "y_srp_value", "operator", "category_out", "srp_unit_out",
    "srp_value_out", "unit_out", "value_out"
  ), info = "dataset=unicon_advance_pull_false_columns")
  expect_equal(full$value_out, pulled, info = "compare=full$value_out_vs_pull_true, unit_out=km/day")
})

test_that("argument lengths are validated and scalar inputs are recycled", {
  expect_error(
    unicon_advance(
      x_unit_in = c("miles", "km"),
      y_unit_in = c("hour", "hour", "hour"),
      x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for x_unit_in argument incompatible",
    info = "x_unit_in_length=2, y_unit_in_length=3, x_value_in_length=3, y_value_in_length=3, unit_out_length=3"
  )

  expect_error(
    unicon_advance(
      x_unit_in = c("miles", "miles", "miles"),
      y_unit_in = c("hour", "hour"),
      x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for y_unit_in argument incompatible",
    info = "x_unit_in_length=3, y_unit_in_length=2, x_value_in_length=3, y_value_in_length=3, unit_out_length=3"
  )

  expect_error(
    unicon_advance(
      x_unit_in = c("miles", "miles", "miles"),
      y_unit_in = c("hour", "hour", "hour"),
      x_value_in = c(1, 2),
      y_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for x_value_in argument incompatible",
    info = "x_unit_in_length=3, y_unit_in_length=3, x_value_in_length=2, y_value_in_length=3, unit_out_length=3"
  )

  expect_error(
    unicon_advance(
      x_unit_in = c("miles", "miles", "miles"),
      y_unit_in = c("hour", "hour", "hour"),
      x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1),
      unit_out = c("km/hour", "km/hour", "km/hour")
    ),
    "Length for y_value_in argument incompatible",
    info = "x_unit_in_length=3, y_unit_in_length=3, x_value_in_length=3, y_value_in_length=2, unit_out_length=3"
  )

  expect_error(
    unicon_advance(
      x_unit_in = c("miles", "miles", "miles"),
      y_unit_in = c("hour", "hour", "hour"),
      x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1, 1),
      unit_out = c("km/hour", "km/hour")
    ),
    "Length for unit_out argument incompatible",
    info = "x_unit_in_length=3, y_unit_in_length=3, x_value_in_length=3, y_value_in_length=3, unit_out_length=2"
  )

  expect_error(
    unicon_advance("miles", "hour", numeric(), 1, "km/hour"),
    "Length for x_value_in argument must be >= 1L",
    info = "x_unit_in=miles, y_unit_in=hour, x_value_in_length=0, y_value_in=1, unit_out=km/hour"
  )

  expect_error(
    unicon_advance("miles", "hour", 1, numeric(), "km/hour"),
    "Length for y_value_in argument must be >= 1L",
    info = "x_unit_in=miles, y_unit_in=hour, x_value_in=1, y_value_in_length=0, unit_out=km/hour"
  )

  recycled <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = c(100, 200, 300),
    y_value_in = 2,
    unit_out = "km/hour"
  )

  expect_equal(
    recycled,
    c(
      suppressWarnings(suppressMessages(unicon_full(50, "mile/hour", "km/hour"))),
      suppressWarnings(suppressMessages(unicon_full(100, "mile/hour", "km/hour"))),
      suppressWarnings(suppressMessages(unicon_full(150, "mile/hour", "km/hour")))
    ),
    tolerance = 1e-8,
    info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100|200|300, y_value_in=2, unit_out=km/hour"
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
    result <- unicon_advance(
      x_unit_in = case[[1]],
      y_unit_in = case[[2]],
      x_value_in = case[[3]],
      y_value_in = case[[4]],
      unit_out = NA,
      operator_in = case[[6]],
      pull = FALSE
    )

    expect_equal(
      result$category_out,
      case[[5]],
      info = paste0(
        "x_unit_in=", case[[1]],
        ", y_unit_in=", case[[2]],
        ", x_value_in=", case[[3]],
        ", y_value_in=", case[[4]],
        ", operator_in=", case[[6]],
        ", category_expected=", case[[5]]
      )
    )
    expect_equal(
      result$srp_value_out,
      advance_expected(case[[3]], case[[1]], case[[4]], case[[2]], case[[6]]),
      tolerance = 1e-8,
      info = paste0(
        "x_unit_in=", case[[1]],
        ", y_unit_in=", case[[2]],
        ", x_value_in=", case[[3]],
        ", y_value_in=", case[[4]],
        ", operator_in=", case[[6]]
      )
    )
    expect_equal(
      result$srp_unit_out,
      unique(srp_lookup$srp[srp_lookup$category == case[[5]]])[1],
      info = paste0(
        "x_unit_in=", case[[1]],
        ", y_unit_in=", case[[2]],
        ", category_out=", case[[5]],
        ", operator_in=", case[[6]]
      )
    )
  }
})

test_that("invalid and swapped parent relationships are handled", {
  expect_error(
    unicon_advance(
      x_unit_in = "kg",
      y_unit_in = "celsius",
      x_value_in = 1,
      y_value_in = 1,
      unit_out = NA
    ),
    "There is no recorded relationship between parent units",
    info = "x_unit_in=kg, y_unit_in=celsius, x_value_in=1, y_value_in=1, unit_out=NA"
  )

  normal <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = "km/hour"
  )

  swapped <- unicon_advance(
    x_unit_in = "hour",
    y_unit_in = "miles",
    x_value_in = 2,
    y_value_in = 100,
    unit_out = NA
  )

  expect_equal(
    swapped,
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = 100,
      y_value_in = 2,
      unit_out = NA
    ),
    tolerance = 1e-8,
    info = "compare=swapped_vs_normalized, x_unit_in=hour, y_unit_in=miles, x_value_in=2, y_value_in=100, unit_out=NA"
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

  expect_equal(multiply_result$category_out, "volume", info = "x_unit_in=ha, y_unit_in=m, operator_in=multiply, unit_out=l")
  expect_equal(multiply_result$value_out, 6, tolerance = 1e-8, info = "x_unit_in=ha, y_unit_in=m, x_value_in=2, y_value_in=3, operator_in=multiply, unit_out=l")
  expect_equal(divide_result$category_out, "length", info = "x_unit_in=ha, y_unit_in=m, operator_in=divide, unit_out=NA")
  expect_equal(divide_result$srp_value_out, 0.5, tolerance = 1e-8, info = "x_unit_in=ha, y_unit_in=m, x_value_in=2, y_value_in=4, operator_in=divide, unit_out=NA")

  expect_error(
    unicon_advance(
      x_unit_in = "ha",
      y_unit_in = "m",
      x_value_in = 2,
      y_value_in = 4,
      unit_out = NA
    ),
    "Please specify operator_in",
    info = "x_unit_in=ha, y_unit_in=m, x_value_in=2, y_value_in=4, unit_out=NA"
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
    "operator_in does not match the relationship derived between parent units",
    info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, operator_in=multiply, unit_out=km/hour"
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
    tolerance = 1e-8,
    info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=km/hour"
  )

  expect_error(
    unicon_advance(
      x_unit_in = "miles",
      y_unit_in = "hour",
      x_value_in = 100,
      y_value_in = 2,
      unit_out = "km"
    ),
    "unit_out does not exist for the relationship derived between parent units",
    info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=km"
  )

  srp_default <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "hour",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = NA,
    pull = FALSE
  )

  expect_equal(srp_default$unit_out, srp_default$srp_unit_out, info = "x_unit_in=miles, y_unit_in=hour, unit_out=NA, pull=FALSE")
  expect_equal(srp_default$value_out, srp_default$srp_value_out, tolerance = 1e-8, info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=NA")

  alias_result <- unicon_advance(
    x_unit_in = " Miles ",
    y_unit_in = " HOUR ",
    x_value_in = 100,
    y_value_in = 2,
    unit_out = " km / hour "
  )

  expect_equal(alias_result, converted, tolerance = 1e-8, info = "x_unit_in=Miles, y_unit_in=HOUR, unit_out=km/hour")
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
    x_value_in = x:6,
    y_value_in = c(9, 8, 7, 5, 4, 2),
    unit_out = rep("km/day", 6)
  )

  expected_vignette <- suppressWarnings(suppressMessages(unicon_full(
    value_in = (x:6) / c(9, 8, 7, 5, 4, 2),
    unit_in = rep("mile/hour", 6),
    unit_out = rep("km/day", 6)
  )))

  mass_fraction <- unicon_advance(
    x_unit_in = "kg",
    y_unit_in = "kg",
    x_value_in = 7,
    y_value_in = 7,
    unit_out = NA
  )

  round_trip <- suppressWarnings(suppressMessages(unicon_full(
    value_in = speed_kmh,
    unit_in = "km/hour",
    unit_out = "mile/hour"
  )))

  expect_equal(speed_mph, 50, tolerance = 1e-8, info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=mile/hour")
  expect_equal(speed_kmh, 80.4672, tolerance = 1e-3, info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, unit_out=km/hour")
  expect_equal(workings$x_srp_value, advance_srp(100, "miles"), tolerance = 1e-8, info = "field=x_srp_value, x_unit_in=miles, x_value_in=100")
  expect_equal(workings$y_srp_value, advance_srp(2, "hour"), tolerance = 1e-8, info = "field=y_srp_value, y_unit_in=hour, y_value_in=2")
  expect_equal(vignette_example, expected_vignette, tolerance = 1e-8, info = "x_unit_in=milesx6, y_unit_in=hourx6, unit_out=km/day")
  expect_equal(mass_fraction, 1, tolerance = 1e-8, info = "x_unit_in=kg, y_unit_in=kg, x_value_in=7, y_value_in=7, unit_out=NA")
  expect_equal(round_trip, speed_mph, tolerance = 1e-8, info = "round_trip=km/hour_to_mile/hour, x_value_in=speed_kmh")
})

test_that("edge cases and integration paths are covered", {
  zero_numerator <- unicon_advance("m", "sec", 0, 5, unit_out = NA)
  zero_denominator <- unicon_advance("m", "sec", 5, 0, unit_out = NA)
  negative_values <- unicon_advance("m", "sec", -10, y, unit_out = NA)
  large_values <- unicon_advance("m", "sec", 1e12, 1e-6, unit_out = NA)
  small_values <- unicon_advance("m", "sec", 1e-12, 1e6, unit_out = NA)
  missing_values <- unicon_advance(
    "m",
    "sec",
    c(1, NA, NaN),
    c(1, 1, 1),
    unit_out = NA
  )
  single_value <- unicon_advance("m", "sec", 4, y, unit_out = NA)

  expect_equal(zero_numerator, 0, tolerance = 1e-8, info = "x_unit_in=m, y_unit_in=sec, x_value_in=0, y_value_in=5")
  expect_true(is.infinite(zero_denominator), info = "x_unit_in=m, y_unit_in=sec, x_value_in=5, y_value_in=0")
  expect_equal(negative_values, -5, tolerance = 1e-8, info = "x_unit_in=m, y_unit_in=sec, x_value_in=-10, y_value_in=2")
  expect_equal(large_values, 1e18, tolerance = 1e8, info = "x_unit_in=m, y_unit_in=sec, x_value_in=1e12, y_value_in=1e-6")
  expect_equal(small_values, 1e-18, tolerance = 1e-30, info = "x_unit_in=m, y_unit_in=sec, x_value_in=1e-12, y_value_in=1e6")
  expect_true(is.na(missing_values[2]), info = "x_unit_in=m, y_unit_in=sec, x_value_in=NA row=2")
  expect_true(is.nan(missing_values[3]), info = "x_unit_in=m, y_unit_in=sec, x_value_in=NaN row=3")
  expect_equal(single_value, 2, tolerance = 1e-8, info = "x_unit_in=m, y_unit_in=sec, x_value_in=4, y_value_in=2")

  expect_error(
    unicon_advance("not_a_unit", "sec", 1, 1, unit_out = NA),
    "Some x_unit_in values failed to find matches",
    info = "x_unit_in=not_a_unit, y_unit_in=sec, x_value_in=1, y_value_in=1, unit_out=NA"
  )

  expect_error(
    unicon_advance("m", "not_a_unit", 1, 1, unit_out = NA),
    "Some y_unit_in values failed to find matches",
    info = "x_unit_in=m, y_unit_in=not_a_unit, x_value_in=1, y_value_in=1, unit_out=NA"
  )

  direct_srp <- advance_expected(100, "miles", 2, "hour", "divide")
  advance_srp_value <- unicon_advance(
    "miles",
    "hour",
    100,
    2,
    unit_out = NA,
    pull = FALSE
  )
  expect_equal(advance_srp_value$srp_value_out, direct_srp, tolerance = 1e-8, info = "x_unit_in=miles, y_unit_in=hour, x_value_in=100, y_value_in=2, operator_in=divide")

  area <- unicon_advance(
    x_unit_in = "l",
    y_unit_in = "m",
    x_value_in = 12,
    y_value_in = 3,
    unit_out = "l__m",
    operator_in = "divide"
  )
  volume <- unicon_advance(
    x_unit_in = "l__m",
    y_unit_in = "m",
    x_value_in = area,
    y_value_in = 3,
    unit_out = "l",
    operator_in = "multiply"
  )
  mi1ed_systems <- unicon_advance(
    x_unit_in = "miles",
    y_unit_in = "sec",
    x_value_in = 1,
    y_value_in = 60,
    unit_out = "km/hour"
  )

  expect_equal(volume, 12, tolerance = 1e-8, info = "x_unit_in=l__m, y_unit_in=m, x_value_in=area, y_value_in=3, operator_in=multiply, unit_out=l")
  expect_equal(
    mi1ed_systems,
    suppressWarnings(suppressMessages(unicon_full(1 / 60, "mile/sec", "km/hour"))),
    tolerance = 1e-8,
    info = "x_unit_in=miles, y_unit_in=sec, x_value_in=1, y_value_in=60, unit_out=km/hour"
  )
})

## Tests for unicon_join_datasets -----------------------------------------------

## ---- shared test fixtures ---------------------------------------------------

make_base <- function() {
  tibble::tibble(
    id = c("m", "km", "cm", "s", "min", "kg", "pa", "kpa"),
    alias = c("m", "km", "cm", "s", "min", "kg", "pa", "kpa"),
    category = c(
      "length", "length", "length",
      "time", "time",
      "mass",
      "pressure", "pressure"
    ),
    srp = c("m", "m", "m", "s", "s", "kg", "pa", "pa"),
    slope = c(1, 1000, 0.01, 1, 60, 1, 1, 1000),
    intercept = rep(0, 8)
  )
}

make_derived <- function() {
  ## speed: both x="length" and y="time" are in base → derived rows are created.
  ## flow: x="volume" is NOT in base → filtered out, and keeps all(x in base)=FALSE
  ##       which prevents the stop() while keeping all(y in base)=TRUE for the warning.
  tibble::tibble(
    id       = c("speed", "flow"),
    x        = c("length", "volume"),
    y        = c("time", "time"),
    operator = c("divide", "divide")
  )
}

make_operators <- function() {
  tibble::tibble(
    operator = c("divide", "multiply"),
    id       = c("_", "."),
    fun      = c("/", "*"),
    alias    = c("/", "*")
  )
}

## ---- return type & schema ---------------------------------------------------

test_that("unicon_join_datasets returns a data frame", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  expect_s3_class(result, "data.frame")
})

test_that("unicon_join_datasets output contains required columns", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  expect_true(
    all(c("id", "alias", "category", "srp", "slope", "intercept", "type") %in%
      names(result)),
    info = "all required columns present"
  )
})

## ---- base rows are preserved ------------------------------------------------

test_that("base units are present in output with type == 'base'", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  base_rows <- result[result$type == "base", ]
  expect_true(nrow(base_rows) > 0, info = "at least one row with type=base")
  expect_true("m" %in% base_rows$id, info = "base unit 'm' present")
})

## ---- area derivation --------------------------------------------------------

test_that("area units are derived from length with correct suffixes", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  area_rows <- result[result$category == "area", ]
  expect_true(nrow(area_rows) > 0, info = "area rows exist")

  # id suffix is '2'
  expect_true(
    any(grepl("2$", area_rows$id)),
    info = "some area ids end in '2'"
  )

  # alias forms: 'squarecm', 'cm2', 'cmsquared' etc.
  expect_true(
    any(grepl("2|squared|square", area_rows$alias)),
    info = "area aliases contain 2, squared, or square"
  )

  # srp must be 'ha' for area
  expect_true(
    all(area_rows$srp == "ha"),
    info = "area srp is 'ha'"
  )
})

test_that("area slope equals (length_slope / 100)^2", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  # m2: slope of m is 1 => (1/100)^2 = 1e-4
  m2_row <- result[result$id == "m2" & result$category == "area", ]
  expect_true(nrow(m2_row) > 0, info = "m2 area row exists")
  expect_equal(m2_row$slope[[1]], (1 / 100)^2, tolerance = 1e-10)
})

## ---- volume derivation ------------------------------------------------------

test_that("volume units are derived from length with correct suffixes", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  vol_rows <- result[result$category == "volume", ]
  expect_true(nrow(vol_rows) > 0, info = "volume rows exist")

  expect_true(
    any(grepl("3|cubed|cubic", vol_rows$alias)),
    info = "volume aliases contain 3, cubed, or cubic"
  )

  expect_true(
    all(vol_rows$srp == "l"),
    info = "volume srp is 'l'"
  )
})

test_that("volume slope equals (length_slope / 0.1)^3", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  # m3: slope of m is 1 => (1/0.1)^3 = 1000
  m3_row <- result[result$id == "m3" & result$category == "volume", ]
  expect_true(nrow(m3_row) > 0, info = "m3 volume row exists")
  expect_equal(m3_row$slope[[1]], (1 / 0.1)^3, tolerance = 1e-10)
})

## ---- pressure srp override --------------------------------------------------

test_that("pressure units always have srp == 'pa'", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  pressure_rows <- result[result$category == "pressure", ]
  if (nrow(pressure_rows) > 0) {
    expect_true(
      all(pressure_rows$srp == "pa"),
      info = "all pressure srp values are 'pa'"
    )
  }
})

## ---- no spurious duplicate/wrong rows ---------------------------------------

test_that("area rows with srp 'l_m' are filtered out", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  bad_area <- result[result$category == "area" & result$srp == "l_m", ]
  expect_equal(nrow(bad_area), 0L, info = "no area rows with srp == 'l_m'")
})

test_that("length rows with srp 'ha_m' are filtered out", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  bad_len <- result[result$category == "length" & result$srp == "ha_m", ]
  expect_equal(nrow(bad_len), 0L, info = "no length rows with srp == 'ha_m'")
})

## ---- derived unit categories present only when x & y in base ----------------

test_that("derived rows are present when x and y categories are in base", {
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  speed_rows <- result[result$category == "speed", ]
  expect_true(nrow(speed_rows) > 0, info = "speed rows present when both base categories available")
})

test_that("derived rows are absent when x category is not in base", {
  ## flow has x="volume" which is NOT in make_base(), so no flow rows expected
  result <- suppressWarnings(
    unicon_join_datasets(make_base(), make_derived(), make_operators())
  )
  flow_rows <- result[result$category == "flow", ]
  expect_equal(nrow(flow_rows), 0L, info = "flow absent when volume missing from base")
})

## ---- warning when only one dimension of derived is fully in base ------------

test_that("a warning is issued when only one side of derived is in base", {
  ## all(y in base) = TRUE (y="time" always in base), all(x in base) = FALSE
  ## (x="volume" not in base)  =>  OR condition fires the warning
  expect_warning(
    unicon_join_datasets(make_base(), make_derived(), make_operators()),
    "Derived data is not just made of base units"
  )
})

test_that("package data snapshot: .unicon_state datasets", {
  expect_snapshot({
    cat("unit_alias nrow:", nrow(.unicon_state$unit_alias), "\n")
    cat("unit_alias names:", paste(names(.unicon_state$unit_alias), collapse = ","), "\n")

    cat("unit_srp nrow:", nrow(.unicon_state$unit_srp), "\n")
    cat("unit_srp names:", paste(names(.unicon_state$unit_srp), collapse = ","), "\n")

    cat("unit_models nrow:", nrow(.unicon_state$unit_models), "\n")
    cat("unit_models names:", paste(names(.unicon_state$unit_models), collapse = ","), "\n")

    cat("relationships nrow:", nrow(.unicon_state$relationships), "\n")
    cat("relationships names:", paste(names(.unicon_state$relationships), collapse = ","), "\n")
  })
})

test_that("json parity: key base units match internal models", {
  key_units <- list(
    list(path = c("units", "base", "mass", "lb.json"), id = "lb", slope = 453.59237, srp = "g"),
    list(path = c("units", "base", "mass", "oz.json"), id = "oz", slope = 28.349523125, srp = "g"),
    list(path = c("units", "base", "temperature", "fahrenheit.json"), id = "fahrenheit", slope = 0.555556, srp = "C"),
    list(path = c("units", "base", "pressure", "pa.json"), id = "pa", slope = 1, srp = "pa"),
    list(path = c("units", "base", "pressure", "kpa.json"), id = "kpa", slope = 1000, srp = "pa"),
    list(path = c("units", "base", "pressure", "mpa.json"), id = "mpa", slope = 1000000, srp = "pa"),
    list(path = c("units", "base", "pressure", "bar.json"), id = "bar", slope = 100000, srp = "pa")
  )

  for (u in key_units) {
    j <- read_pkg_json(u$path[[1]], u$path[[2]], u$path[[3]], u$path[[4]])

    expect_equal(j$srp, u$srp)
    expect_equal(j$model$slope, u$slope, tolerance = 1e-12)

    internal <- dplyr::filter(.unicon_state$unit_models, .data$id == u$id)
    expect_true(nrow(internal) >= 1L)

    expect_true(any(internal$srp == j$srp))
    expect_true(any(abs(internal$slope - j$model$slope) < 1e-12))
  }
})

test_that("json parity: pressure category uses pa as SRP internally", {
  pressure <- dplyr::filter(.unicon_state$unit_models, .data$category == "pressure")
  expect_true(nrow(pressure) > 0L)
  expect_true(all(pressure$srp == "pa"))
})

test_that("internal data invariants: known aliases and row counts are stable", {
  sec_alias <- dplyr::filter(.unicon_state$unit_alias, .data$id == "s")
  expect_true(any(sec_alias$alias == "s"))

  expect_true(nrow(.unicon_state$unit_alias) > 1000L)
  expect_true(nrow(.unicon_state$unit_models) > 100L)
  expect_true(nrow(.unicon_state$relationships) > 10L)

  expect_snapshot({
    cat("unit_alias_nrow:", nrow(.unicon_state$unit_alias), "\n")
    cat("unit_models_nrow:", nrow(.unicon_state$unit_models), "\n")
    cat("relationships_nrow:", nrow(.unicon_state$relationships), "\n")
  })
})

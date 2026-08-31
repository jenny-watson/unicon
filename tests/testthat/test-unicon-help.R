test_that("unicon_help has no missing values", {
  unicon_reset_units()

  h <- unicon_help()

  expect_true(is.data.frame(h))
  expect_false(anyNA(h))
})

test_that("unicon_help has a 1-to-1 mapping between srp and category", {
  unicon_reset_units()

  h <- unicon_help()

  expect_true(all(c("srp", "category") %in% names(h)))

  unique_pairs <- unique(h[c("srp", "category")])

  expect_equal(nrow(unique_pairs), length(unique(h$srp)))
  expect_equal(nrow(unique_pairs), length(unique(h$category)))
})

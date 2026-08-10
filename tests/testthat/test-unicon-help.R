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

  srp_to_category <- vapply(
    split(h$category, h$srp),
    function(x) length(unique(x)) == 1L,
    logical(1)
  )
  category_to_srp <- vapply(
    split(h$srp, h$category),
    function(x) length(unique(x)) == 1L,
    logical(1)
  )

  expect_true(all(srp_to_category))
  expect_true(all(category_to_srp))
  expect_equal(length(unique(h$srp)), length(unique(h$category)))
})

test_that("function works", {
  df <- data.frame(
    m = c("k", "cm"),
    q = c(18, 11)
  )

  df_si <- data.frame(
    m = c("k", "cm"),
    q = c(18, 11),
    si = c("celcius", "m"),
    si_quantity = c(-255.15, 0.11)
  )

  expect_equal(
    unit_conv(df,
      measure = m,
      quantity = q
    ),
    df_si,
    ignore_attr = TRUE
  ) # ignores difference between tbl_df and tbl_object used to demo test
})


test_that("blanks and associated warnings work", { # warning still on this test although I've included the warning?

  df <- data.frame(
    m = c("stone", "mls"),
    q = c(1, 2000)
  )

  df_si <- data.frame(
    m = c("stone", "mls"),
    q = c(1, 2000),
    si = c(NA_character_, NA_character_),
    si_quantity = c(NA_real_, NA_real_)
  )

  expect_equal(
    unit_conv(df,
      measure = m,
      quantity = q
    ),
    df_si,
    ignore_attr = TRUE
  )

  expect_warning(
    unit_conv(df,
      measure = m,
      quantity = q
    ),
    "NA values detected so results may be incomplete"
  )
})


test_that("2 columns added", {
  df <- data.frame(
    m = c("k", "mls"),
    q = c(18, 2000)
  )

  expect_equal(
    ncol(unit_conv(df,
      measure = m,
      quantity = q
    )),
    ncol(df) + 2
  )

  ## comes up with warning from above
})


test_that("same rows", { # to test joins

  df <- data.frame(
    m = c("k", "mls"),
    q = c(18, 2000)
  )

  expect_equal(
    nrow(unit_conv(df,
      measure = m,
      quantity = q
    )),
    nrow(df)
  )

  ## comes up with warning from above
})


test_that("data type error works", { # to test joins

  df <- data.frame(
    m = c("k", "mls"),
    q = c("k", "mls")
  )

  expect_error(
    unit_conv(df,
      measure = m,
      quantity = q
    ),
    "quantity must be numeric"
  )

  df <- data.frame(
    m = c(18, 2000),
    q = c(18, 2000)
  )

  expect_error(
    unit_conv(df,
      measure = m,
      quantity = q
    ),
    "measure must be character"
  )

  ## comes up with warning from above
})

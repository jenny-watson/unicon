test_that("documented unicon_full and unicon_lite examples stay aligned", {
  raw_values <- c(54.21, 71.24, 55.81, 11.33, 70.59)
  raw_units <- c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "g/m2")
  raw_ids <- c("t__ha", "st__acre", "t__ha", "kg__ha", "g__m_2")
  unit_out <- "tonnes / ha"
  id_out <- "t__ha"

  expect_equal(
    unicon_full(
      value_in = raw_values,
      unit_in = raw_units,
      unit_out = unit_out,
    ),
    unicon_lite(
      value_in = raw_values,
      id_in = raw_ids,
      id_out = id_out
    )$value_out
  )
})

test_that("documented invalid unit example keeps valid rows and flags the bad one", {
  raw_values <- c(54.21, 71.24, 55.81, 11.33, 70.59)
  raw_units <- c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "nonsense per fiction")
  valid_rows <- seq_len(4L)
  valid_ids <- c("t__ha", "st__acre", "t__ha", "kg__ha")
  unit_out <- "tonnes / ha"
  id_out <- "t__ha"

  expect_warning(
    full_tbl <- unicon_full(
      value_in = raw_values,
      unit_in = raw_units,
      unit_out = unit_out,
      pull = FALSE
    ),
    "Some input units failed to find matches\\."
  )

  lite_out <- unicon_lite(
    value_in = raw_values[valid_rows],
    id_in = valid_ids,
    id_out = id_out
  )

  expect_false(any(full_tbl$error_in[valid_rows]))
  expect_true(full_tbl$error_in[[length(raw_values)]])
  expect_equal(full_tbl$value_out[valid_rows], lite_out$value_out)
  expect_true(is.na(full_tbl$value_out[[length(raw_values)]]))
})

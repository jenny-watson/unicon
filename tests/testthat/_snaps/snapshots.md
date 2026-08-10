# unit_alias snapshot: dimensions and column names

    Code
      cat("nrow:", nrow(unit_alias), "\n")
    Output
      nrow: 14328 
    Code
      cat("ncol:", ncol(unit_alias), "\n")
    Output
      ncol: 2 
    Code
      cat("names:", paste(sort(names(unit_alias)), collapse = ", "), "\n")
    Output
      names: alias, id 

# unit_alias snapshot: class and types

    Code
      cat("class:", class(unit_alias), "\n")
    Output
      class: tbl_df tbl data.frame 
    Code
      cat("id type:", class(unit_alias$id), "\n")
    Output
      id type: character 
    Code
      cat("alias type:", class(unit_alias$alias), "\n")
    Output
      alias type: character 

# unit_alias snapshot: sample of known unit entries

    Code
      cat("m row present:", nrow(m_row) == 1L, "\n")
    Output
      m row present: TRUE 
    Code
      cat("kg row present:", nrow(kg_row) == 1L, "\n")
    Output
      kg row present: TRUE 

# unit_srp snapshot: dimensions and column names

    Code
      cat("nrow:", nrow(unit_srp), "\n")
    Output
      nrow: 325 
    Code
      cat("ncol:", ncol(unit_srp), "\n")
    Output
      ncol: 4 
    Code
      cat("names:", paste(sort(names(unit_srp)), collapse = ", "), "\n")
    Output
      names: category, id, srp, type 

# unit_srp snapshot: class and types

    Code
      cat("class:", class(unit_srp), "\n")
    Output
      class: tbl_df tbl data.frame 
    Code
      cat("id type:", class(unit_srp$id), "\n")
    Output
      id type: character 
    Code
      cat("srp type:", class(unit_srp$srp), "\n")
    Output
      srp type: character 

# unit_srp snapshot: known SRP entries

    Code
      cat("length SRP:", unit_srp$srp[unit_srp$id == "m"], "\n")
    Output
      length SRP: m 
    Code
      cat("mass SRP:", unit_srp$srp[unit_srp$id == "kg"], "\n")
    Output
      mass SRP: g 
    Code
      cat("temperature SRP:", unit_srp$srp[unit_srp$id == "C"], "\n")
    Output
      temperature SRP: C 

# unit_models snapshot: dimensions and column names

    Code
      cat("nrow:", nrow(unit_models), "\n")
    Output
      nrow: 326 
    Code
      cat("ncol:", ncol(unit_models), "\n")
    Output
      ncol: 2 
    Code
      cat("names:", paste(sort(names(unit_models)), collapse = ", "), "\n")
    Output
      names: id, model 

# unit_models snapshot: class and types

    Code
      cat("class:", class(unit_models), "\n")
    Output
      class: tbl_df tbl data.frame 
    Code
      cat("id type:", class(unit_models$id), "\n")
    Output
      id type: character 
    Code
      cat("model type:", class(unit_models$model), "\n")
    Output
      model type: list 

# unit_models snapshot: SRP units have slope=1 intercept=0

    Code
      cat("m slope:", m_model$slope, "\n")
    Output
      m slope: 1 
    Code
      cat("m intercept:", m_model$intercept, "\n")
    Output
      m intercept: 0 
    Code
      cat("kg slope:", kg_model$slope, "\n")
    Output
      kg slope: 1000 
    Code
      cat("kg intercept:", kg_model$intercept, "\n")
    Output
      kg intercept: 0 

# unit_models snapshot: temperature SRP (Celsius) has slope=1 intercept=0

    Code
      cat("C slope:", c_model$slope, "\n")
    Output
      C slope: 1 
    Code
      cat("C intercept:", c_model$intercept, "\n")
    Output
      C intercept: 0 

# relationships snapshot: dimensions and column names

    Code
      cat("nrow:", nrow(relationships), "\n")
    Output
      nrow: 34 
    Code
      cat("ncol:", ncol(relationships), "\n")
    Output
      ncol: 4 
    Code
      cat("names:", paste(sort(names(relationships)), collapse = ", "), "\n")
    Output
      names: id, operator, x, y 

# relationships snapshot: class

    Code
      cat("class:", class(relationships), "\n")
    Output
      class: tbl_df tbl data.frame 

# relationships snapshot: known relationships present

    Code
      cat(paste(cats, collapse = "\n"), "\n")
    Output
      amount_of_substance
      area
      area_density
      concentration
      force
      length
      mass
      mass_fraction
      pressure
      speed
      time
      volume
      volume_density
      volume_fraction 

# unicon_full error snapshot: non-numeric value_in

    Code
      unicon_full("1", "m", "cm")
    Condition
      Error in `unicon_full()`:
      ! Argument `value_in` must be numeric.

# unicon_full error snapshot: non-character unit_in

    Code
      unicon_full(1, 2, "cm")
    Condition
      Error in `unicon_full()`:
      ! Argument `unit_in` must be a character vector.

# unicon_full error snapshot: non-character unit_out

    Code
      unicon_full(1, "m", TRUE)
    Condition
      Error in `unicon_full()`:
      ! Argument `unit_out` must be a character vector or `NA`.

# unicon_full error snapshot: zero-length value_in

    Code
      unicon_full(numeric(0), "m", "cm")
    Condition
      Error in `unicon_full()`:
      ! Argument `value_in` must have length >= 1.

# unicon_full error snapshot: wrong-length unit_in

    Code
      unicon_full(1:2, c("m", "cm", "km"), "cm")
    Condition
      Error in `unicon_full()`:
      ! Argument `unit_in` must have length 1 or length(value_in).

# unicon_full error snapshot: wrong-length unit_out

    Code
      unicon_full(1:3, "m", c("cm", "mm"))
    Condition
      Error in `unicon_full()`:
      ! Argument `unit_out` must have length 1 or length(value_in).

# unicon_full message snapshot: no unit_out given

    Code
      unicon_full(1, "m")
    Message
      No output unit given. Converting all values to standard reference unit.
    Output
      [1] 1

# unicon_full message snapshot: partially missing unit_out

    Code
      unicon_full(c(1, 2), "m", c("cm", NA))
    Message
      Output unit missing in some cases. Converting to standard reference unit where missing.
    Output
      [1] 100   2

# unicon_full warning snapshot: unknown unit_in

    Code
      unicon_full(1, "not_a_unit", "cm")
    Condition
      Warning in `unicon_full()`:
      Some units failed to convert or had invalid IDs. Set `pull = FALSE` for detailed output.
    Output
      [1] NA

# unicon_full warning snapshot: unknown unit_out

    Code
      unicon_full(1, "m", "not_a_unit")
    Output
      [1] 1

# unicon_full warning snapshot: mismatched unit types

    Code
      unicon_full(1, "m", "kg")
    Condition
      Warning in `unicon_full()`:
      Some units failed to convert or had invalid IDs. Set `pull = FALSE` for detailed output.
    Output
      [1] NA

# unicon_lite warning snapshot: unknown id_in

    Code
      unicon_lite(1, "not_a_unit", "cm")
    Output
      # A tibble: 1 x 9
        id_in  id_out srp_in error_in error_srp error_out value_in value_srp value_out
        <chr>  <chr>  <chr>  <lgl>    <lgl>     <lgl>        <dbl>     <dbl>     <dbl>
      1 not_a~ cm     <NA>   TRUE     NA        FALSE            1        NA        NA

# unicon_lite warning snapshot: mismatched unit types

    Code
      unicon_lite(1, "m", "kg")
    Output
      # A tibble: 1 x 9
        id_in id_out srp_in error_in error_srp error_out value_in value_srp value_out
        <chr> <chr>  <chr>  <lgl>    <lgl>     <lgl>        <dbl>     <dbl>     <dbl>
      1 m     kg     m      FALSE    TRUE      FALSE            1         1        NA

# unicon_advance error snapshot: mismatched value lengths

    Code
      unicon_advance(x_unit_in = "miles", y_unit_in = "hour", x_value_in = c(1, 2, 3),
      y_value_in = c(1, 1), unit_out = "km/hour")
    Condition
      Error in `unicon_advance()`:
      ! Argument `x_value_in` and `y_value_in` must have same length.

# unicon_advance error snapshot: no recorded relationship

    Code
      unicon_advance(x_unit_in = "m", y_unit_in = "kg", x_value_in = 1, y_value_in = 1,
        unit_out = NA)
    Message
      No output unit given. Converting all values to standard reference unit.
      No output unit given. Converting all values to standard reference unit.
    Condition
      Error in `unicon_advance()`:
      ! There is no recorded relationship between parent units

# unicon_advance error snapshot: operator mismatch

    Code
      unicon_advance(x_unit_in = "kg", y_unit_in = "ha", x_value_in = 10, y_value_in = 2,
        unit_out = NA, operator_in = "multiply")
    Message
      No output unit given. Converting all values to standard reference unit.
      No output unit given. Converting all values to standard reference unit.
    Condition
      Error in `unicon_advance()`:
      ! `operator_in` does not match the relationship derived between parent units

# unicon_make_own_base_data error snapshot: non-character id

    Code
      unicon_make_own_base_data(1, "alias", "cat", "srp", 1, 0)
    Condition
      Error in `unicon_make_own_base_data()`:
      ! `id`, `alias`, `category` and `srp` need to be characters

# unicon_make_own_base_data error snapshot: non-numeric slope

    Code
      unicon_make_own_base_data("id", "alias", "cat", "srp", "one", 0)
    Condition
      Error in `unicon_make_own_base_data()`:
      ! `slope` and `intercept` need to be numeric

# unicon_make_own_base_data warning snapshot: non-zero intercept

    Code
      unicon_make_own_base_data("id", "alias", "cat", "srp", 1, 5)
    Condition
      Warning in `unicon_make_own_base_data()`:
      `intercept` is not zero, please check this is correct

# unicon_make_own_base_data error snapshot: mismatched vector lengths

    Code
      unicon_make_own_base_data(c("id1", "id2"), "alias", "cat", "srp", 1, 0)
    Condition
      Error in `unicon_make_own_base_data()`:
      ! All vectors supplied to `unicon_make_own_base_data` must be the same length. Lengths provided: id = 2, alias = 1, category = 1, srp = 1, slope = 1, intercept = 1

# unicon_reset_units snapshot: resets state silently

    Code
      unicon_reset_units()

# unicon_own_status snapshot: returns FALSE after reset

    Code
      unicon_own_status()
    Output
      [1] FALSE


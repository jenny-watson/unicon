# unicon_full pull = TRUE snapshots

    Code
      unicon_full(1, "kilometres", "mi", pull = TRUE)
    Output
      [1] 0.6213727
    Code
      unicon_full(c(0, 100, -40), "celsius", "fahrenheit", pull = TRUE)
    Output
      [1]  31.99748 211.98308 -39.99676
    Code
      unicon_full(c(1, 2.5), "kg", "g", pull = TRUE)
    Output
      [1] 1000 2500

# unicon_full pull = FALSE snapshot

    Code
      unicon_full(c(1, 2), c("m", "kg"), c("cm", "g"), pull = FALSE)
    Output
      # A tibble: 2 x 13
        unit_in unit_out alias_in alias_out id_in id_srp id_out error_in error_srp
        <chr>   <chr>    <chr>    <chr>     <chr> <chr>  <chr>  <lgl>    <lgl>    
      1 m       cm       m        cm        m     m      cm     FALSE    FALSE    
      2 kg      g        kg       g         kg    g      g      FALSE    FALSE    
      # i 4 more variables: error_out <lgl>, value_in <dbl>, value_srp <dbl>,
      #   value_out <dbl>

# unicon_full missing unit_out snapshots

    Code
      unicon_full(c(100, 1), c("cm", "kg"), pull = FALSE)
    Message
      No output unit given. Converting all values to standard reference unit.
    Output
      # A tibble: 2 x 13
        unit_in unit_out alias_in alias_out id_in id_srp id_out error_in error_srp
        <chr>   <lgl>    <chr>    <chr>     <chr> <chr>  <chr>  <lgl>    <lgl>    
      1 cm      NA       cm       <NA>      cm    m      m      FALSE    NA       
      2 kg      NA       kg       <NA>      kg    g      g      FALSE    NA       
      # i 4 more variables: error_out <lgl>, value_in <dbl>, value_srp <dbl>,
      #   value_out <dbl>

# unicon_full unrecognised unit snapshots

    Code
      suppressMessages(unicon_full(1, "not_a_unit", "km", pull = FALSE))
    Condition
      Warning in `unicon_full()`:
      Some input units failed to find matches.
    Output
      # A tibble: 1 x 13
        unit_in    unit_out alias_in  alias_out id_in id_srp id_out error_in error_srp
        <chr>      <chr>    <chr>     <chr>     <chr> <chr>  <chr>  <lgl>    <lgl>    
      1 not_a_unit km       not_a_un~ km        <NA>  <NA>   km     TRUE     NA       
      # i 4 more variables: error_out <lgl>, value_in <dbl>, value_srp <dbl>,
      #   value_out <dbl>

# unicon_full mismatched unit types snapshot

    Code
      suppressMessages(unicon_full(1, "m", "g", pull = FALSE))
    Condition
      Warning in `unicon_full()`:
      Some requested conversions were not valid (unit type mismatch).
    Output
      # A tibble: 1 x 13
        unit_in unit_out alias_in alias_out id_in id_srp id_out error_in error_srp
        <chr>   <chr>    <chr>    <chr>     <chr> <chr>  <chr>  <lgl>    <lgl>    
      1 m       g        m        g         m     m      g      FALSE    TRUE     
      # i 4 more variables: error_out <lgl>, value_in <dbl>, value_srp <dbl>,
      #   value_out <dbl>

# unicon_lite conversion table snapshot

    Code
      unicon_lite(c(1, 2), c("m", "kg"), c("cm", "g"))
    Output
      # A tibble: 2 x 9
        id_in id_out id_srp error_in error_srp error_out value_in value_srp value_out
        <chr> <chr>  <chr>  <lgl>    <lgl>     <lgl>        <dbl>     <dbl>     <dbl>
      1 m     cm     m      FALSE    FALSE     FALSE            1         1       100
      2 kg    g      g      FALSE    FALSE     FALSE            2      2000      2000

# unicon_lite missing id_out snapshot

    Code
      unicon_lite(c(100, 1), c("cm", "kg"))
    Output
      # A tibble: 2 x 9
        id_in id_out id_srp error_in error_srp error_out value_in value_srp value_out
        <chr> <chr>  <chr>  <lgl>    <lgl>     <lgl>        <dbl>     <dbl>     <dbl>
      1 cm    m      m      FALSE    NA        FALSE          100         1         1
      2 kg    g      g      FALSE    NA        FALSE            1      1000      1000

# unicon_lite unrecognised id snapshot

    Code
      unicon_lite(1, "not_a_unit", "km")
    Output
      # A tibble: 1 x 9
        id_in  id_out id_srp error_in error_srp error_out value_in value_srp value_out
        <chr>  <chr>  <chr>  <lgl>    <lgl>     <lgl>        <dbl>     <dbl>     <dbl>
      1 not_a~ km     <NA>   TRUE     NA        FALSE            1        NA        NA

# unicon_lite mismatched unit types snapshot

    Code
      unicon_lite(1, "m", "g")
    Output
      # A tibble: 1 x 9
        id_in id_out id_srp error_in error_srp error_out value_in value_srp value_out
        <chr> <chr>  <chr>  <lgl>    <lgl>     <lgl>        <dbl>     <dbl>     <dbl>
      1 m     g      m      FALSE    TRUE      FALSE            1         1        NA

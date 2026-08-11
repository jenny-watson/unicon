# unicon_full pull=FALSE snapshot: column names

    Code
      cat(paste(names(out), collapse = "\n"), "\n")
    Output
      unit_in
      unit_out
      alias_in
      alias_out
      id_in
      srp_in
      id_out
      error_in
      error_srp
      error_out
      value_in
      value_srp
      value_out 

# unicon_full pull=FALSE snapshot: column types

    Code
      types <- col_types(out)
      for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
    Output
      unit_in : character 
      unit_out : character 
      alias_in : character 
      alias_out : character 
      id_in : character 
      srp_in : character 
      id_out : character 
      error_in : logical 
      error_srp : logical 
      error_out : logical 
      value_in : numeric 
      value_srp : numeric 
      value_out : numeric 

# unicon_full pull=FALSE snapshot: full shape for length conversion

    Code
      snap_shape(out)
    Output
      nrow: 2 
      ncol: 13 
        unit_in : character 
        unit_out : character 
        alias_in : character 
        alias_out : character 
        id_in : character 
        srp_in : character 
        id_out : character 
        error_in : logical 
        error_srp : logical 
        error_out : logical 
        value_in : numeric 
        value_srp : numeric 
        value_out : numeric 

# unicon_full pull=FALSE snapshot: full shape for temperature conversion

    Code
      snap_shape(out)
    Output
      nrow: 2 
      ncol: 13 
        unit_in : character 
        unit_out : character 
        alias_in : character 
        alias_out : character 
        id_in : character 
        srp_in : character 
        id_out : character 
        error_in : logical 
        error_srp : logical 
        error_out : logical 
        value_in : numeric 
        value_srp : numeric 
        value_out : numeric 

# unicon_full pull=FALSE snapshot: shape consistent across categories

    Code
      cat("length names match mass:", identical(names(length_out), names(mass_out)),
      "\n")
    Output
      length names match mass: TRUE 
    Code
      cat("length names match temp:", identical(names(length_out), names(temp_out)),
      "\n")
    Output
      length names match temp: TRUE 
    Code
      cat("length types match mass:", identical(col_types(length_out), col_types(
        mass_out)), "\n")
    Output
      length types match mass: TRUE 
    Code
      cat("length types match temp:", identical(col_types(length_out), col_types(
        temp_out)), "\n")
    Output
      length types match temp: TRUE 

# unicon_lite snapshot: column names

    Code
      cat(paste(names(out), collapse = "\n"), "\n")
    Output
      id_in
      id_out
      srp_in
      error_in
      error_srp
      error_out
      value_in
      value_srp
      value_out 

# unicon_lite snapshot: column types

    Code
      types <- col_types(out)
      for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
    Output
      id_in : character 
      id_out : character 
      srp_in : character 
      error_in : logical 
      error_srp : logical 
      error_out : logical 
      value_in : numeric 
      value_srp : numeric 
      value_out : numeric 

# unicon_lite snapshot: full shape for mass conversion

    Code
      snap_shape(out)
    Output
      nrow: 2 
      ncol: 9 
        id_in : character 
        id_out : character 
        srp_in : character 
        error_in : logical 
        error_srp : logical 
        error_out : logical 
        value_in : numeric 
        value_srp : numeric 
        value_out : numeric 

# unicon_lite snapshot: shape consistent across unit categories

    Code
      cat("length names match mass:", identical(names(length_out), names(mass_out)),
      "\n")
    Output
      length names match mass: TRUE 
    Code
      cat("length names match temp:", identical(names(length_out), names(temp_out)),
      "\n")
    Output
      length names match temp: TRUE 
    Code
      cat("length types match mass:", identical(col_types(length_out), col_types(
        mass_out)), "\n")
    Output
      length types match mass: TRUE 
    Code
      cat("length types match temp:", identical(col_types(length_out), col_types(
        temp_out)), "\n")
    Output
      length types match temp: TRUE 

# unicon_lite and unicon_full share overlapping column names and types

    Code
      cat("shared columns:", paste(shared_cols, collapse = ", "), "\n")
    Output
      shared columns: id_in, id_out, srp_in, error_in, error_srp, error_out, value_in, value_srp, value_out 
    Code
      lite_types <- col_types(lite_out[shared_cols])
      full_types <- col_types(full_out[shared_cols])
      cat("types match:", identical(lite_types, full_types), "\n")
    Output
      types match: TRUE 

# unicon_advance pull=FALSE snapshot: column names

    Code
      cat(paste(names(out), collapse = "\n"), "\n")
    Output
      x_category
      x_unit_in
      x_value_in
      x_alias_in
      x_id_in
      x_id_out
      x_error_in
      x_error_srp
      x_error_out
      x_value_srp
      y_category
      y_unit_in
      y_value_in
      y_alias_in
      y_id_in
      y_id_out
      y_error_in
      y_error_srp
      y_error_out
      y_value_srp
      operator_in
      id
      unit_in
      unit_out
      alias_in
      alias_out
      id_in
      srp_in
      id_out
      error_in
      error_srp
      error_out
      value_in
      value_srp
      value_out 

# unicon_advance pull=FALSE snapshot: column types

    Code
      types <- col_types(out)
      for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
    Output
      x_category : character 
      x_unit_in : character 
      x_value_in : numeric 
      x_alias_in : character 
      x_id_in : character 
      x_id_out : character 
      x_error_in : logical 
      x_error_srp : logical 
      x_error_out : logical 
      x_value_srp : numeric 
      y_category : character 
      y_unit_in : character 
      y_value_in : numeric 
      y_alias_in : character 
      y_id_in : character 
      y_id_out : character 
      y_error_in : logical 
      y_error_srp : logical 
      y_error_out : logical 
      y_value_srp : numeric 
      operator_in : character 
      id : character 
      unit_in : character 
      unit_out : character 
      alias_in : character 
      alias_out : character 
      id_in : character 
      srp_in : character 
      id_out : character 
      error_in : logical 
      error_srp : logical 
      error_out : logical 
      value_in : numeric 
      value_srp : numeric 
      value_out : numeric 

# unicon_advance pull=FALSE snapshot: shape for speed conversion

    Code
      snap_shape(out)
    Output
      nrow: 2 
      ncol: 35 
        x_category : character 
        x_unit_in : character 
        x_value_in : numeric 
        x_alias_in : character 
        x_id_in : character 
        x_id_out : character 
        x_error_in : logical 
        x_error_srp : logical 
        x_error_out : logical 
        x_value_srp : numeric 
        y_category : character 
        y_unit_in : character 
        y_value_in : numeric 
        y_alias_in : character 
        y_id_in : character 
        y_id_out : character 
        y_error_in : logical 
        y_error_srp : logical 
        y_error_out : logical 
        y_value_srp : numeric 
        operator_in : character 
        id : character 
        unit_in : character 
        unit_out : character 
        alias_in : character 
        alias_out : character 
        id_in : character 
        srp_in : character 
        id_out : character 
        error_in : logical 
        error_srp : logical 
        error_out : logical 
        value_in : numeric 
        value_srp : numeric 
        value_out : numeric 

# unicon_advance pull=FALSE snapshot: shape for area_density conversion

    Code
      snap_shape(out)
    Output
      nrow: 2 
      ncol: 35 
        x_category : character 
        x_unit_in : character 
        x_value_in : numeric 
        x_alias_in : character 
        x_id_in : character 
        x_id_out : character 
        x_error_in : logical 
        x_error_srp : logical 
        x_error_out : logical 
        x_value_srp : numeric 
        y_category : character 
        y_unit_in : character 
        y_value_in : numeric 
        y_alias_in : character 
        y_id_in : character 
        y_id_out : character 
        y_error_in : logical 
        y_error_srp : logical 
        y_error_out : logical 
        y_value_srp : numeric 
        operator_in : character 
        id : character 
        unit_in : character 
        unit_out : logical 
        alias_in : character 
        alias_out : character 
        id_in : character 
        srp_in : character 
        id_out : character 
        error_in : logical 
        error_srp : logical 
        error_out : logical 
        value_in : numeric 
        value_srp : numeric 
        value_out : numeric 

# unicon_advance pull=FALSE shape is consistent across derived categories

    Code
      cat("speed names match density:", identical(names(speed_out), names(density_out)),
      "\n")
    Output
      speed names match density: TRUE 
    Code
      cat("speed types match density:", identical(col_types(speed_out), col_types(
        density_out)), "\n")
    Output
      speed types match density: FALSE 

# unicon_make_own_base_data snapshot: column names

    Code
      cat(paste(names(out), collapse = "\n"), "\n")
    Output
      id
      alias
      category
      srp
      slope
      intercept 

# unicon_make_own_base_data snapshot: column types

    Code
      types <- col_types(out)
      for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
    Output
      id : character 
      alias : character 
      category : character 
      srp : character 
      slope : numeric 
      intercept : numeric 

# unicon_make_own_derived_data snapshot: column names

    Code
      cat(paste(names(out), collapse = "\n"), "\n")
    Output
      id
      x
      y
      operator 

# unicon_make_own_derived_data snapshot: column types

    Code
      types <- col_types(out)
      for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
    Output
      id : character 
      x : character 
      y : character 
      operator : character 

# unicon_make_own_operators_data snapshot: column names

    Code
      cat(paste(names(out), collapse = "\n"), "\n")
    Output
      operator
      id
      fun
      alias 

# unicon_make_own_operators_data snapshot: column types

    Code
      types <- col_types(out)
      for (nm in names(types)) cat(nm, ":", types[[nm]], "\n")
    Output
      operator : character 
      id : character 
      fun : character 
      alias : character 


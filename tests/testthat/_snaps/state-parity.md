# package data snapshot: .unicon_state datasets

    Code
      cat("unit_alias nrow:", nrow(state$unit_alias), "\n")
    Output
      unit_alias nrow: 70036 
    Code
      cat("unit_alias names:", paste(names(state$unit_alias), collapse = ","), "\n")
    Output
      unit_alias names: id,alias 
    Code
      cat("unit_srp nrow:", nrow(state$unit_srp), "\n")
    Output
      unit_srp nrow: 840 
    Code
      cat("unit_srp names:", paste(names(state$unit_srp), collapse = ","), "\n")
    Output
      unit_srp names: id,type,category,srp 
    Code
      cat("unit_models nrow:", nrow(state$unit_models), "\n")
    Output
      unit_models nrow: 841 
    Code
      cat("unit_models names:", paste(names(state$unit_models), collapse = ","), "\n")
    Output
      unit_models names: id,model 
    Code
      cat("relationships nrow:", nrow(state$relationships), "\n")
    Output
      relationships nrow: 42 
    Code
      cat("relationships names:", paste(names(state$relationships), collapse = ","),
      "\n")
    Output
      relationships names: id,x,y,operator 

# internal data invariants: known aliases and row counts are stable

    Code
      cat("unit_alias_nrow:", nrow(state$unit_alias), "\n")
    Output
      unit_alias_nrow: 70036 
    Code
      cat("unit_models_nrow:", nrow(state$unit_models), "\n")
    Output
      unit_models_nrow: 841 
    Code
      cat("relationships_nrow:", nrow(state$relationships), "\n")
    Output
      relationships_nrow: 42 


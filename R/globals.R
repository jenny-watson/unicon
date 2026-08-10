# R/globals.R
# instead of .data$ - tidy version

utils::globalVariables(
  c(
    "error_in",
    "error_out",
    "error_srp",
    "id_in",
    "id_out",
    "id_out_not_na",
    "model",
    "model_in",
    "model_out",
    "srp_in",
    "srp_out",
    "value_out",
    "value_srp"
  )
)

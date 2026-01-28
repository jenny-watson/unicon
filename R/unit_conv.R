

unit_conv = function(df,
                     measure,
                     quantity){


  dataframe = df |>
    left_join(unit_alias, ## join alias to find id
              join_by({{measure}} == alias)) |> ## {{}} allows any column to be passed regardless of name
    left_join(unit_models, ## get slope and intercept info
              join_by(id)) |>
    left_join(unit_si, ## get SI info
              join_by(id)) |>
    unnest_wider(model) |> ## unnest model vars for calcs
    mutate(si_quantity = ({{quantity}} * slope) + intercept) |>
    select(-c(id, ## drop unneeded cols so df looks same before function with added cols
              slope,
              intercept,
              type,
              category))


  return(dataframe)

}



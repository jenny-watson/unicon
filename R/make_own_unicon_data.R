#' @title Make package data using user's own addins
#' @description Calculate necessary dataframes used in other package functions
#' using user's own additional data. If using package's own data not necessary.
#' @param user_base_dir A file pathway where base unit .json files are stored.
#' Defaults to package data directory file.path("inst", "units", "base")
#' @param user_derived_dir A file pathway where derived unit .json files are stored.
#' Defaults to package data directory file.path("inst", "units", "derived")
#' @import dplyr tidyr stringr
#' @export

make_own_unicon_data = function(user_base_dir = NA,
                             user_derived_dir = NA) {

  ## allow user to add their own base data and bind to package data

  if(!is.na(user_base_dir)) {

    base_data = bind_rows(
      load_base_data(),
      load_base_data(user_base_dir))

  } else {

    base_data = load_base_data()

  }

  ## allow user to add their own derived data and bind to package data

  if(!is.na(user_derived_dir)) {

    derived_data = bind_rows(
      load_derived_data(),
      load_derived_data(user_derived_dir))

  } else {

    derived_data = load_derived_data()

  }

  ## load operators data
  operators_data <- imap_dfr(
    load_json_files(operators_dir),
    ~ tibble(
      operator = .y,
      id = .x$id,
      fun = .x$fun,
      alias = .x$alias
    )
  )

  # category relationships
  ## unique categories
  u_cat <- base_data |>
    distinct(category) |>
    bind_rows(
      derived_data |>
        pivot_longer(
          cols = -operator,
          names_to = "type",
          values_to = "category"
        )
    ) |>
    distinct(category)

  ## one copy of each relationship
  cat_rel <- derived_data |>
    select(
      den_1 = id,
      num = x,
      den_2 = y
    )

  ## all copies of relationships with operators
  category_relationships <- bind_rows(
    left_join(
      u_cat,
      cat_rel,
      by = c("category" = "num")
    ),
    left_join(
      u_cat,
      cat_rel,
      by = c("category" = "den_1")
    ),
    left_join(
      u_cat,
      cat_rel,
      by = c("category" = "den_2")
    )
  ) |>
    # get rid of blank joins
    filter(!(is.na(num) & is.na(den_1) & is.na(den_1))) |>
    mutate(
      operator = if_else(is.na(num),
                         "multiply",
                         "divide"
      ),
      uid = 1:n()
    ) |>
    # remove blank parent cells
    pivot_longer(
      cols = c(
        num,
        den_1,
        den_2
      ),
      names_to = "type",
      values_to = "parent_metric"
    ) |>
    filter(!is.na(parent_metric)) |>
    # assign so correct order (matters for divide relationships)
    mutate(
      parent_type = case_when(
        operator == "multiply" & type == "den_1" ~ "parent_1",
        operator == "multiply" & type == "den_2" ~ "parent_2",
        operator == "divide" & type == "num" ~ "parent_1",
        operator == "divide" & type == "den_1" ~ "parent_2",
        operator == "divide" & type == "den_2" ~ "parent_2"
      )
    ) |>
    pivot_wider(
      id_cols = c(
        uid,
        category,
        operator
      ),
      names_from = parent_type,
      values_from = parent_metric
    ) |>
    select(
      category,
      parent_1,
      operator,
      parent_2
    ) |>
    distinct() # removes length * length = area duplicate

  ## join datasets together
  staging_join <- derived_data |>
    ## join to x
    left_join(
      base_data |>
        filter(intercept == 0) |> ## is this needed?
        rename_with(~ paste0(., ".x")),
      by = c("x" = "category.x"),
      relationship = "many-to-many"
    ) |>
    ## join to y
    left_join(
      base_data |>
        filter(intercept == 0) |> ## is this needed?
        rename_with(~ paste0(., ".y")),
      by = c("y" = "category.y"),
      relationship = "many-to-many"
    ) |>
    ## join to operators
    left_join(
      operators_data |>
        rename_with(~ paste0(., ".o")),
      by = c("operator" = "operator.o"),
      relationship = "many-to-many"
    ) |>
    ## format and calculate
    mutate(
      category = id,
      id = paste0(id.x, id.o, id.y),
      alias = paste0(alias.x, alias.o, alias.y), # problem per has no spaces?
      si = paste0(si.x, id.o, si.y),
      slope = slope.x / slope.y,
      intercept = 0,
      type = "derived",
      .keep = "none"
    ) |>
    ## bind to base data
    bind_rows(
      base_data |>
        mutate(type = "base")
    ) |>
    ## doesn't work for all if metric is derived twice or derived via multiply
    filter(
      !is.na(slope),
      category != "acceleration", # wrong units from mass and force
      !(category == 'area' & si == 'litre__m'),
      !(category == 'length' & si == 'ha__m')
    )

  # make corrections,
  join <- bind_rows(
    staging_join,
    ## correct acceleration
    category_relationships |>
      filter(
        category == "acceleration",
        parent_1 == "speed"
      ) |>
      left_join(
        staging_join |>
          rename_with(~ paste0(., ".x")),
        by = c("parent_1" = "category.x"),
        relationship = "many-to-many"
      ) |>
      left_join(
        staging_join |>
          rename_with(~ paste0(., ".y")),
        by = c("parent_2" = "category.y"),
        relationship = "many-to-many"
      ) |>
      left_join(
        operators_data |>
          rename_with(~ paste0(., ".o")),
        by = c("operator" = "operator.o"),
        relationship = "many-to-many"
      ) |>
      mutate(
        category,
        id = paste0(id.x, id.o, id.y),
        alias = paste0(alias.x, alias.o, alias.y), # problem per has no spaces?
        si = paste0(si.x, id.o, si.y),
        slope = slope.x / slope.y,
        intercept = 0,
        type = "derived",
        .keep = "none"
      ),
    ## correct mass flow rate
    ### first get all unit combinations
    category_relationships |>
      filter(category == "mass_flow_rate") |>
      left_join(
        staging_join |>
          rename_with(~ paste0(., ".x")),
        by = c("parent_1" = "category.x"),
        relationship = "many-to-many"
      ) |>
      left_join(
        staging_join |>
          rename_with(~ paste0(., ".y")),
        by = c("parent_2" = "category.y"),
        relationship = "many-to-many"
      ) |>
      select(
        category,
        starts_with("id"),
        starts_with("alias"),
        starts_with("si"),
      ) |>
      mutate( # get last bit of x and first bit of y then mass/time # this is slow
        id.xy = sub(".*__", "", id.x),
        id.yx = sub("__.*", "", id.y),
        alias.xy = case_when(
          str_detect(alias.x, "per") ~ sub(".*per", "", alias.x),
          str_detect(alias.x, "/") ~ sub(".*/", "", alias.x)
        ),
        alias.yx = case_when(
          str_detect(alias.y, "per") ~ sub("per.*", "", alias.y),
          str_detect(alias.y, "/") ~ sub("/.*", "", alias.y)
        ),
        si.xy = sub(".*__", "", si.x),
        si.yx = sub("__.*", "", si.y),
        # to get proper naming convention
        operator = "divide"
      ) |>
      select(-c(
        id.x, # reduce df size significantly
        id.y,
        alias.x,
        alias.y,
        si.x,
        si.y
      )) |>
      distinct() |>
      rename_with(~ sub("\\.yx$", ".x", .x), ends_with(".yx")) |> # make easier to read
      rename_with(~ sub("\\.xy$", ".y", .x), ends_with(".xy")) |>
      left_join(
        operators_data |>
          rename_with(~ paste0(., ".o")),
        by = c("operator" = "operator.o"),
        relationship = "many-to-many"
      ) |>
      mutate( # naming convention
        category,
        id = paste0(id.x, id.o, id.y),
        alias = paste0(alias.x, alias.o, alias.y), # problem per has no spaces?
        si = paste0(si.x, id.o, si.y),
        intercept = 0,
        type = "derived",
      ) |>
      ### second, calculate slope
      left_join(
        base_data |>
          select(alias,
                 slope.x = slope
          ),
        by = c("alias.x" = "alias")
      ) |>
      left_join(
        base_data |>
          select(alias,
                 slope.y = slope
          ),
        by = c("alias.y" = "alias")
      ) |>
      mutate(
        category,
        id,
        alias,
        si,
        slope = slope.x / slope.y,
        intercept,
        type,
        .keep = "none"
      )
  )

  ## final datasets

  # alias
  ## make sure complete and clean
  unit_alias <- join |>
    distinct(id, alias) |> ## ensure all unique
    ## add in folder name as alias to ensure all combinations captured
    bind_rows(
      join |>
        distinct(id) |>
        mutate(alias = id)
    ) |>
    distinct() |>
    arrange(id) |>
    # remove whitespace and upper case
    mutate(alias = str_replace_all(str_to_lower(alias), "\\s+", ""))

  # standard units
  unit_si <- join |>
    distinct(
      id,
      type,
      category,
      si
    )

  # models
  ## add in blanks
  unit_models <- join |>
    distinct(
      id,
      slope,
      intercept
    ) |>
    bind_rows(
      tibble(id = NA, slope = NA, intercept = NA)
    ) |>
    ## to get list back
    nest(model = c(slope, intercept)) |>
    # make into list rather than mini dataframes
    mutate(model = map(model, ~ as.list(.x)))

  ## write new package data

  saveRDS(unit_alias, 'pkg_data/unit_alias.RDS')
  saveRDS(unit_models, 'pkg_data/unit_models.RDS')
  saveRDS(unit_si, 'pkg_data/unit_si.RDS')
  saveRDS(category_relationships, 'pkg_data/category_relationships.RDS')

}

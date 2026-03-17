t = base_data |>
  distinct(category) |>
  bind_rows(
    derived_data |>
      pivot_longer(cols = -operator,
                   names_to = 'type',
                   values_to = 'category') |>
      distinct(category)
  ) |>
  distinct(category)


d = derived_data |>
  select(den_1 = id,
         num = x,
         den_2 = y)

j = bind_rows(
  left_join(t,
            d,
            by = c('category' = 'num')),
  left_join(t,
            d,
            by = c('category' = 'den_1')),
  left_join(t,
            d,
            by = c('category' = 'den_2'))) |>
  filter(!(is.na(num) & is.na(den_1) & is.na(den_1))) |>
  mutate(operator = if_else(is.na(num),
                            'multiply',
                            'divide'),
         uid = 1:n()) |>
  pivot_longer(cols = c(num,
                       den_1,
                       den_2),
              names_to = 'type',
              values_to = 'parent_metric') |>
  filter(!is.na(parent_metric)) |>
  mutate(parent_type = case_when(operator == 'multiply' & type == 'den_1' ~ 'parent_1',
                                 operator == 'multiply' & type == 'den_2' ~ 'parent_2',
                                 operator == 'divide' & type == 'num' ~ 'parent_1',
                                 operator == 'divide' & type == 'den_1' ~ 'parent_2',
                                 operator == 'divide' & type == 'den_2' ~ 'parent_2'))

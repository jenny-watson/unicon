

df = data.frame(info = letters[1:10],
                length = c(rep('inch', 10)),
                length_value = c(10:19),
                temp = c(rep('k', 10)),
                temp_value = c(360:369),
                area = c(rep('acre', 10)),
                area_value = c(200:209))


pairs <- tibble::tibble(
  measure  = c("length", "temp", "area"),
  quantity = c("length_value", "temp_value", "area_value")
)

## this doesnt work, creates one si and si_quantity column not paired cols per measure

test = purrr::pmap_dfr(
  pairs,
  ~ unit_conv(df, measure = ..1, quantity = ..2)
)

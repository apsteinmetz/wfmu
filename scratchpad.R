# Use `pizzaplace` to create a gt table
# with grouped data; add a summary with the
# `summary_rows()` function and then add a
# footnote to the "peppr_salami" row group
# label with `tab_footnote()` and with
# `cells_group()` in `locations`
tab_3 <-
  pizzaplace %>%
  dplyr::filter(
    name %in% c("soppressata", "peppr_salami")
  ) %>%
  dplyr::group_by(name, size) %>%
  dplyr::summarize(
    `Pizzas Sold` = n()
  )
tab_3 %>%   gt(rowname_col = "size") %>%
  summary_rows(
    groups = TRUE,
    columns = vars("Pizzas Sold"),
    fns = list(TOTAL = "sum"),
    formatter = fmt_number,
    decimals = 0,
    use_seps = TRUE
  ) 

%>%
  
  
  tab_footnote(
    footnote = "The Pepper-Salami.",
    cells_group(groups = "peppr_salami")
  )


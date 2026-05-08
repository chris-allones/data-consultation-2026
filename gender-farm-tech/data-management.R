# data management
## names for selected columns
df1 <- read_excel("data/ama-raw-data.xlsx", 2) |>
  select(variables) |>
  mutate(id = NA) |>
  distinct(variables, .keep_all = TRUE) |>
  pivot_wider(
    names_from = variables,
    values_from = id
  )

## full dataset
df0 <- read_excel("data/ama-raw-data.xlsx")

## combining
common_cols <- intersect(names(df1), names(df0))
df <- df0 |> select(all_of(common_cols))


df |> glimpse()

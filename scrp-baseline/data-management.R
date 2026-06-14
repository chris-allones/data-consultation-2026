## set-up
theme_1 <-
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )


## reading data
scrp_data_colnames <-
  read_excel("data/scrp-baseline-training-bohol-hilongos.xlsx", sheet = 3) |>
  colnames()

df <- read_excel(
  "data/scrp-baseline-training-bohol-hilongos.xlsx",
  sheet = 3
) |>
  clean_names() |>
  rename(
    "gender" = starts_with("gender"),
    "civil_status" = contains("civil"),
    "education_attainment" = contains("educational"),
    "hh_head" = contains("head"),
    "tenurial_stat" = contains("tenurial"),
    "price_per_kilo" = contains("sold"),
    "quantity_sold" = contains("how_much_per_sack"),
    "annual_revenue" = contains("gross_income"),
    "org_membership" = contains("member_of_org"),
    "cra_knowledge" = contains("knowledge_in_cra"),
  ) |>
  mutate(
    location = if_else(str_detect(address, "Bohol|bohol"), "Bohol", "Leyte")
  ) |>
  mutate(
    education_attainment = education_attainment |>
      recode_values(
        0 ~ "Elementary level",
        1 ~ "Elementray graduate",
        2 ~ "High school level",
        3 ~ "High school graduate",
        4 ~ "College level",
        5 ~ "Vocational",
        6 ~ "College graduate",
        7 ~ "Graduate studies",
        8 ~ "ALS"
      )
  ) |>
  mutate(
    education_attainment = factor(
      education_attainment,
      levels = c(
        "Elementary level",
        "Elementray graduate",
        "High school level",
        "High school graduate",
        "College level",
        "Vocational",
        "College graduate",
        "Graduate studies",
        "ALS"
      )
    )
  ) |>
  mutate(
    civil_status = civil_status |>
      recode_values(
        0 ~ "Single",
        1 ~ "Married",
        2 ~ "Live-in",
        3 ~ "Widow/er"
      )
  ) |>
  mutate(gender = if_else(gender == 1, "Male", "Female")) |>
  mutate(hh_head = if_else(hh_head == 0, "Husband", "Wife")) |>
  mutate(
    tenurial_stat = tenurial_stat |>
      recode_values(
        0 ~ "Tenant",
        1 ~ "Owned",
        2 ~ "Rented",
        3 ~ "Mortgaged",
        4 ~ "Free",
        5 ~ "Communal"
      )
  ) |>
  select(-contact_number, -name, -address)


## data for yield and production

df_harvest <-
  df |>
  rownames_to_column(var = "id") |>
  filter(!is.na(location)) |>
  filter(id != 12) |> # remove outlier
  mutate(avg_yield_ha = average_yield_per_cropping_sack / farm_size_ha) |>
  select(id, location, avg_yield_ha, cra_knowledge) |>
  mutate(id = factor(id, levels = 1:45))

## Compute group means with chosen x positions
df_mean_group_harvest <-
  df_harvest |>
  group_by(location) |>
  summarise(mean_group_havest = mean(avg_yield_ha), .groups = "drop") |>
  mutate(
    x_pos = case_when(
      location == "Bohol" ~ 11,
      location == "Leyte" ~ 37
    )
  )

# Separate mean data for each location
df_mean_bohol <- df_mean_group_harvest |> filter(location == "Bohol")
df_mean_leyte <- df_mean_group_harvest |> filter(location == "Leyte")
n_leyte <- nrow(df_harvest |> filter(location == "Leyte"))
n_bohol <- nrow(df_harvest |> filter(location == "Bohol"))


## data for yield by CRA knowledge
df_cra_knowledge_harvest <-
  df_harvest |>
  group_by(location, cra_knowledge) |>
  summarise(
    avg_yield_ha = mean(avg_yield_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    x_pos = case_when(
      location == "Bohol" ~ 11,
      location == "Leyte" ~ 37
    )
  )

df_mean_bohol_2 <- df_cra_knowledge_harvest |>
  filter(location == "Bohol") |>
  na.omit()
df_mean_leyte_2 <- df_cra_knowledge_harvest |> filter(location == "Leyte")
n_no_cra_leyte <- df_harvest |>
  filter(location == "Leyte") |>
  filter(cra_knowledge == 0) |>
  nrow()

n_with_cra_leyte <- df_harvest |>
  filter(location == "Leyte") |>
  filter(cra_knowledge == 1) |>
  nrow()

n_no_cra_bohol <- df_harvest |>
  filter(location == "Bohol") |>
  filter(cra_knowledge == 0) |>
  nrow()

n_with_cra_bohol <- df_harvest |>
  filter(location == "Bohol") |>
  filter(cra_knowledge == 1) |>
  nrow()

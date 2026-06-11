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

df |> glimpse()

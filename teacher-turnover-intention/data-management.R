# default theme setting

# custom theme
custom_theme <-
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      margin = margin(b = 15),
      face = "bold"
    ),
    plot.title.position = "panel",
    plot.subtitle = element_text(
      color = "gray40",
      margin = margin(b = 15),
      size = 12
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )



# loading data
## cleaning data
df <- read_excel("data/turnover-data.xlsx") |>
  clean_names() |>
  mutate(
    age = str_extract(age, "\\d+"),
    age = as.numeric(age),
    weekly_teaching_hours = str_extract(weekly_teaching_hours, "\\d+"),
    weekly_teaching_hours = as.numeric(weekly_teaching_hours),
    highest_educational_attainment = case_when(
                                    str_detect(highest_educational_attainment, "College") ~ "College degree",
                                    str_detect( highest_educational_attainment, "Associate") ~ "Associate course",
                                    TRUE ~ highest_educational_attainment),
    num = as.numeric(str_extract(experience, "\\d+")),    
    unit = case_when(
      str_detect(str_to_lower(experience), "month|months") ~ "month",
      str_detect(str_to_lower(experience), "yr|year|years|year's") ~ "year",
      TRUE ~ "year"),
    experience_years = case_when(
      unit == "month" ~ num / 12,
      unit == "year"  ~ num,
      TRUE ~ num),
    experience_years = if_else(experience_years < 0.1, 21/12, experience_years)
  ) |> 
  mutate(
    # Lowercase for consistency
    monthly_income = str_to_lower(monthly_income),
    # Remove peso signs, commas, "p", "php", "₱", "pesos", "usd", words
    monthly_income = str_replace_all(monthly_income, "₱|php|p|pesos|usd", ""),
    monthly_income = str_replace_all(monthly_income, "[^0-9k\\- ]", ""),
    monthly_income = str_trim(monthly_income),
    monthly_income = str_replace_all(monthly_income, "(\\d+)\\s*k", "\\1000"),
    income_value = case_when(
      str_detect(monthly_income, "-") ~ {
        nums <- str_extract_all(monthly_income, "\\d+")[[1]]
        mean(as.numeric(nums))
      },
      TRUE ~ as.numeric(str_extract(monthly_income, "\\d+"))
    ),
    income_value = if_else(income_value <= 30, income_value * 1000, income_value)
  ) |> 
  select(-num, -unit, -experience, -monthly_income) |> 
  relocate(experience_years, .after = highest_educational_attainment) |> 
  relocate(income_value, .after = esl_platform)

## data for ESL platform used
df_els_platform <- df |>
  separate_rows(esl_platform, sep = ",|&|and|/") |>
  mutate(esl_platform = str_trim(esl_platform)) |>
  mutate(esl_platform = str_to_lower(esl_platform)) |>
  mutate(esl_platform = case_when(
    str_detect(esl_platform, "team") ~ "teams",
    str_detect(esl_platform, "class") ~ "classin",
    str_detect(esl_platform, "zoom") ~ "zoom",
    str_detect(esl_platform, "voov") ~ "voov",
    str_detect(esl_platform, "gmeet|google meet") ~ "google meet",
    str_detect(esl_platform, "skype") ~ "skype",
    str_detect(esl_platform, "palfish") ~ "palfish",
    str_detect(esl_platform, "qq") ~ "qq",
    str_detect(esl_platform, "company") ~ "company platform",
    TRUE ~ esl_platform
  )) |> 
  select(esl_platform)


## likert items data
### statements
lkrt_statements <-
  read_excel("data/turnover-data.xlsx", 2) |>
  clean_names() |>
  mutate(item = str_to_lower(items)) |>
  select(factor, statement, item) |> 
  mutate(factor = if_else(str_detect(factor, "Workpla"), "Workplace perception", factor))

lkrt_dta <-
  df |>
  select(ta1:ti4) |>
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |>
  count(item, response) |>
  group_by(item) |>
  mutate(percent = n / sum(n)) |>
  left_join(lkrt_statements, by = "item") |>
  relocate(c(factor, statement), .before = item) |>
  ungroup() |>
  mutate(
    response = factor(
      response,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Strongly Disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly Agree"
      )
    ),
    pct_lab = str_c(round(percent * 100, 0))
  ) |>
  mutate(response = fct_rev(response))


### plot factor
plot_factor <-
  function(factor_name, stwidth = 40) {
    lkrt_dta |>
      mutate(statement = str_wrap(statement, width = stwidth)) |>
      filter(str_detect(factor, factor_name)) |>
      ggplot(aes(percent, statement, fill = response)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = pct_lab),
        position = position_fill(vjust = 0.5),
        color = "white",
        fontface = "bold"
      ) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(
        values = c("#2c6e49", "#4c956c", "#d6cfcb", "#ffc9b9", "#d68c45")
      ) +
      guides(
        fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)
      ) +
      facet_wrap(~factor) +
      labs(
        fill = NULL,
        x = NULL,
        y = NULL
      ) +
      custom_theme
  }

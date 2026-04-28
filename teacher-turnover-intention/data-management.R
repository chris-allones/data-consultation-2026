# loading data
df <- read_excel("data/turnover-data.xlsx") |>
  clean_names() |>
  mutate(
    age = str_extract(age, "\\d+"),
    age = as.numeric(age),
    weekly_teaching_hours = str_extract(weekly_teaching_hours, "\\d+"),
    weekly_teaching_hours = as.numeric(weekly_teaching_hours)
  )

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

## data cleaning
df |> glimpse()
df |> count(highest_educational_attainment) |> View()

df |>
  mutate(
    highest_educational_attainment = case_when(
      str_detect(highest_educational_attainment, "College") ~ "College degree",
      str_detect(
        highest_educational_attainment,
        "Associate"
      ) ~ "Associate course",
      TRUE ~ highest_educational_attainment
    )
  ) |>
  count(highest_educational_attainment)


## likert items data
### statements
lkrt_statements <-
  read_excel("data/digi-fin-data.xlsx", 2) |>
  clean_names() |>
  mutate(item = str_to_lower(code)) |>
  rename(
    "statement" = questions
  ) |>
  select(factor, statement, item)

lkrt_dta <-
  df |>
  select(bks1:fa4) |>
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
  function(factor_name, stwidth = 50) {
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

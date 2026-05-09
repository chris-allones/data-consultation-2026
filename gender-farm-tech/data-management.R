# data management
## names for selected columns
df <- read_excel("data/ama-raw-data-with var.xlsx") |>
  clean_names() |>
  mutate(
    ethnicity = if_else(str_detect(ethnicity, "waray"), "waray", ethnicity)
  ) |>
  mutate(
    nonfarm_income = as.numeric(nonfarm_income),
    nonfarm_income = if_else(is.na(nonfarm_income), 0, nonfarm_income),
    nonfarm_income = if_else(nonfarm_income == 0, NA, nonfarm_income),
    offfarm_income_yr = if_else(offfarm_income_yr == 0, NA, offfarm_income_yr),
    land_type = if_else(str_detect(land_type, "Lowland"), "Lowland", land_type)
  ) |>
  select(
    -religion
  )


#=======================================================

## setup
## custom theme
custom_theme <-
  theme_gray() +
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
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )


## funtions general
### separate multiple words in one colum with clear delimiter
str_separate <-
  function(df = df, var_name) {
    df |>
      separate_longer_delim({{ var_name }}, delim = ",") |>
      mutate(
        {{ var_name }} := str_to_lower({{ var_name }}),
        {{ var_name }} := str_remove_all({{ var_name }}, "\\([^)]*\\)"),
        {{ var_name }} := str_remove_all({{ var_name }}, "[0-9%]"),
        {{ var_name }} := str_remove_all({{ var_name }}, "[\r\n/]"),
        {{ var_name }} := str_remove_all({{ var_name }}, "[^a-z\\s]"),
        {{ var_name }} := str_squish({{ var_name }}),
        {{ var_name }} := str_trim({{ var_name }})
      )
  }

## percent label
pct_label <- function(df, count_var) {
  df |>
    mutate(
      pct = {{ count_var }} / sum({{ count_var }}, na.rm = TRUE),
      pct_lab = str_c(
        " ",
        round(pct * 100, 0),
        "% ",
        "(n = ",
        {{ count_var }},
        ")"
      )
    )
}


## functions specific
### bar plot by gender
plot_pct_by_sex <- function(df, category_var, ncol = 1, x_max = 1) {
  df |>
    select(sex, {{ category_var }}) |>
    count(sex, {{ category_var }}) |>
    na.omit() |>
    group_by(sex) |>
    pct_label(n) |>
    ungroup() |>
    mutate(
      {{ category_var }} := reorder_within({{ category_var }}, pct, sex)
    ) |>
    ggplot(aes(pct, {{ category_var }}, fill = sex)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = pct_lab), hjust = 0) +
    scale_y_reordered() +
    scale_x_continuous(
      limits = c(0, x_max),
      labels = scales::percent_format()
    ) +
    facet_wrap(~sex, scales = "free_y", ncol = ncol) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    custom_theme
}


## bar plot simplified

plot_bar_by_sex <- function(df, category_var, ncol = 1, x_max = 1) {
  df |>
    mutate(
      {{ category_var }} := reorder_within({{ category_var }}, pct, sex)
    ) |>
    ggplot(aes(pct, {{ category_var }}, fill = sex)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = pct_lab), hjust = 0) +
    scale_y_reordered() +
    scale_x_continuous(
      limits = c(0, x_max),
      labels = scales::percent_format()
    ) +
    facet_wrap(~sex, scales = "free_y", ncol = ncol) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    custom_theme
}

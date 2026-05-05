# loading data
df <- read_excel("data/saad-digi-marketing-dta.xlsx") |> 
  clean_names() |>
  mutate(
    education = str_to_lower(education),
    education = case_when(
      str_detect(education, "college grad",) ~ "College graduate",
      str_detect(education, "college le") ~ "College level",
      str_detect(education, "elementary grad|elementray grad") ~ "Elementary graduate",
      str_detect(education, "elementary le") ~ "Elementary level",
      str_detect(education, "high school grad|hs grad") ~ "High school graduate",
      str_detect(education, "high school le|high school") ~ "High school level",
      TRUE ~ education
    ),
    years_established = if_else(str_detect(years_established, "3-5"), "3-5 yrs", years_established),
    value_adding = if_else(str_detect(value_adding_processed_products_specify, "No"), "No", "Yes"),
    has_device = if_else(has_device == "Oo", "No", has_device)
  )



## separate descriptive
### commodity data
commodity_dta <- 
  df |> 
  select(commodity) |> 
  mutate(commodity = str_replace_all(commodity, ";", ",")) |>  # unify delimiters
  separate_longer_delim(commodity, delim = ",") |> 
  mutate(commodity = str_trim(commodity)) |> 
  mutate(commodity = str_to_lower(commodity))

### specific value-adding commodity
value_adding_spec_dta <- 
  df |> 
  select(value_adding_processed_products_specify) |> 
  mutate(
    value_adding_spec = str_replace_all(value_adding_processed_products_specify, ";", ",")
  ) |> 
  separate_longer_delim(value_adding_spec, delim = ",") |> 
  mutate(value_adding_spec = str_trim(value_adding_spec)) |>  
  filter(!value_adding_spec %in% c("No", "Yes")) |> 
  mutate(value_adding_spec = str_to_lower(value_adding_spec))


### market
market_dta <- 
  df |> 
  select(market) |> 
  separate_longer_delim(market, delim = ",") |> 
  mutate(market = str_trim(market)) |> 
  mutate(market = str_to_lower(market)) |> 
  mutate(
    market = if_else(str_detect(market, "insti"), "institutional buyer", market),
    market = if_else(market == "direct", "direct selling", market),
    market  = if_else(str_detect(market, "kadiwa"), "kadiwa", market)
  ) |> 
  count(market, sort = TRUE)

















# custom theme
custom_theme <- 
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b=15), face = "bold"),
        plot.title.position = "panel",
        plot.subtitle = element_text(color = "gray40", margin = margin(b=15), size = 12),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      )


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
  pivot_longer(cols = everything(),
               names_to = "item",
               values_to = "response") |>  
  count(item, response) |> 
  group_by(item) |>
  mutate(percent = n/sum(n)) |> 
  left_join(lkrt_statements, by = "item") |> 
  relocate(c(factor, statement), .before = item) |> 
  ungroup() |> 
  mutate(response = factor(response, levels = c(1, 2, 3, 4, 5), labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
         pct_lab = str_c(round(percent * 100, 0))) |> 
  mutate(response = fct_rev(response))
  


### plot factor
plot_factor <- 
  function(factor_name, stwidth = 50){
  lkrt_dta |> 
  mutate(statement = str_wrap(statement, width = stwidth)) |> 
  filter(str_detect(factor, factor_name)) |> 
  ggplot(aes(percent, statement, fill = response)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = pct_lab), position = position_fill(vjust = 0.5), color = "white", fontface = "bold") +
  scale_x_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#2c6e49", "#4c956c", "#d6cfcb", "#ffc9b9", "#d68c45")) +
  guides(fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)) +
  facet_wrap(~ factor) +
  labs(
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  custom_theme
}


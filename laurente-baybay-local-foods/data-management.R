# custom theme
custom_theme <- 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b=15), face = "bold"),
        plot.title.position = "panel",
        plot.subtitle = element_text(color = "gray40", margin = margin(b=15), size = 12),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      )

# importing dataset
local_food_dta <- 
  read_excel("data/baybay-local-foods-data.xlsx") |> 
  clean_names() |> 
  select(-timestamp, -do_you_consent_to_take_part_in_this_survey)

 
# data management

## awareness on local delicacies

### rice-based delicacies awareness
aware_rice_based_dta <- 
  local_food_dta |> 
  select(moron:maja_blanca) |> 
  pivot_longer(cols = moron:maja_blanca,
               names_to = "delicacy",
               values_to = "rating") |> 
  count(delicacy, rating) |> 
  na.omit() |> 
  group_by(delicacy) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(rate_lab = case_when(
         rating == 5 ~ "Highly aware",
         rating == 4 ~ "Aware",
         rating == 3 ~ "Neutral",
         rating == 2 ~ "Not aware",
         rating == 1 ~ "Strongly not aware",
         )) |> 
  mutate(rate_lab = factor(rate_lab, levels = c("Highly aware", "Aware", "Neutral", "Not aware", "Strongly not aware"))) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  mutate(pct_lab = round(pct * 100, 0)) |> 
  mutate(delicacy = case_when(str_detect(delicacy, "budbod") ~ "budbod (suman latik)",
                              str_detect(delicacy, "lidgid") ~ "lidgid (suman tumini)",
                              TRUE ~ delicacy))



### snacks and processed products
aware_snacks_process_dta <- 
  local_food_dta |> 
  select(buko_pie:peanut_triangle) |> 
  pivot_longer(cols = buko_pie:peanut_triangle,
               names_to = "delicacy",
               values_to = "rating") |> 
  count(delicacy, rating) |> 
  na.omit() |> 
  group_by(delicacy) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(rate_lab = case_when(
         rating == 5 ~ "Highly aware",
         rating == 4 ~ "Aware",
         rating == 3 ~ "Neutral",
         rating == 2 ~ "Not aware",
         rating == 1 ~ "Strongly not aware",
         )) |> 
  mutate(rate_lab = factor(rate_lab, levels = c("Highly aware", "Aware", "Neutral", "Not aware", "Strongly not aware"))) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  mutate(pct_lab = round(pct * 100, 0))


### dairy and specialty products
aware_beverage_dairy_dta <- 
  local_food_dta |>
  select(kamote_ice_cream:sikwate) |> 
  pivot_longer(cols = kamote_ice_cream:sikwate,
               names_to = "delicacy",
               values_to = "rating") |> 
  count(delicacy, rating) |> 
  na.omit() |> 
  group_by(delicacy) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(rate_lab = case_when(
         rating == 5 ~ "Highly aware",
         rating == 4 ~ "Aware",
         rating == 3 ~ "Neutral",
         rating == 2 ~ "Not aware",
         rating == 1 ~ "Strongly not aware",
         )) |> 
  mutate(rate_lab = factor(rate_lab, levels = c("Highly aware", "Aware", "Neutral", "Not aware", "Strongly not aware"))) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  mutate(pct_lab = round(pct * 100, 0))



## tastes

### rice-based delicacies
taste_rice_based_dta <- 
  local_food_dta |> 
  select(starts_with("taste_")) |> 
  pivot_longer(everything(), 
               names_to = "delicacy",
               values_to = "rating") |> 
  count(delicacy, rating) |> 
  na.omit() |> 
  group_by(delicacy) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(rate_lab = case_when(
         rating == 5 ~ "Highly aware",
         rating == 4 ~ "Aware",
         rating == 3 ~ "Neutral",
         rating == 2 ~ "Not aware",
         rating == 1 ~ "Strongly not aware",
         )) |> 
  mutate(rate_lab = factor(rate_lab, levels = c("Highly aware", "Aware", "Neutral", "Not aware", "Strongly not aware"))) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  mutate(delicacy = str_remove_all(delicacy, "taste ")) |> 
  mutate(pct_lab = round(pct * 100, 0)) |> 
  mutate(delicacy = case_when(str_detect(delicacy, "budbod") ~ "budbod (suman latik)",
                              str_detect(delicacy, "lidgid") ~ "lidgid (suman tumini)",
                              TRUE ~ delicacy))


### snacks and processed products
taste_snacks_process_data <- 
  local_food_dta |> 
  select(contains("_processed_products_taste_")) |> 
  pivot_longer(everything(),
               names_to = "delicacy",
               values_to = "rating") |> 
  mutate(delicacy = str_remove_all(delicacy, "snacks_processed_products_taste_")) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  count(delicacy, rating) |> 
  na.omit() |> 
  group_by(delicacy) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(rate_lab = case_when(
         rating == 5 ~ "Highly aware",
         rating == 4 ~ "Aware",
         rating == 3 ~ "Neutral",
         rating == 2 ~ "Not aware",
         rating == 1 ~ "Strongly not aware",
         )) |> 
  mutate(rate_lab = factor(rate_lab, levels = c("Highly aware", "Aware", "Neutral", "Not aware", "Strongly not aware"))) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  mutate(pct_lab = round(pct * 100, 0))


### dairy and specialty products
taste_beverage_dairy_dta <- 
  local_food_dta |> 
  select(starts_with("dairy_specialty_products_taste"),
         starts_with("beverage_taste")) |> 
  pivot_longer(everything(),
               names_to = "delicacy",
               values_to = "rating") |> 
  mutate(delicacy = str_remove_all(delicacy, "dairy_specialty_products_taste_")) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  count(delicacy, rating) |> 
  na.omit() |> 
  group_by(delicacy) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(rate_lab = case_when(
         rating == 5 ~ "Highly aware",
         rating == 4 ~ "Aware",
         rating == 3 ~ "Neutral",
         rating == 2 ~ "Not aware",
         rating == 1 ~ "Strongly not aware",
         )) |> 
  mutate(rate_lab = factor(rate_lab, levels = c("Highly aware", "Aware", "Neutral", "Not aware", "Strongly not aware"))) |> 
  mutate(delicacy = str_replace_all(delicacy, "_", " ")) |> 
  mutate(pct_lab = round(pct * 100, 0))


## Appearance

local_food_dta |> 
  glimpse()

local_food_dta |> 
  select(contains("_appearance_")) |> 
  glimpse()

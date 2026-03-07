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
### rice-based delicacies
appearance_rice_based_dta <- 
  local_food_dta |> 
  select(contains("_appearance_")) |> 
  select(starts_with("rice_based")) |> 
  pivot_longer(everything(), 
               names_to = "delicacy",
               values_to = "rating") |> 
  mutate(delicacy = str_remove_all(delicacy, "^.*appearance_")) |> 
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
  mutate(delicacy = str_remove_all(delicacy, "taste ")) |> 
  mutate(pct_lab = round(pct * 100, 0)) |> 
  mutate(delicacy = case_when(str_detect(delicacy, "budbod") ~ "budbod (suman latik)",
                              str_detect(delicacy, "lidgid") ~ "lidgid (suman tumini)",
                              TRUE ~ delicacy))


### snacks and processed products

appearance_snacks_process_dta <- 
  local_food_dta |> 
  select(contains("_appearance_")) |> 
  select(starts_with("snacks_processed")) |> 
  pivot_longer(everything(),
               names_to = "delicacy",
               values_to = "rating") |> 
  mutate(delicacy = str_remove_all(delicacy, "^.*appearance_")) |> 
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
  mutate(delicacy = str_remove_all(delicacy, "taste ")) |> 
  mutate(pct_lab = round(pct * 100, 0)) |> 
  ungroup()


### dairy and specialty products

appearance_beverage_dairy_dta <- 
  local_food_dta |> 
  select(contains("_appearance_")) |> 
  select(starts_with("dairy"), starts_with("beverage")) |> 
  pivot_longer(everything(),
               names_to = "delicacy",
               values_to = "rating") |> 
  mutate(delicacy = str_remove_all(delicacy, "^.*appearance_")) |> 
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
  mutate(delicacy = str_remove_all(delicacy, "taste ")) |> 
  mutate(pct_lab = round(pct * 100, 0)) |> 
  ungroup()



## willingness to support gastronomic tourism
### activity participation
activity_part_dta <- 
  local_food_dta |> 
  select(food_festivals_promoting_baybay_local_food:none) |>
  pivot_longer(everything(),
               names_to = "activity",
               values_to = "count") |> 
  mutate(activity = str_replace_all(activity, "_", " ")) |> 
  group_by(activity) |> 
  summarise(n = sum(count, na.rm = TRUE)) |> 
  mutate(pct = n / nrow(local_food_dta)) |> 
  mutate(pct_lab = str_c(round(pct * 100, 2), "%", "(n=", n, ")"))
  
### recommendation local delicacies
reco_local_delicacy_dta <- 
  local_food_dta |> 
  select(x2_would_you_like_to_recommend_baybay_s_local_food_to_tourists) |> 
  rename("recommend" = x2_would_you_like_to_recommend_baybay_s_local_food_to_tourists) |> 
  mutate(recommend = if_else(recommend == "Defnitely", "Definitely", recommend)) |> 
  count(recommend) |> 
  na.omit() |> 
  mutate(pct = n / sum(n)) |> 
  mutate(pct_lab = str_c(round(pct * 100, 2), "%", "(n=", n, ")"))



## likert perceptions
read_excel("data/baybay-local-foods-data.xlsx", 2) |> 
  mutate(description = str_extract(desciption, "\\[.*?\\]")) |> 
  mutate(description = str_remove_all(description, "\\[|\\]")) |> 
  View()


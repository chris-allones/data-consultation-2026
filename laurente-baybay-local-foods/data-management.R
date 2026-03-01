# set working directory
setwd(here::here("laurente-baybay-local-foods"))

# libraries
library(tidyverse)
library(readxl)
library(janitor)
library(scales)

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


local_food_dta |> glimpse()

# awareness on local delicacies
## rice-based delicacies awareness
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
  mutate(pct_lab = round(pct * 100, 0))

aware_rice_based_dta |> 
  ggplot(aes(pct, delicacy, fill = rate_lab)) +
  geom_col() +
  geom_text(aes(label = pct_lab), position = position_fill(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#073b4c", "#118ab2", "#06d6a0", "#ffba66ff", "#e63946")) +
  scale_x_continuous(labels = percent_format()) +
  custom_theme +
  labs(
    title = "Rice-based delicacies level of awareness",
    fill =  NULL,
    y = NULL,
    x = NULL
  )

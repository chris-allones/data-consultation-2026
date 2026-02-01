## libraries
library(tidyverse)
library(janitor)
library(readxl)

# custom theme
custom_theme <- 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 16, margin = margin(b=15), face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "gray40", margin = margin(b=15), size = 12),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      )

# data management
## import data

likert_res_dta <- 
  read_excel("data/rage-bait-data.xlsx") |> 
  clean_names() |> 
  mutate(are5 = if_else(are5 == "Dsiagree", "Disagree", are5))

fct_likert_dta <- 
  likert_res_dta |> 
  select(-respondents, -sex) |> 
  mutate(across(.cols = everything(), 
.fns = ~ case_when(.x == "Strongly Agree" ~ 5,
                   .x == "Agree" ~ 4,
                   .x == "Neutral" ~ 3,
                   .x == "Disagree" ~ 2,
                   .x == "Strongly Disagree" ~ 1,
                   TRUE ~ as.numeric(.x)
                  )
                )
        )

socio_demo_dta <- 
  read_excel("data/rage-bait-data.xlsx", sheet = 2) |> 
  clean_names()

likert_statement_dta <- 
  read_excel("data/rage-bait-data.xlsx", sheet = 3) |> 
  clean_names() |> 
  mutate(
    statement = str_trim(statement, "both"),
    categories = str_to_title(categories),
    item = str_to_lower(item)
  )


## by social media platform
soc_med_dta <- 
  socio_demo_dta |> 
  select(sex, starts_with("social")) |> 
  separate_longer_delim(cols = social_media, delim = ",") |> 
  mutate(social_media = str_trim(social_media, "both")) 


## category for soc med
soc_med_category_sub_dta <- 
  soc_med_dta |> 
  mutate(social_media_content = str_remove_all(social_media_content, "\\s*\\([^)]*\\)")) |> 
  separate_longer_delim(cols = social_media_content, delim = ",") |> 
  mutate(social_media_content = str_trim(social_media_content, "both")) |> 
  count(social_media_content, sort = T) |> 
  filter(n > 10)

## soc med category
soc_med_category <-
  soc_med_dta |> 
  mutate(social_media_content = str_remove_all(social_media_content, "\\s*\\([^)]*\\)")) |> 
  separate_longer_delim(cols = social_media_content, delim = ",") |> 
  mutate(social_media_content = str_trim(social_media_content, "both"))

## combining soc med categories
soc_med_content_dta <- 
  soc_med_category_sub_dta |> 
  left_join(soc_med_category) |> 
  select(-n)


## likert statement proportions
fct_response_dta <- 
  likert_res_dta |>
  select(rcr1:are6) |> 
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |> 
  left_join(likert_statement_dta, by = "item") |> 
  relocate(categories, .before = item) |> 
  mutate(response = if_else(response == "Dsiagree", "Disagree", response)) |> # correnting typo error in the dataset
  count(categories, statement, response) |>
  group_by(statement) |> 
  mutate(pct = n / sum(n)) |> 
  ungroup() |> 
  na.omit()
  
## creating function to plot likert responses
plt_rage_fct <- function(category){
  fct_response_dta |> 
  mutate(fct_response = factor(response, levels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree"))) |> 
  mutate(
    statement = str_wrap(statement, 50),
    pct_label = round(pct * 100, 0)
  ) |> 
  filter(str_detect(categories, category)) |> 
  ggplot(aes(pct, statement, fill = fct_response)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = pct_label), position = position_fill(vjust = 0.5), color = "white", fontface = "bold") +
  scale_x_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("#073b4c", "#118ab2", "#06d6a0", "#ffba66ff", "#e63946")) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ categories) +
  labs(
    y = NULL,
    x = NULL,
    fill = NULL
  ) +
  custom_theme
}




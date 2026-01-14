## set working directory
setwd(here::here("ovpaf-survey"))

## load libraries
library(tidyverse)
library(readxl)
library(janitor)


## load data
data_raw <- read_excel("data/vsu-safety-resilience.xlsx") |> 
  clean_names()

## likert data and statement
likert_statement_dta <- read_excel("data/vsu-safety-resilience.xlsx", 
           sheet = 2) |> 
  clean_names()

item_statement_dta <- 
  data_raw |> 
  select(position:s15) |> 
  pivot_longer(cols = s1:s15,
               names_to = "statement",
               values_to = "response") |> 
  left_join(likert_statement_dta,
            by = c("statement" = "item")) |> 
  rename("description" = statement.y) |> 
  count(position, categories, description, response) |> 
  group_by(position, categories, description) |>
  mutate(total = sum(n),
         perc = n/total) |> 
  ungroup() |>
  mutate(description = str_wrap(description, width = 60)) |> 
  mutate(response_fct = factor(response, 
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Strongly Disagree", 
                                          "Disagree", 
                                          "Neutral", 
                                          "Agree", 
                                          "Strongly Agree"))) |> 
  mutate(response_fct = fct_rev(response_fct))


## plotting statement
item_statement_dta |> 
  filter(str_detect(categories, "Emotional")) |>
  ggplot(aes(x = perc, y = description, fill = response_fct)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white",
            size = 5) +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_wrap(~position) +
  labs(title = "Perception on Safety and Resilience Statements",
       x = "Percentage",
       y = "Statement",
       fill = "Response") +
  theme_minimal() +
  theme(legend.position = "bottom")

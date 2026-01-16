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
  mutate(description = str_wrap(description, width = 40)) |> 
  mutate(response_fct = factor(response, 
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Strongly Disagree", 
                                          "Disagree", 
                                          "Neutral", 
                                          "Agree", 
                                          "Strongly Agree"))) |> 
  mutate(response_fct = fct_rev(response_fct))



## student data
### load data
student_raw_dta <- 
  read_excel("data/vsu-safety-resilience.xlsx", 3) |> 
  clean_names() |> 
  remove_empty()


## likert data and statement
student_likert_statement_dta <- 
  read_excel("data/vsu-safety-resilience.xlsx", sheet = 4) |> 
  clean_names()

student_item_statement_dta <- 
  student_raw_dta |> 
  select(si1:si23) |> 
  pivot_longer(cols = everything(),
               names_to = "statement",
               values_to = "response") |> 
  left_join(student_likert_statement_dta,
            by = c("statement" = "item")) |> 
  rename("description" = statement.y) |> 
  na.omit() |> 
  count(category, description, response) |> 
  group_by(category, description) |>
  mutate(total = sum(n),
         perc = n/total) |> 
  ungroup() |>
  mutate(description = str_wrap(description, width = 40)) |> 
  mutate(response_fct = factor(response, 
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Strongly Disagree", 
                                          "Disagree", 
                                          "Neutral", 
                                          "Agree", 
                                          "Strongly Agree"))) |> 
  mutate(response_fct = fct_rev(response_fct))


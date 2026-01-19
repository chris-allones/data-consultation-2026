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


## guests and visitors
### load data
guest_raw_dta <- 
  read_excel("data/vsu-safety-resilience.xlsx", sheet = 5) |> 
  clean_names()

### student items statements

guest_items_dta <- 
  read_excel("data/vsu-safety-resilience.xlsx", sheet = 6) |> 
  clean_names()

guest_item_statement_dta <- 
  guest_raw_dta |> 
  select(s1:s5) |> 
  pivot_longer(cols = everything(),
               names_to = "statement",
              values_to = "response") |> 
  left_join(guest_items_dta, by = join_by("statement" == "item")) |> 
  rename("description" = statement.y) |> 
  count(description, response) |> 
  group_by(description) |> 
  mutate(pct = n/sum(n)) |> 
  ungroup() |> 
  mutate(response_fct = factor(response, levels = 1:4,
                        labels = c("Very poor", "Poor", "Good", "Very good"))) |> 
  mutate(response_fct = fct_rev(response_fct))


## data combine

data_raw |> 
  select(position, recommend:resident) |> 
  glimpse()

student_raw_dta |> 
  select(feedback:traffic_light) |> 
  glimpse()

guest_raw_dta |> 
  select(suggestion:other_spec) |> 
  glimpse()

######################################################

library(dplyr)

# Prepare each dataset with a common structure
faculty_staff <- data_raw |> 
  select(position, recommend, participation, participation_specify, feedback, traffic_light, age, sex, work_year, resident) |> 
  mutate(group = "Faculty & Staff")

student <- student_raw_dta |> 
  select(feedback, age, sex, year, location, safe_travel, safety_exper, traffic_light) |> 
  rename("resident" = location) |> 
  mutate(
    position = NA,
    recommend = NA,
    participation = NA,
    participation_specify = NA,
    work_year = NA,
    suggestion = NA,
    stakeholders = NA,
    other_spec = NA,
    group = "Student"
  )

guest <- guest_raw_dta |> 
  select(suggestion, feedback, traffic_light, age, sex, stakeholders, other_spec) |> 
  mutate(
    position = NA,
    recommend = NA,
    participation = NA,
    participation_specify = NA,
    work_year = NA,
    resident = NA,
    year = NA,
    location = NA,
    safe_travel = NA,
    safety_exper = NA,
    group = "Guest"
  )

# Combine all datasets
combined_data <- bind_rows(faculty_staff, student, guest)

# View structure
glimpse(combined_data)


combined_data |> 
  select(-recommend, -position, -participation, -participation_specify, -feedback, -work_year, -year, -safe_travel, -safety_exper, -suggestion, -stakeholders, -other_spec, -location) |> 
  tbl_summary(
    by = group
  )

## set working directory
setwd(here::here("rentaro-ai-effects-academic"))

## libraries
library(tidyverse)
library(readxl)
library(janitor)


## data management
ai_ms_dta <-
  read_excel("data/ai-data-labeled.xlsx", sheet = 2) |>
  clean_names() |>
  select(-c(timestamp:do_you_agree_to_participate_in_this_study)) |>
  rename(
    "degree" = graduate_degree_program_specialization,
    "year_level" = year_level_as_a_graduate_student,
    "grad_years" = number_of_years_as_a_graduate_student,
    "residence" = residence_address_ex_baybay_city,
    "working_experience" = years_of_working_experience,
    "weekly_allowance" = estimated_weekly_allowance_as_a_graduate_student,
    "gadget_own" = do_you_possess_any_gadgets,
    "gadget_own_type" = please_select_what_type_of_gadgets_you_own_that_you_use_as_a_graduate_student,
    "internet_availability" = do_you_have_the_internet_connection,
    "internet_source" = source_of_the_internet,
    "internet_provider" = where_do_you_get_the_internet_connection,
    "internet_monthly_budget" = estimated_monthly_budget_for_internet_connection,
    "ai_familiarity" = are_you_familiar_with_ai_tools,
    "ai_use_freq" = how_often_do_you_use_ai,
    "ai_info_source" = where_do_you_get_the_information_about_ai,
    "ai_training" = have_you_attended_any_ai_related_training,
    "ai_tools_used" = which_ai_tools_do_you_use_please_check_all_the_ai_tools_you_use
  )


## recode degree into program and major columns
ai_ms_dta <- ai_ms_dta |>
  mutate(
    degree_lower = str_to_lower(str_squish(degree)),
    program = case_when(
      str_detect(
        degree_lower,
        "magdev|m.*ag.*dev|master.*agri.*dev|master.*agricultural dev"
      ) ~ "MAgDev",
      str_detect(
        degree_lower,
        "m\\.? ?ed|^med$|master of education|master.*educ"
      ) ~ "MEd",
      str_detect(
        degree_lower,
        "^mm$|^mm[- ]|mmbm|management|master.*manag"
      ) ~ "MM",
      str_detect(degree_lower, "phd|doctor of philosophy|doctor") ~ "PhD",
      str_detect(
        degree_lower,
        "ms|master of science|master in science|masters of science"
      ) ~ "MS",
      # bare field names with no degree prefix -> assume MS
      !is.na(degree_lower) &
        !degree_lower %in% c("bsa", "bs chemistry", "mslt") ~ "MS",
      .default = NA_character_
    ),

    major = case_when(
      # MEd majors (check before generic field patterns)
      str_detect(
        degree_lower,
        "m.*ed.*bio|master.*educ.*bio|master of education in bio"
      ) ~ "Biology",
      str_detect(degree_lower, "m.*ed.*chem|master.*educ.*chem") ~ "Chemistry",
      str_detect(
        degree_lower,
        "m.*ed.*pe|m.*ed.*physical|physical ed"
      ) ~ "Physical Education",
      str_detect(degree_lower, "m.*ed.*eng|master.*educ.*eng") ~ "English",

      # fields shared across programs
      str_detect(
        degree_lower,
        "agri.*econ|agecon|ag.*econ|applied econ"
      ) ~ "Agricultural Economics",
      str_detect(degree_lower, "agri.*edu") ~ "Agricultural Education",
      str_detect(degree_lower, "agri.*ext|agex") ~ "Agricultural Extension",
      str_detect(degree_lower, "agron") ~ "Agronomy",
      str_detect(
        degree_lower,
        "animal sci|an sci|ansc|animal science"
      ) ~ "Animal Science",
      str_detect(degree_lower, "animal prod") ~ "Animal Production",
      str_detect(degree_lower, "chem") ~ "Chemistry",
      str_detect(degree_lower, "dev.*com|devcom") ~ "Development Communication",
      str_detect(
        degree_lower,
        "dev.*soc|devsoc|^msds$"
      ) ~ "Development Sociology",
      str_detect(degree_lower, "entomol|entom") ~ "Entomology",
      str_detect(degree_lower, "food sci|fst") ~ "Food Science and Technology",
      str_detect(degree_lower, "forest|^msf$|^msfor$") ~ "Forestry",
      str_detect(degree_lower, "horti") ~ "Horticulture",
      str_detect(
        degree_lower,
        "land admin|^mlam$"
      ) ~ "Land Administration and Management",
      str_detect(degree_lower, "plant breed") ~ "Plant Breeding",
      str_detect(
        degree_lower,
        "plant path|ppath|plant prot"
      ) ~ "Plant Pathology",
      str_detect(degree_lower, "soil sci|^master in soil") ~ "Soil Science",
      str_detect(degree_lower, "tropical ecol|^trec$") ~ "Tropical Ecology",
      str_detect(degree_lower, "biology") ~ "Biology",

      .default = NA_character_
    )
  ) |>
  select(-degree_lower)


ai_ms_dta |>
  select(program, major, everything()) |>
  View()
glimpse()

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
  select(-degree_lower) |>
  relocate(program:major, .after = degree)


## extracting graduate years
ai_ms_dta <- ai_ms_dta |>
  mutate(
    grad_years = case_when(
      is.na(grad_years) | grad_years == "n/a" ~ NA_real_,
      grad_years %in%
        c(
          "5 months",
          "4 months",
          "6 months/half a year",
          "half a year",
          "More than half",
          "0.5 years (half a year)",
          "1st semester",
          "1 semester"
        ) ~ 0.5,
      grad_years == "<1" ~ 0.5,
      grad_years == ">1" ~ 1.0,
      grad_years %in% c("Less than 1 year", "Less than one year") ~ 0.5,
      grad_years == "1 and half" ~ 1.5,
      grad_years == "three" ~ 3,
      TRUE ~ parse_number(grad_years)
    )
  ) |>
  # remove numbers in year level variable
  mutate(year_level = str_remove(year_level, "^\\d+\\s*")) |>
  # extract year of working experience
  mutate(
    working_experience_numeric = case_when(
      is.na(working_experience) |
        str_to_lower(working_experience) %in%
          c("n/a", "n.a.", "none", "na") ~ NA_real_,

      # month-only entries -> convert to fraction of year
      str_detect(str_to_lower(working_experience), "^1 month$") ~ 1 / 12,
      str_detect(str_to_lower(working_experience), "^4 months?$") ~ 4 / 12,
      str_detect(
        str_to_lower(working_experience),
        "^6 months?$|^6months$|^half year|^half a year"
      ) ~ 0.5,
      str_detect(str_to_lower(working_experience), "^8 months?$") ~ 8 / 12,
      str_detect(str_to_lower(working_experience), "^9 months?$") ~ 9 / 12,
      str_detect(str_to_lower(working_experience), "6 months as") ~ 0.5,

      # "Less than a year" / "More than 1 year"
      str_to_lower(working_experience) == "less than a year" ~ 0.5,
      str_to_lower(working_experience) == "more than 1 year" ~ 1.0,

      # fractional written forms
      working_experience == "1/3" ~ 1 / 3,
      working_experience %in%
        c("1 1/2 years", "1&1/2", "1 and half years") ~ 1.5,
      str_detect(str_to_lower(working_experience), "^1 year and 5 months") ~ 1 +
        5 / 12,
      str_detect(
        str_to_lower(working_experience),
        "^1 year and 6 months"
      ) ~ 1.5,
      str_detect(
        str_to_lower(working_experience),
        "^2 years and 4 months"
      ) ~ 2 + 4 / 12,
      str_detect(
        str_to_lower(working_experience),
        "^2 years and 7 months"
      ) ~ 2 + 7 / 12,
      str_detect(
        str_to_lower(working_experience),
        "^5 years and 6 months"
      ) ~ 5.5,

      # everything else: extract first number
      TRUE ~ parse_number(working_experience)
    )
  ) |>
  # arrange factor level
  mutate(
    year_level = factor(
      year_level,
      levels = c(
        "First Year",
        "Second Year",
        "Third Year",
        "Fourth Year",
        "Fifth Year"
      )
    ),
    employment_status = factor(
      employment_status,
      levels = c(
        "Full-time graduate student",
        "Working full-time graduate student",
        "Working part-time graduate student",
        "Others"
      )
    )
  )

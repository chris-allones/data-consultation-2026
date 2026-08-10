# setup working directory
setwd(here::here("viserdac-onboarding"))

#libraries
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

## import data
viserdac_dta <- 
  read_excel("viserdac-data.xlsx") |> 
  clean_names()


## date data cleaning

parse_messy_date <- function(x) {
  is_serial <- str_detect(x, "^\\d+$")
  from_serial <- as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")

  yr <- str_extract(x, "\\d{4}")
  x_noyr <- str_remove(x, "\\d{4}")
  mon <- str_extract(x_noyr, "^[A-Za-z]+") |> str_sub(1, 3)
  day <- str_extract(x_noyr, "\\d{1,2}")
  day <- if_else(is.na(day), "1", day)
  from_text <- dmy(paste(day, mon, yr), quiet = TRUE)

  if_else(is_serial, from_serial, from_text)
}

viserdac_dta <- viserdac_dta |>
  mutate(
    date_raw = str_trim(date),
    date_clean = parse_messy_date(date_raw)
  )


## annual cummulative number of training
### line plot
viserdac_dta |>
  mutate(year_clean = year(date_clean)) |>
  summarise(total_participants = sum(participant, na.rm = TRUE), .by = year_clean) |>
  arrange(year_clean) |>
  mutate(cumulative_participants = cumsum(total_participants)) |>
  ggplot(aes(x = year_clean, y = cumulative_participants)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Cumulative participants",
    title = "Cumulative training participants over time"
  )

## bar plot
viserdac_dta |>
  mutate(year_clean = year(date_clean)) |>
  summarise(total_participants = sum(participant, na.rm = TRUE), .by = year_clean) |>
  arrange(year_clean) |>
  mutate(cumulative_participants = cumsum(total_participants)) |>
  ggplot(aes(x = year_clean, y = cumulative_participants)) +
  geom_col() +
  geom_text(aes(label = cumulative_participants), vjust = -0.5, size = 3) +
  labs(
    x = "Year",
    y = "Cumulative participants",
    title = "Cumulative training participants over time"
  )



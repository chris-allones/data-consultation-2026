# setup working directory
setwd(here::here("viserdac-onboarding"))

#libraries
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(tidytext)
library(ggwordcloud)

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
  summarise(
    total_participants = sum(participant, na.rm = TRUE),
    .by = year_clean
  ) |>
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
p_cumm_trained_people <- viserdac_dta |>
  mutate(year_clean = year(date_clean)) |>
  summarise(
    total_participants = sum(participant, na.rm = TRUE),
    .by = year_clean
  ) |>
  arrange(year_clean) |>
  mutate(cumulative_participants = cumsum(total_participants)) |>
  ggplot(aes(x = year_clean, y = cumulative_participants)) +
  geom_col(fill = "#e86060") +
  geom_text(
    aes(label = cumulative_participants, y = cumulative_participants + 250),
    size = 5,
    fontface = "bold"
  ) +
  geom_line(
    aes(y = cumulative_participants + 100),
    color = "steelblue",
    linewidth = 1
  ) +
  geom_point(
    aes(y = cumulative_participants + 100),
    color = "steelblue",
    size = 2
  ) +
  scale_x_continuous(breaks = seq(2017, 2026, by = 1)) +
  scale_y_continuous(breaks = seq(0, 5e3, by = 1e3), limits = c(0, 5e3)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Cumulative training participants over time"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    axis.title = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
  )

# saving the plot
ggsave(
  filename = "plot/cumulative_training_participants.png",
  plot = p_cumm_trained_people,
  width = 10,
  height = 6,
  dpi = 300
)


## wordcloud of training topics

word_groups <- c(
  "statistical" = "statistics",
  "economy" = "economic",
  "economics" = "economic",
  "methods" = "method",
  "projects" = "project",
  "regional" = "region",
  "regions" = "region",
  "agricultural" = "agriculture",
  "agribusiness" = "agriculture"
)

exclude_words <- c(
  "training",
  "workshop",
  "webinar",
  "analysis",
  "spss",
  "series",
  "seminar",
  "lecture",
  "assessment",
  "evaluation",
  "project",
  "projects"
)

training_dec_dta <-
  viserdac_dta |>
  select(training) |>
  unnest_tokens(word, training) |>
  anti_join(stop_words, by = "word") |>
  mutate(word = recode(word, !!!word_groups)) |>
  filter(!word %in% exclude_words) |>
  count(word, sort = TRUE) |>
  filter(n > 1)

p_final <- ggplot(training_dec_dta, aes(label = word, size = n)) +
  geom_text_wordcloud(rm_outside = TRUE, color = "#e86060") +
  scale_size_area(max_size = 20) +
  theme_minimal()

print(p_final)


## save wordcloud plot
ggsave(
  filename = "plot/training_wordcloud.png",
  plot = p_final,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

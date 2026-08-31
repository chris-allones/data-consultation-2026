library(tidyverse)

# ---- Historical data ----
enroll <- tibble(
  year = 2021:2026,
  enrolment = c(2680, 2595, 2403, 2368, 2359, 2099)
)

# Excludes 2026: enrolment dropped ~11% that year, well beyond the
# -0.4% to -7.4% range seen in prior years, so it's treated as a
# possible anomaly rather than baseline trend.
enroll_ex <- enroll |> filter(year != 2026)

# ---- Compute historical CAGR (2021-2025) ----
last_year_ex <- max(enroll_ex$year)
last_value_ex <- enroll_ex$enrolment[enroll_ex$year == last_year_ex]

cagr_ex <- (last_value_ex / enroll_ex$enrolment[enroll_ex$year == min(enroll_ex$year)])^
  (1 / (nrow(enroll_ex) - 1)) - 1

# ---- Scenario 1: Flat (enrolment holds at 2025 level) ----
n_years <- 10

scenario_flat <- tibble(year = (last_year_ex + 1):(last_year_ex + n_years)) |>
  mutate(enrolment = last_value_ex)

# ---- Scenario 2: Constant decline (CAGR applied indefinitely) ----
scenario_moderate <- tibble(year = (last_year_ex + 1):(last_year_ex + n_years)) |>
  mutate(n = row_number(), enrolment = last_value_ex * (1 + cagr_ex)^n) |>
  select(year, enrolment)

# ---- Scenario 3: Damped decline (decline rate tapers toward 0) ----
# phi < 1 shrinks the decline rate applied each successive year, so the
# trajectory levels off rather than compounding indefinitely (cf. damped-
# trend exponential smoothing). phi is a modeling assumption, not
# estimated from data.
phi <- 0.75

damped <- tibble(n = 1:n_years) |>
  mutate(
    year = last_year_ex + n,
    rate = cagr_ex * (phi^(n - 1)),
    cum_factor = cumprod(1 + rate),
    enrolment = last_value_ex * cum_factor
  ) |>
  select(year, enrolment)

# ---- Combine scenarios ----
scenarios_all <- bind_rows(
  scenario_flat |> mutate(scenario = "Flat (0% change)"),
  damped |> mutate(scenario = "Damped decline (levels off)"),
  scenario_moderate |> mutate(scenario = "Constant decline (-3.14%/yr)")
)

scenarios_wide <- scenarios_all |>
  pivot_wider(names_from = scenario, values_from = enrolment)

# ---- Plot ----
ggplot() +
  geom_ribbon(
    data = scenarios_wide,
    aes(x = year, ymin = `Constant decline (-3.14%/yr)`, ymax = `Flat (0% change)`),
    fill = "grey70", alpha = 0.3
  ) +
  geom_line(
    data = enroll_ex, aes(x = year, y = enrolment, color = "Observed"),
    linetype = "dashed"
  ) +
  geom_point(
    data = enroll_ex, aes(x = year, y = enrolment, color = "Observed"), size = 2
  ) +
  geom_line(data = scenarios_all, aes(x = year, y = enrolment, color = scenario)) +
  scale_color_manual(
    values = c(
      "Observed" = "black",
      "Flat (0% change)" = "#619CFF",
      "Damped decline (levels off)" = "#00BA38",
      "Constant decline (-3.14%/yr)" = "#F8766D"
    )
  ) +
  labs(
    x = "Year", y = "Enrolment (JHS & SHS)", color = NULL,
    title = "Enrolment Scenarios: Flat, Damped, and Constant Decline",
    subtitle = "Excludes 2026; damped scenario assumes decline rate tapers toward 0 over time"
  )
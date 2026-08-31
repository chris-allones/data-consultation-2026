library(tidyverse)

# ---- Historical data ----
enroll2 <- tibble(
  year = 2021:2026,
  enrolment = c(4303, 4378, 4303, 4236, 4205, 3784)
)

# Full data (2021-2026) is now used for trend estimation, including the
# steep -10.0% YoY drop in 2026. This makes the projected decline more
# pessimistic than excluding 2026 would.

# ---- Compute historical CAGR (2021-2026) ----
last_year2 <- max(enroll2$year)
last_value2 <- enroll2$enrolment[enroll2$year == last_year2]

cagr2 <- (last_value2 / enroll2$enrolment[enroll2$year == min(enroll2$year)])^
  (1 / (nrow(enroll2) - 1)) - 1

# ---- Scenario 1: Flat (enrolment holds at 2026 level) ----
n_years <- 10

scenario_flat2 <- tibble(year = (last_year2 + 1):(last_year2 + n_years)) |>
  mutate(enrolment = last_value2)

# ---- Scenario 2: Constant decline (CAGR applied indefinitely) ----
scenario_moderate2 <- tibble(year = (last_year2 + 1):(last_year2 + n_years)) |>
  mutate(n = row_number(), enrolment = last_value2 * (1 + cagr2)^n) |>
  select(year, enrolment)

# ---- Scenario 3: Damped decline (decline rate tapers toward 0) ----
# phi < 1 shrinks the decline rate applied each successive year, so the
# trajectory levels off rather than compounding indefinitely. phi is a
# modeling assumption, not estimated from data.
phi <- 0.75

damped2 <- tibble(n = 1:n_years) |>
  mutate(
    year = last_year2 + n,
    rate = cagr2 * (phi^(n - 1)),
    cum_factor = cumprod(1 + rate),
    enrolment = last_value2 * cum_factor
  ) |>
  select(year, enrolment)

# ---- Combine scenarios ----
const_label2 <- paste0("Constant decline (", round(cagr2 * 100, 2), "%/yr)")

scenarios_all2 <- bind_rows(
  scenario_flat2 |> mutate(scenario = "Flat (0% change)"),
  damped2 |> mutate(scenario = "Damped decline (levels off)"),
  scenario_moderate2 |> mutate(scenario = const_label2)
)

scenarios_wide2 <- scenarios_all2 |>
  pivot_wider(names_from = scenario, values_from = enrolment)

# ---- Plot ----
p2 <- ggplot() +
  geom_ribbon(
    data = scenarios_wide2,
    aes(
      x = year,
      ymin = .data[[const_label2]],
      ymax = `Flat (0% change)`
    ),
    fill = "grey70",
    alpha = 0.3
  ) +
  geom_line(
    data = enroll2,
    aes(x = year, y = enrolment, color = "Observed"),
    linetype = "dashed"
  ) +
  geom_point(
    data = enroll2,
    aes(x = year, y = enrolment, color = "Observed"),
    size = 2
  ) +
  geom_line(
    data = scenarios_all2,
    aes(x = year, y = enrolment, color = scenario)
  ) +
  scale_color_manual(
    values = setNames(
      c("black", "#619CFF", "#00BA38", "#F8766D"),
      c("Observed", "Flat (0% change)", "Damped decline (levels off)", const_label2)
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(enroll2$year), max(scenarios_all2$year), by = 2)
  ) +
  labs(
    x = "Year",
    y = "Enrolment",
    color = NULL,
    title = "Enrolment Scenarios: Flat, Damped, and Constant Decline",
    subtitle = "Compound annual growth rate (CAGR) based on 2021-2026)"
  )

p2

ggsave(
  "hnvs-enrollment/enrolment_scenario_plot_2.png",
  plot = p2,
  width = 9,
  height = 6,
  dpi = 300
)

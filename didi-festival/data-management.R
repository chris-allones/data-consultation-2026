## working directory
setwd(here::here("didi-festival"))

## libraries
library(tidyverse)
library(readxl)
library(janitor)
library(scales)


## importing data
fct_dta <- 
  read_excel("data/fest-data.xlsx") |> 
  clean_names()

fct_description <- 
  read_excel("data/fest-data.xlsx", sheet = 2) |> 
  clean_names()

### factor data
lkrt_dta <- 
  fct_dta |> 
  select(fa_1:ewb_10) |> 
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "value"
  ) |>  
  count(item, value) |>
  group_by(item) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  left_join(fct_description, by = c("item" = "item")) |> 
  relocate(c(factor, description), .before = item) |> 
  mutate(
    pct_lab = str_c(round(percent * 100, 0))
  ) |>
  mutate(description = fct_rev(description))


## custom function
# custom theme
custom_theme <-
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      margin = margin(b = 15),
      face = "bold"
    ),
    plot.title.position = "panel",
    plot.subtitle = element_text(
      color = "gray40",
      margin = margin(b = 15),
      size = 12
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

### plot factor
# anchor labels for the two Likert scales used across factors
likert_labels <- list(
  `5` = c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree"
  ),
  `7` = c(
    "Not at all",
    "Slightly",
    "Somewhat",
    "Moderately",
    "Quite a lot",
    "Very much",
    "Extremely"
  )
)

plot_factor <-
  function(factor_name, stwidth = 40) {
    dta_sub <-
      lkrt_dta |>
      mutate(description = str_wrap(description, width = stwidth)) |>
      filter(str_detect(factor, fixed(factor_name)))

    # number of Likert points for this factor, based on the full possible
    # range (e.g. 1-5 or 1-7) rather than just the observed values, so the
    # legend/palette stays consistent even if some options were never chosen
    n_points <- max(dta_sub$value, na.rm = TRUE)
    scale_labs <- likert_labels[[as.character(n_points)]]
    if (is.null(scale_labs)) {
      scale_labs <- as.character(seq_len(n_points))
    }

    # build a palette of the right length from the same warm-to-dark hues
    base_pal <- c("#dc2f02", "#fe7f2d", "#dda15e", "#e43f6e", "#990033")
    pal <- colorRampPalette(base_pal)(n_points)

    dta_sub |>
      mutate(
        value = factor(value, levels = seq_len(n_points), labels = scale_labs)
      ) |>
      ggplot(aes(percent, description, fill = value)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = pct_lab),
        position = position_fill(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = 4
      ) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(values = pal, drop = FALSE) +
      guides(
        fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)
      ) +
      facet_wrap(~factor) +
      labs(
        fill = NULL,
        x = NULL,
        y = NULL
      ) +
      custom_theme
  }


plot_factor("Festival")

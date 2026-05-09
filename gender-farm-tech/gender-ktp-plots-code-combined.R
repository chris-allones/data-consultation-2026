## set working directory
# setwd("D:/Githu-repository/data-consultation-2025/gender-ktp")
setwd("~/Github repository/data-consultation-2025/gender-ktp")


## load packages
library(tidyverse)
library(readxl)

## data management
hh_mgmt_dta <- read_excel("data/gender-ktp-data.xlsx")
hh_fam_child_care_dta <- read_excel("data/gender-ktp-data.xlsx", 2)
hh_family_needs_dta <- read_excel("data/gender-ktp-data.xlsx", 3)
hh_farming_tasks <- read_excel("data/gender-ktp-data.xlsx", 4)

## gender roles in decision-making and performance of household management
p_hh_mgmt <- 
  hh_mgmt_dta %>% 
  group_by(activities) %>% 
  mutate(pct = count / sum(count) * 100) %>%
  filter(pct != 0) %>% 
  mutate(decision = factor(decision, levels = c("Women only", "Women mostly", "Both equally", "Men mostly", "Men only"))) %>% 
  ggplot(aes(y = activities, x = pct, fill = decision)) +
  geom_col() +
  # label values with percent
  geom_text(aes(label = round(pct, 1)), position = position_stack(vjust = 0.5), fontface = "bold", color = "white", size = 4) +
  scale_fill_manual(values = c("#e27396", "#ff99ac", "#8d99ae", "#00b4d8", "#0077b6")) +
  labs(x = "Percentage of decisions", y = "Activities", fill = "Decision maker/ \n Who performs") +
  theme_minimal() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, margin = margin(r = 20)),
        legend.position = "top",
        legend.box = "horizontal")

### saving plot
ggsave("figures/household_management.jpeg", 
       plot = p_hh_mgmt, 
       width = 11, height = 6.5, dpi = 300, units = "in")


## gender roles in decision-making and performance of family and child care
p_hh_fam_child_care <- 
  hh_fam_child_care_dta %>% 
  group_by(activities) %>% 
  mutate(pct = count / sum(count) * 100) %>% 
  mutate(decision = factor(decision, levels = c("Women only", "Women mostly", "Both equally", "Men mostly", "Men only"))) %>% 
  ggplot(aes(y = activities, x = pct, fill = decision)) +
  geom_col() +
  # label values with percent
  geom_text(aes(label = round(pct, 1)), position = position_stack(vjust = 0.5), fontface = "bold", color = "white", size = 4) +
  scale_fill_manual(values = c("#e27396", "#ff99ac", "#8d99ae", "#00b4d8", "#0077b6")) +
  labs(x = "Percentage of decisions", y = "Activities", fill = "Decision maker/ \n Who performs") +
  theme_minimal() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, margin = margin(r = 20)),
        legend.position = "top",
        legend.box = "horizontal")

### saving plot
ggsave("figures/family_child_care.jpeg", 
       plot = p_hh_fam_child_care, 
       width = 11, height = 3.5, dpi = 300, units = "in")


## Gender roles in decision-making and performance of personal and family needs
p_hh_family_needs <- 
  hh_family_needs_dta |> 
  group_by(activities) |> 
  mutate(pct = count / sum(count) * 100) |> 
  ungroup() |>
  mutate(decision = factor(decision, levels = c("Women only", "Women mostly", "Both equally", "Men mostly", "Men only"))) |> 
  filter(pct != 0) |> 
  ggplot(aes(y = activities, x = pct, fill = decision)) +
  geom_col() +
  geom_text(aes(label = round(pct, 1)), position = position_stack(vjust = 0.5), size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#e27296", "#ff9aac", "#8d99af", "#00b4d7", "#0077b9")) +
  labs(
    x = "Percentage of decisions",
    y = "Activities",
    fill = "Decision maker/ \n Who performs"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(rep(10, 4)),
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(r=15)),
    axis.title.y = element_text(size = 14)
  )

## saving plot
ggsave(
  filename = "figures/household-family-needs.jpeg",
  plot = p_hh_family_needs,
  width = 10,
  height = 3,
  dpi = 300
)


## Gender Roles in Decision-Making and Performance of Vegetable Farming Tasks 
activities_levels <- 
  hh_farming_tasks |> 
  select(activities) |> 
  distinct(activities) |> 
  pull(activities)


p_farming_task <- 
  hh_farming_tasks |> 
  mutate(decision_maker_bi = case_when(
    str_detect(decision_maker, "Men") ~ "Men mostly",
    str_detect(decision_maker, "Women") ~ "Women mostly",
    str_detect(decision_maker, "Both") ~ "Both equally"
  )) |> 
  group_by(activities, decision_maker_bi) |> 
  summarise(who_decide_bi = sum(who_decide),
            who_perform_bi = sum(who_perform)) |>
  ungroup() |> 
  group_by(activities) |> 
  mutate(
    pct_decide = who_decide_bi / sum(who_decide_bi) * 100,
    pct_perform = who_perform_bi / sum(who_perform_bi) * 100
  ) |> 
  ungroup() |> 
  pivot_longer(cols= c(pct_decide, pct_perform), 
               names_to = "type", 
               values_to = "pct") |> 
  mutate(activities = factor(activities, levels = activities_levels)) |> 
  mutate(activities = fct_rev(activities)) |>
  mutate(type = recode(type, 
                       pct_decide = "Decision-making", 
                       pct_perform = "Performing tasks")) |>
  ggplot((aes(y = activities, x = pct, fill = decision_maker_bi))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), 
            position = position_dodge(width = 1), 
            hjust = -0.1,
            size = 3) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("gray70", "#0077b6", "#e27396")) +
  facet_wrap(~type) +
  theme_minimal() +
  labs(
    x = "Percentage of decisions",
    y = "Activities",
    fill = element_blank()
  ) +
  theme(
    plot.margin = margin(rep(10, 4)),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),
    legend.text = element_text(size = 12)
  )

## saving plot
ggsave(
  plot = p_farming_task,
  filename = "figures/household-farming-tasks.jpeg",
  width = 10,
  height = 10,
  dpi = 400
)














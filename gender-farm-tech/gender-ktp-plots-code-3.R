# setting working directory
setwd("C:/Users/chris/Documents/Github-repository/data-consultation-2025/gender-ktp")

# libraries
library(tidyverse)
library(readxl)
library(janitor)

# data management
prod_gender_dta <- 
  read_excel("data/gender-ktp-farming-hrs-by-gender.xlsx") |> 
  clean_names() |> 
  pivot_longer(cols = female_hrs_ha:male_hrs_ha,
               values_to = "hrs",
               names_to = "gender") |> 
  mutate(gender = str_remove_all(gender, "_hrs_ha"))


p_prod_activities_hrs <- 
  prod_gender_dta |> 
  filter(crop != "Eggplant") |> 
  ggplot(aes(hrs, activity, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = hrs), position = position_dodge2(width = 1), size = 3, hjust = -0.2)+
  scale_x_continuous(limits = c(0, 100)) +
  facet_wrap(~crop, scale = "free") +
  labs(fill = element_blank(),
       y = element_blank()) +
  theme_minimal() +
  theme(
    plot.margin = margin(rep(15, 4)),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, margin = margin(b=20)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12, margin = margin(b=15))
  )


## saving plot
ggsave(
  filename = "figures/prod_activities_hrs_by_gender.jpeg",
  plot = p_prod_activities_hrs, 
  width = 10,
  height = 10,
  dpi = 300
)


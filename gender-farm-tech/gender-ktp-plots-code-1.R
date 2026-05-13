## set working directory
heresetwd(here::here("gender-farm-tech"))

## data management
hh_mgmt_dta <- read_excel("data/gender-ktp-data.xlsx")
hh_fam_child_care_dta <- read_excel("data/gender-ktp-data.xlsx", 2)

## gender roles in decision-making and performance of household management
p_hh_mgmt <-
       hh_mgmt_dta %>%
       group_by(activities) %>%
       mutate(pct = count / sum(count) * 100) %>%
       filter(pct != 0) %>%
       mutate(
              decision = factor(
                     decision,
                     levels = c(
                            "Women only",
                            "Women mostly",
                            "Both equally",
                            "Men mostly",
                            "Men only"
                     )
              )
       ) %>%
       ggplot(aes(y = activities, x = pct, fill = decision)) +
       geom_col() +
       # label values with percent
       geom_text(
              aes(label = round(pct, 1)),
              position = position_stack(vjust = 0.5),
              fontface = "bold",
              color = "white",
              size = 4
       ) +
       scale_fill_manual(
              values = c("#e27396", "#ff99ac", "#8d99ae", "#00b4d8", "#0077b6")
       ) +
       labs(
              x = "Percentage of decisions",
              y = "Activities",
              fill = "Decision maker/ \n Who performs"
       ) +
       theme_minimal() +
       theme(
              plot.margin = margin(1, 1, 1, 1, "cm"),
              panel.grid = element_blank(),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14, margin = margin(r = 20)),
              legend.position = "top",
              legend.box = "horizontal"
       )

### saving plot
ggsave(
       "plot/household_management.jpeg",
       plot = p_hh_mgmt,
       width = 11,
       height = 6.5,
       dpi = 300,
       units = "in"
)


## gender roles in decision-making and performance of family and child care
p_hh_fam_child_care <-
       hh_fam_child_care_dta %>%
       group_by(activities) %>%
       mutate(pct = count / sum(count) * 100) %>%
       mutate(
              decision = factor(
                     decision,
                     levels = c(
                            "Women only",
                            "Women mostly",
                            "Both equally",
                            "Men mostly",
                            "Men only"
                     )
              )
       ) %>%
       ggplot(aes(y = activities, x = pct, fill = decision)) +
       geom_col() +
       # label values with percent
       geom_text(
              aes(label = round(pct, 1)),
              position = position_stack(vjust = 0.5),
              fontface = "bold",
              color = "white",
              size = 4
       ) +
       scale_fill_manual(
              values = c("#e27396", "#ff99ac", "#8d99ae", "#00b4d8", "#0077b6")
       ) +
       labs(
              x = "Percentage of decisions",
              y = "Activities",
              fill = "Decision maker/ \n Who performs"
       ) +
       theme_minimal() +
       theme(
              plot.margin = margin(1, 1, 1, 1, "cm"),
              panel.grid = element_blank(),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14, margin = margin(r = 20)),
              legend.position = "top",
              legend.box = "horizontal"
       )

### saving plot
ggsave(
       "plot/family_child_care.jpeg",
       plot = p_hh_fam_child_care,
       width = 11,
       height = 3.5,
       dpi = 300,
       units = "in"
)

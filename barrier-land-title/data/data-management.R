## setup
### working directory
setwd(here::here("barrier-land-title"))


### libraries
# libraries
library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(EFAtools)
library(gtsummary)
library(kableExtra)
library(correlation)
library(reshape2)
library(tidyr)
library(tidytext)
library(ggwordcloud)
library(widyr)
library(ggraph)
library(tidygraph)


## import data
df_text <-
  read_excel("data/land-title.xlsx", 2) |>
  clean_names() |>
  select(
    starts_with("what_is_the_main_reason"),
    starts_with("suggestions_or")
  ) |>
  rename(
    'reason' = what_is_the_main_reason_why_people_in_palo_remain_untitled,
    'suggestion' = suggestions_or_recommendations_to_make_land_titling_faster_and_easier_in_palo_leyte
  )


## custom theme
custom_theme <-
  theme_gray() +
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
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 20)
    ),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )


## funtions general
### separate multiple words in one colum with clear delimiter
str_separate <-
  function(df = df, var_name) {
    df |>
      separate_longer_delim({{ var_name }}, delim = ",") |>
      mutate(
        {{ var_name }} := str_to_lower({{ var_name }}),
        {{ var_name }} := str_remove_all({{ var_name }}, "\\([^)]*\\)"),
        {{ var_name }} := str_remove_all({{ var_name }}, "[0-9%]"),
        {{ var_name }} := str_remove_all({{ var_name }}, "[\r\n/]"),
        {{ var_name }} := str_remove_all({{ var_name }}, "[^a-z\\s]"),
        {{ var_name }} := str_squish({{ var_name }}),
        {{ var_name }} := str_trim({{ var_name }})
      )
  }

### local stopwords
filter_word <- c(
  "ma’am",
  "jen",
  "jen’s",
  "interviewer",
  "kii",
  "transcription",
  "it’s",
  "don’t",
  "php",
  "there’s",
  "who’s",
  "interviewer’s",
  "interviwer’t",
  "we’ve",
  "we’re",
  "wasn’t",
  "kg",
  "kgs",
  "sofia",
  "theo",
  "they’re",
  "i’m",
  "isn’t",
  "ms",
  "that’s",
  "maam",
  "jenelyn",
  "perez",
  "can’t",
  "one’s",
  "ang",
  "sa",
  "na",
  "kay",
  "man",
  "of",
  "ug",
  "dili",
  "naay",
  "para",
  "pa",
  "kung",
  "lang",
  "by",
  "and",
  "to",
  "the",
  "mi",
  "nga",
  "in",
  "siya",
  "naa",
  "mao",
  "may",
  "mas",
  "kaayo",
  "sya",
  "wala",
  "saging",
  "kape",
  "akong",
  "ka",
  "jud",
  "diri",
  "ra",
  "maka",
  "ta",
  "amo",
  "pwde",
  "gud",
  "namo",
  "nay",
  "mag",
  "pwede",
  "amoang",
  "pang",
  "nag",
  "ni",
  "pero",
  "ray",
  "agn",
  "ani",
  "daw",
  "diha",
  "run",
  "dayun",
  "dyud",
  "gyud",
  "gyod",
  "wa",
  "unya",
  "rag",
  "pud",
  "pag",
  "naman",
  "mu",
  "maski",
  "ma",
  "kay",
  "kani",
  "imoha",
  "imoha",
  "ila",
  "gikan",
  "ugod",
  "abat",
  "iyaha",
  "kana",
  "mang",
  "nalang",
  "basta",
  "bisan",
  "dira",
  "dun",
  "ky",
  "laing",
  "for",
  "ana",
  "if",
  "ng",
  "no",
  "on",
  "othr",
  "there",
  "ting",
  "uban",
  "walay",
  "silay",
  "pod",
  "is",
  "mga",
  "mo",
  "ok",
  "too",
  "with",
  "po",
  "uhh",
  "ah",
  "peter",
  "mitch’t",
  "mitch’s",
  "you're",
  "let’s",
  "neil",
  "neil’s",
  "didn’t",
  "sir",
  "what’s",
  "I’ll",
  "nako",
  "nila",
  "he’s",
  "didto",
  "diay",
  "isa",
  "mitch",
  "tuyok",
  "imong",
  "consent",
  "recorded",
  "nakita",
  "bukid",
  "ko",
  "b",
  "b’s",
  "fe",
  "recording",
  "short",
  "clip",
  "won’t",
  "bustamante",
  "anj",
  "kasi",
  "ano",
  "yeah",
  "meron",
  "naming",
  "yung",
  "din",
  "uh",
  "uhm",
  "inaudible",
  "parang",
  "speaker",
  "audience",
  "abella",
  "counsilor",
  "haidee",
  "varieties",
  "i’ll",
  "skipped",
  "audio",
  "twinkle",
  "mf1",
  "mf2",
  "woman",
  "male",
  "wf2",
  "ay",
  "ayay",
  "nya",
  "or",
  "sge",
  "sgeg",
  "sige",
  "nanga",
  "og",
  "at",
  "sigeg",
  "ito",
  "because",
  "ibang",
  "ito",
  "iba",
  "tas",
  "nang",
  "ming",
  "it",
  "bantog",
  "dahil",
  "o",
  "kaya",
  "so",
  "gi",
  "ga",
  "sig",
  "sila",
  "that",
  "are",
  "aking",
  "mmga",
  "6",
  "25",
  "5",
  "3",
  "46",
  "11"
)

## percent label
pct_label <- function(df, count_var) {
  df |>
    mutate(
      pct = {{ count_var }} / sum({{ count_var }}, na.rm = TRUE),
      pct_lab = str_c(
        " ",
        round(pct * 100, 0),
        "% ",
        "(n = ",
        {{ count_var }},
        ")"
      )
    )
}


## main reason
set.seed(20260513)


p_word_reason <- 
  df_text |>
  select(reason) |>
  mutate(q_id = row_number()) |>
  unnest_tokens(word, reason) |>
  anti_join(stop_words) |>
  filter(!word %in% filter_word) |>
  pairwise_cor(word, q_id, sort = TRUE) |>
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(
    color = "steelblue",
    aes(size = centrality_pagerank()),
    show.legend = F
  ) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_void() +
  labs(
    title = str_wrap(
      "What is the main reason why people in Palo remain untitled?",
      60
    )
  ) +
  theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        margin = margin(b = 10),
        face = "bold"
      ),
      plot.subtitle = element_text(
        color = "gray60",
        margin = margin(b = 15),
        size = 16,
        hjust = 0.5
      ),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )

## saving plot
ggsave(
  plot = p_word_reason,
  filename = "plot/reason.jpeg",
  width = 12,
  heigh = 8,
  unit = "in",
  dpi = 400,
)




## suggestions
set.seed(20260513)

p_word_suggestion <- 
  df_text |>
  select(suggestion) |>
  mutate(q_id = row_number()) |>
  unnest_tokens(word, suggestion) |>
  anti_join(stop_words) |>
  filter(!word %in% filter_word) |>
  pairwise_cor(word, q_id, sort = TRUE) |>
  filter(correlation > .3) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(
    color = "steelblue",
    aes(size = centrality_pagerank()),
    show.legend = F
  ) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_void() +
  labs(
    title = str_wrap(
      "Suggestions or recommendations to make land titling faster and easier in Palo, Leyte?",
      60
    )
  ) +
  theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        margin = margin(b = 10),
        face = "bold"
      ),
      plot.subtitle = element_text(
        color = "gray60",
        margin = margin(b = 15),
        size = 16,
        hjust = 0.5
      ),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )

## saving plot
ggsave(
  plot = p_word_suggestion,
  filename = "plot/suggestion.jpeg",
  width = 12,
  heigh = 8,
  unit = "in",
  dpi = 400,
)

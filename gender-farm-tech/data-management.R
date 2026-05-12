# data management
## names for selected columns
df <- read_excel("data/ama-raw-data-with var.xlsx") |>
  clean_names() |>
  mutate(
    ethnicity = if_else(str_detect(ethnicity, "waray"), "waray", ethnicity)
  ) |>
  mutate(
    nonfarm_income = as.numeric(nonfarm_income),
    nonfarm_income = if_else(is.na(nonfarm_income), 0, nonfarm_income),
    nonfarm_income = if_else(nonfarm_income == 0, NA, nonfarm_income),
    offfarm_income_yr = if_else(offfarm_income_yr == 0, NA, offfarm_income_yr),
    land_type = if_else(str_detect(land_type, "Lowland"), "Lowland", land_type)
  ) |>
  select(
    -religion
  )


## data for regression on socioecon
reg_df <-
  df |>
  select(
    postlearn_action,
    sex,
    age,
    civil_status,
    educ_attainment,
    hh_head,
    hh_size,
    nonfarm_income,
    onfarm_income_yr,
    offfarm_income_yr,
    water_sufficient,
    hh_org_member,
    org_status,
    attend_training5yr,
    aware_newtech,
    percep2:percep11,
    know2:know14,
    practice1:practice16,
    attitude1:attitude8
  ) |>
  mutate(across(
    percep2:attitude8,
    .fns = ~ case_when(
      .x == "Strongly agree" ~ 5,
      .x == "Agree" ~ 4,
      .x == "No opinion" ~ 3,
      .x == "Disagree" ~ 2,
      .x == "Strongly disagree" ~ 1,
    )
  )) |>
  mutate(
    adopt = if_else(str_detect(postlearn_action, "Adopt|Attend|Seek"), 1, 0),
    adopt = if_else(is.na(adopt), 0, adopt),
    summ_percep = rowSums(across(starts_with("percep")), na.rm = TRUE),
    summ_know = rowSums(across(starts_with("know")), na.rm = TRUE),
    summ_practice = rowSums(across(starts_with("practice")), na.rm = TRUE),
    summ_attitude = rowSums(across(starts_with("percep")), na.rm = TRUE),
    total_income = rowSums(across(contains("income")), na.rm = TRUE),
    educ_attainment = if_else(
      educ_attainment == "Elementary",
      "Elementary",
      "High school and higher"
    ),
    civil_status = if_else(civil_status == "single", "single", "married"),
    summ_practice = summ_practice * -1
  ) |>
  select(
    -postlearn_action,
    -c(percep2:attitude8),
    -offfarm_income_yr,
    -org_status,
    -water_sufficient,
    -nonfarm_income,
    -onfarm_income_yr,
    -summ_attitude,
    -hh_head,
    -total_income
  )


## data for barriers model
barrier_df <-
  df |>
  select(
    postlearn_action,
    sex,
    age,
    civil_status,
    educ_attainment,
    hh_head,
    hh_size,
    nonfarm_income,
    onfarm_income_yr,
    offfarm_income_yr,
    water_sufficient,
    hh_org_member,
    org_status,
    attend_training5yr,
    aware_newtech,
    seminar_notavail:no_need
  ) |>
  mutate(
    adopt = if_else(str_detect(postlearn_action, "Adopt|Attend|Seek"), 1, 0),
    adopt = if_else(is.na(adopt), 0, adopt),
    educ_attainment = if_else(
      educ_attainment == "Elementary",
      "Elementary",
      "High school and higher"
    ),
    civil_status = if_else(civil_status == "single", "single", "married"),
    across(seminar_notavail:no_need, ~ if_else(is.na(.x), 0, 1)),
    across(seminar_notavail:no_need, ~ if_else(.x == 0, "No", "Yes"))
  ) |>
  relocate(adopt, .before = sex) |>
  select(
    -postlearn_action,
    -offfarm_income_yr,
    -water_sufficient,
    -nonfarm_income,
    -onfarm_income_yr,
    -org_status,
    -hh_head,
    -seminar_notavail,
    -house_far,
    -no_need
  )

#=======================================================
## likert plot

## likert items data
### statements
lkrt_statements <-
  read_excel("data/ama-raw-data-with var.xlsx", 3) |>
  clean_names() |>
  mutate(item = str_to_lower(items)) |>
  select(factor, statement, item)


lkrt_dta <-
  df |>
  select(sex, percep1:attitude8) |>
  select(-percep1, -know1) |>
  pivot_longer(
    cols = percep2:attitude8,
    names_to = "item",
    values_to = "response"
  ) |>
  na.omit() |>
  count(sex, item, response) |>
  group_by(sex, item) |>
  mutate(percent = n / sum(n)) |>
  left_join(lkrt_statements, by = "item") |>
  relocate(c(factor, statement), .before = item) |>
  ungroup() |>
  mutate(
    response = factor(
      response,
      levels = c(
        "Strongly Disagree",
        "Disagree",
        "No opinion",
        "Agree",
        "Strongly Agree"
      )
    ),
    pct_lab = str_c(round(percent * 100, 0))
  ) |>
  mutate(response = fct_rev(response))


### plot factor
plot_factor <-
  function(factor_name, stwidth = 50, gender = "Female") {
    lkrt_dta |>
      filter(sex == gender) |>
      mutate(statement = str_wrap(statement, width = stwidth)) |>
      filter(str_detect(factor, factor_name)) |>
      ggplot(aes(percent, statement, fill = response)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = pct_lab),
        position = position_fill(vjust = 0.5),
        color = "white",
        fontface = "bold"
      ) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(
        values = c("#2c6e49", "#4c956c", "#d6cfcb", "#ffc9b9", "#d68c45")
      ) +
      guides(
        fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)
      ) +
      facet_wrap(~factor) +
      labs(
        fill = NULL,
        y = NULL,
        x = str_c(gender, " reponse")
      ) +
      custom_theme
  }


# ======================================================
## data from previous analysis
hh_mgmt_dta <- read_excel("data/gender-ktp-data.xlsx")
hh_fam_child_care_dta <- read_excel("data/gender-ktp-data.xlsx", 2)
hh_family_needs_dta <- read_excel("data/gender-ktp-data.xlsx", 3)
hh_farming_tasks <- read_excel("data/gender-ktp-data.xlsx", 4)


#=======================================================

## setup
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


## functions specific
### bar plot by gender
plot_pct_by_sex <- function(df, category_var, ncol = 1, x_max = 1) {
  df |>
    select(sex, {{ category_var }}) |>
    count(sex, {{ category_var }}) |>
    na.omit() |>
    group_by(sex) |>
    pct_label(n) |>
    ungroup() |>
    mutate(
      {{ category_var }} := reorder_within({{ category_var }}, pct, sex)
    ) |>
    ggplot(aes(pct, {{ category_var }}, fill = sex)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = pct_lab), hjust = 0) +
    scale_y_reordered() +
    scale_x_continuous(
      limits = c(0, x_max),
      labels = scales::percent_format()
    ) +
    facet_wrap(~sex, scales = "free_y", ncol = ncol) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    custom_theme
}


## bar plot simplified

plot_bar_by_sex <- function(df, category_var, ncol = 1, x_max = 1) {
  df |>
    mutate(
      {{ category_var }} := reorder_within({{ category_var }}, pct, sex)
    ) |>
    ggplot(aes(pct, {{ category_var }}, fill = sex)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = pct_lab), hjust = 0) +
    scale_y_reordered() +
    scale_x_continuous(
      limits = c(0, x_max),
      labels = scales::percent_format()
    ) +
    facet_wrap(~sex, scales = "free_y", ncol = ncol) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    custom_theme
}


df |> glimpse()


## local stopwords

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

## string data
txt_res_df <- df |>
  select(
    sex,
    reason_adopt,
    dec_buyfood_166,
    dec_buyfood_236,
    perf_acqappliance,
    dec_acqfurn,
    perf_acqfurn,
    dec_acqfarminputs,
    perf_avail_loans,
    perf_avail_loans,
    dec_spendincome,
    perf_spendincome,
    dec_leisureact,
    dec_cleanhouse,
    dec_carechild,
    dec_mgmt_hhfinance,
    dec_croptoplant,
    perf_croptoplant,
    dec_buyfarmsup,
    dec_market_outlet,
    dec_recordfarm,
    perf_mgmt_finances,
    dec_mgmt_finances,
    dec_attendtraining
  )

txt_res_df |> View()

txt_res_df |>
  select(sex, dec_acqfarminputs) |>
  mutate(q_id = row_number()) |>
  unnest_tokens(word, dec_acqfarminputs) |>
  anti_join(stop_words) |>
  filter(sex == "Female") |>
  filter(!word %in% filter_word) |>
  pairwise_cor(word, q_id, sort = TRUE) |>
  filter(correlation > .1) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(
    color = "steelblue",
    aes(size = centrality_pagerank()),
    show.legend = F
  ) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_void()

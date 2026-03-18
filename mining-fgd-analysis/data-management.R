# data management
## import fgd data
fgd_data <- 
  read_excel("data/fgd-isrds-data.xlsx") |> 
  clean_names() |> 
  mutate(across(everything(), ~ str_trim(.x, side = "both")))



### local stopwords
filter_word <- c("ma’am", "jen", "jen’s", "interviewer", "kii", "transcription",
                 "it’s", "don’t", "php", "there’s", "who’s",
                 "interviewer’s", "interviwer’t", "we’ve",
                 "we’re", "wasn’t", "kg", "kgs", "sofia",
                 "theo", "they’re", "i’m", "isn’t", "ms", "that’s",
                 "maam", "jenelyn", "perez", "can’t", "one’s", "ang",
                 "sa", "na", "kay", "man",
                 "of", "ug", "dili", "naay", "para", "pa",
                 "kung", "lang", "by", "and", "to", "the",
                 "mi", "nga", "in", "siya", "naa", "mao",
                 "may", "mas", "kaayo", "sya", "wala", 
                 "saging", "kape", "akong", "ka", "jud",
                 "diri", "ra", "maka", "ta", "amo", "pwde",
                 "gud", "namo", "nay", "mag", "pwede",
                 "amoang", "pang", "nag", "ni", "pero", "ray",
                 "agn", "ani", "daw", "diha", "run", "dayun", "dyud",
                 "gyud", "gyod", "wa", "unya", "rag", "pud", "pag",
                 "naman", "mu", "maski", "ma", "kay", "kani", "imoha",
                 "imoha", "ila", "gikan", "ugod", "abat", "iyaha", "kana",
                 "mang", "nalang", "basta", "bisan", "dira", "dun", 
                 "ky", "laing", "for", "ana", "if", "ng", "no", "on", "othr",
                 "there", "ting", "uban", "walay", "silay", "pod", "is", 
                 "mga", "mo", "ok", "too", "with", "po", "uhh", "ah",
                 "peter", "mitch’t", "mitch’s", "you're", "let’s",
                 "neil", "neil’s", "didn’t", "sir", "what’s", "I’ll",
                 "nako", "nila", "he’s", "didto", "diay", "isa", "mitch",
                 "tuyok", "imong", "consent", "recorded", "nakita", "bukid",
                 "ko", "b", "b’s", "fe", "recording", "short", "clip",
                 "won’t", "bustamante", "anj", "kasi", "ano", "yeah", "meron",
                 "naming", "yung", "din", "uh", "uhm", "inaudible", "parang", "speaker",
                 "audience", "abella", "counsilor", "haidee", "varieties",
                 "i’ll", "skipped", "audio", "twinkle", "mf1", "mf2", "woman",
                 "male", "wf2", "ay", "ayay", "nya", "or", "sge", "sgeg", "sige", "nanga", "og", "at", "sigeg", "ito",
                 "because", "ibang", "ito", "iba", "tas", "nang", "ming", "it", "bantog", "dahil", "o", "kaya",
                 "so", "gi", "ga", "sig", "sila", "that", "are", "aking", "mmga", "6", "25", "5", "3",
                "46", "11")

filter_word_2 <- c("ma’am", "jen", "jen’s", "interviewer", "kii", "transcription",
                   "it’s", "don’t", "php", "there’s", "who’s",
                   "interviewer’s", "interviwer’t", "we’ve",
                   "we’re", "wasn’t", "kg", "kgs", "sofia",
                   "theo", "they’re", "i’m", "isn’t", "ms", "that’s",
                   "maam", "jenelyn", "perez", "can’t", "one’s", "ang",
                   "sa", "na", "kay", "man",
                   "of", "ug", "dili", "naay", "para", "pa",
                   "kung", "lang", "by", "and", "to", "the",
                   "mi", "nga", "in", "siya", "naa", "mao",
                   "may", "mas", "kaayo", "sya", "wala", 
                   "saging", "kape", "akong", "ka", "jud",
                   "diri", "ra", "maka", "ta", "amo", "pwde",
                   "dapat", "gud", "namo", "nay", "mag", "pwede",
                   "amoang", "pang", "nag", "ni", "pero", "ray",
                   "agn", "ani", "daw", "diha", "run", "dayun", "dyud",
                   "gyud", "gyod", "wa", "unya", "rag", "pud", "pag",
                   "naman", "mu", "maski", "ma", "kay", "kani", "imoha",
                   "imoha", "ila", "gikan", "ugod", "abat", "iyaha", "kana",
                   "mang", "nalang", "basta", "bisan", "dira", "dun", 
                   "ky", "laing", "for", "ana", "if", "ng", "no", "on", "othr",
                   "there", "ting", "uban", "walay", "silay", "pod", "is", 
                   "mga", "mo", "ok", "too", "with", "po", "uhh", "ah",
                   "peter", "mitch’t", "mitch’s", "you're", "let’s",
                   "neil", "neil’s", "didn’t", "sir", "what’s", "I’ll",
                   "nako", "nila", "he’s", "didto", "diay", "isa", "mitch",
                   "tuyok", "imong", "consent", "recorded", "nakita", "bukid",
                   "ko", "b", "b’s", "fe", "recording", "short", "clip",
                   "won’t", "bustamante", "anj", "kasi", "ano", "yeah", "meron",
                   "naming", "yung", "din", "uh", "uhm", "inaudible", "parang", "speaker",
                   "audience", "abella", "counsilor", "haidee", "varieties",
                   "i’ll", "skipped", "audio", "twinkle", "woman", "farmer",
                   "claire", "male", "mf1", "mf2", "woman", "male", "wf2")



## tokenization
fgd_theme <- 
  fgd_data |> 
  select(theme, description)

fgd_token <- 
  fgd_data |> 
  group_by(theme, question) |> 
  unnest_tokens(word, answer) |> 
  anti_join(stop_words) |> 
  left_join(fgd_theme, by = c("theme"), relationship = "many-to-many") |> 
  relocate(description, .after = theme) |> 
  mutate(word = if_else(word == "babae", "women", word)) |> 
  count(word) |> 
  filter(!word %in% filter_word) |> 
  na.omit()
  

## for wordcloud by questions

q1_fgd <- 
  fgd_token |> 
  filter(str_detect(question, "What roles"))

q2_fgd <- 
  fgd_token |> 
  filter(str_detect(question, "What are the biggest challenge")) |> 
  arrange(desc(n))

q3_fgd <- 
  fgd_token |> 
  filter(str_detect(question, "Who in the community"))

q4_fgd <- 
  fgd_token |> 
  filter(str_detect(question, "Which household"))

q5_fgd <- 
  fgd_token |> 
  filter(str_detect(question, "Do women want to work"))


## data for bigram
q1_word_corr_fgd <- 
  fgd_data |> 
  filter(str_detect(question, "What roles")) |> 
  mutate(q_id = row_number()) |> 
  unnest_tokens(word, answer) |> 
  filter(!word %in% filter_word) |> 
  anti_join(stop_words) |> 
  pairwise_cor(word, q_id, sort = TRUE)

q2_word_corr_fgd <- 
  fgd_data |> 
  filter(str_detect(question, "What are the biggest challenge")) |> 
  mutate(q_id = row_number()) |> 
  unnest_tokens(word, answer) |> 
  filter(!word %in% filter_word) |> 
  anti_join(stop_words) |> 
  pairwise_cor(word, q_id, sort = TRUE)


q3_word_corr_fgd <- 
  fgd_data |> 
  filter(str_detect(question, "Who in the community")) |> 
  mutate(q_id = row_number()) |> 
  unnest_tokens(word, answer) |> 
  filter(!word %in% filter_word) |> 
  anti_join(stop_words) |> 
  pairwise_cor(word, q_id, sort = TRUE)


q4_word_corr_fgd <- 
  fgd_data |> 
  filter(str_detect(question, "Which household")) |> 
  mutate(q_id = row_number()) |> 
  unnest_tokens(word, answer) |> 
  filter(!word %in% filter_word) |> 
  anti_join(stop_words) |> 
  pairwise_cor(word, q_id, sort = TRUE)


q5_word_corr_fgd <- 
  fgd_data |> 
  filter(str_detect(question, "Do women want to work")) |> 
  mutate(q_id = row_number()) |> 
  unnest_tokens(word, answer) |> 
  filter(!word %in% filter_word) |> 
  anti_join(stop_words) |> 
  pairwise_cor(word, q_id, sort = TRUE)


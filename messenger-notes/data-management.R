# Install required packages if not yet installed
# install.packages(c("tidyverse", "tidytext", "topicmodels"))

setwd(here::here("messenger-notes"))

library(tidyverse)
library(tidytext)
library(topicmodels)

# 1. Load your dataset (semicolon-separated CSV)
notes <- read_csv("data/notes-dataset.csv")

# 2. Tokenize text into words
tidy_notes <- notes %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")   # remove common English stopwords

# 3. Create a document-term matrix
dtm <- tidy_notes %>%
  count(note_id, word) %>%
  cast_dtm(note_id, word, n)

# 4. Fit LDA model (choose number of topics, e.g., 4)
lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))

# 5. Extract top terms per topic
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta)

print(top_terms)

# Plot top terms per topic 
top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip() + 
  scale_x_reordered() + 
  labs(title = "Top Terms per Topic",
        x = "Term",
        y = "Beta (probability within topic)")


# 6. Assign each note to its most likely topic
doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

# Merge back with original notes
clustered_notes <- notes %>%
  mutate(note_id = as.character(note_id)) |> 
  left_join(doc_topics, by = c("note_id" = "document"))

print(clustered_notes)


# 2. Tokenize text 
tidy_notes <- notes %>% 
  unnest_tokens(word, text)

# 3. Use Bing sentiment lexicon 
sentiments <- tidy_notes %>% 
  inner_join(get_sentiments("bing"), by = "word")

# 4. Count sentiment per note 
sentiment_counts <- sentiments %>% 
  count(note_id, sentiment) 

# 5. Aggregate overall sentiment 
overall_sentiment <- sentiments %>% 
  count(sentiment) |> 
  mutate(pct = n / sum(n))

# 6. Plot overall sentiment distribution 
ggplot(overall_sentiment, aes(x = sentiment, y = pct, fill = sentiment)) + 
  geom_col(show.legend = FALSE, width = 0.5) + 
  geom_text(aes(label = round(pct * 100, 2), color = sentiment), size = 8, show.legend = F, vjust = -0.8) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Overall Sentiment Distribution", 
       x = "Sentiment", y = "Word Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"))
       
# 7. Plot top positive/negative words 
top_words <- sentiments %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10) 

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~ sentiment, scales = "free") + coord_flip() + labs(title = "Top Words by Sentiment", x = "Word", y = "Frequency")

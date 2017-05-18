require(tidyverse)
require(tidytext)

sentiments_bing <- tidytext::get_sentiments("bing")
sentiments_nrc <- tidytext::get_sentiments("nrc")

responses_words <- read_rds("data/words-tidy.rds")

get_sentiment_scores <- function(words){
  # Scores each word (-1, 0, +1),for use in sentiment scoring
  data.frame(word = words, stringsAsFactors = F) %>%
    left_join(sentiments_bing, by = "word") %>%
    mutate(sentiment = ifelse(sentiment == "negative", -1, 
                              ifelse(sentiment == "positive", 1, 0))) %>%
    replace_na(list(sentiment = 0))
}

average_sentiment <- function(words){
  # Returns the average sentiment accross the df (-100 to +100)
  df <- get_sentiment_scores(words)
  mean(df$sentiment) * 100
}

sentiment_per_doc <- responses_words %>%
  group_by(uuid, col1, col2, col3) %>%
  summarise(sentiment_score = average_sentiment(word)) %>%
  ungroup

  
# Not totally correct as the df excludes stopwords
total_words_per_doc <- count(responses_words,uuid)
  
nrc_sentiment_per_doc <- responses_words %>%
  left_join(sentiments_nrc, by = "word") %>%
  replace_na(list(sentiment = "none")) %>%
  mutate(count = 1) %>%
  group_by(uuid, col1, col2, col3, sentiment) %>%
  summarise(count = sum(count)) %>%
  ungroup %>%
  left_join(total_words_per_doc, by = "uuid") %>%
  mutate(sentiment_score = count/n * 100)

save(sentiment_per_doc,
     nrc_sentiment_per_doc,
     compress = TRUE,
     file = "data/sentiment-scores.RData")

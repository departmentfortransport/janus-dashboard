require(tidyverse)
require(tidytext)

responses_tidy <- read_rds("data/responses-tidy.rds")
responses_words <- read_rds("data/words-tidy.rds")

# Get the stop words (if they are not in memory)
if(!exists("stop_words")) {
  data("stop_words")
  if(exists("extra_stop_words")){
    if(length(extra_stop_words) > 0) {
      x <- data.frame(word = extra_stop_words, lexicon = "extra")
      stop_words <- rbind(stop_words, x)
    }
  }
}



#df of bigrams 
bigrams <- responses_tidy %>%
  mutate(text = stringr::str_replace_all(text, "[^a-zA-Z]", " " )) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(col = bigram, into = c('word1', 'word2') , sep = ' ') %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  tidyr::drop_na()


#bigram count for app
bigram_count <- bigrams %>%
  count(word1, word2) %>%
  ungroup %>%
  filter(n > 2) %>%
  unite(bigram, word1, word2, sep = ' ')

count_bigrams_by_col <- function(colname, bigrams_per_group = 50){
  bigrams %>%
    dplyr::count_(vars = c("word1", "word2", colname)) %>%
    ungroup %>%
    group_by_(colname) %>%
    top_n(bigrams_per_group, n) %>%
    ungroup %>%
    rename_("group" = colname) %>%
    unite(bigram, word1, word2, sep = ' ')
}

# Counts grouped by the columns
bigram_count_col1 <- count_bigrams_by_col("col1")
bigram_count_col2 <- count_bigrams_by_col("col2")
bigram_count_col3 <- count_bigrams_by_col("col3")


################################



save(bigrams,
     bigram_count,
     bigram_count_col1,
     bigram_count_col2,
     bigram_count_col3,
     compress = TRUE,
     file = "data/bigrams.RData")
require(tidyverse)
require(tidytext)

responses_words <- read_rds("data/words-tidy.rds")

# Count the number of times each word appears, in total and across dimensinons of the other cols
# Keep only a subset of the top most n frequent to keep the app size small

word_counts_all <- responses_words %>%
  dplyr::count(word) %>%
  top_n(1000, n) %>%
  ungroup

get_word_counts_by_col <- function(colname, words_per_group = 100){
  responses_words %>%
    dplyr::count_(vars = c("word", colname)) %>%
    ungroup %>%
    group_by_(colname) %>%
    top_n(words_per_group, n) %>%
    ungroup %>%
    rename_("group" = colname)
}

word_counts_col1 <- get_word_counts_by_col("col1")
word_counts_col2 <- get_word_counts_by_col("col2")
word_counts_col3 <- get_word_counts_by_col("col3")

# Save for use in the app

save(word_counts_all,
     word_counts_col1,
     word_counts_col2,
     word_counts_col3,
     file = "data/word-counts.RData")
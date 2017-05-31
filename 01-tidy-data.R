#Examples at http://tidytextmining.com/

library(tidyverse)
library(tidytext)
library(stringr)


# Load data ---------------------------------------------------------------

# Variables colx_label are defined in 00-build.R
# Renames colum names to generic col1, col2, col3 for easier programming
responses_tidy <- read_csv("data/generic-data.csv") %>%
  rename_("col1" = col1_label,
          "col2" = col2_label,
          "col3" = col3_label) %>%
  mutate_all(as.character)

saveRDS(responses_tidy, "data/responses-tidy.rds")

# Load stop words and add user defined ones
data("stop_words")
if (exists("extra_stop_words")){
  if (length(extra_stop_words) > 0) {
    x <- data.frame(word = extra_stop_words, lexicon = "extra")
    stop_words <- rbind(stop_words, x)
  }
}

# Split text into words (tokenise)
# Default regex pattern is used. A more complex one can be used in future
responses_words <- responses_tidy %>%
  mutate(text = stringr::str_replace_all(text, "[^a-zA-Z]", " " )) %>%
  unnest_tokens(word, text, format = "text") %>%
  anti_join(stop_words, by = "word")

saveRDS(responses_words, "data/words-tidy.rds")


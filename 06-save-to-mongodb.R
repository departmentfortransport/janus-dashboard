# Save the dataframes in Mongo DB

library(mongolite)
library(readr)

# Load all data frames
load('data/bigrams.RData')
load('data/sentiment-scores.RData')
load('data/word-counts.RData')
responses_tidy <- read_rds('data/responses-tidy.rds')
topic_model_betas <- read_rds('data/topic-model-beta.rds')
topic_model_gammas <- read_rds('data/topic-model-gamma.rds')
topic_model_relevant_words <- read_rds('data/topic-model-most-relevant-words.rds')

# Rename a couple for consistency
sentiment_nrc_per_doc <- nrc_sentiment_per_doc
sentiment_bing_per_doc <- sentiment_per_doc
bigram_count_all <- bigram_count

data_frames <- list(
  'bigram_count_all',
  'bigram_count_col1',
  'bigram_count_col2',
  'bigram_count_col3',
  'sentiment_nrc_per_doc',
  'sentiment_bing_per_doc',
  'topic_model_relevant_words',
  'topic_model_gammas',
  'topic_model_betas',
  'responses_tidy',
  'word_counts_all',
  'word_counts_col1',
  'word_counts_col2',
  'word_counts_col3'
)

for (df in data_frames) { 
  # Location of mongo db stored in environment variable MONGO_URI
  # https://stackoverflow.com/questions/12291418/how-can-i-make-r-read-my-environmental-variables
  m <- mongo(collection = df,
             db = 'janus',
             url = Sys.getenv('MONGO_URI')
             )
  try( # Drop the collection if it exists already
    m$drop(), 
    silent = TRUE
    )
  m$insert(
    get(df)
  )
}

# -----------------------------------------------------------------------------------------------------
# Topic model data
m <- mongo(collection = 'topic_model_lda_vis_data',
           db = 'janus',
           url = Sys.getenv('MONGO_URI'))
try( # Drop the collection if it exists already
  m$drop(), 
  silent = TRUE
)
m$import(file('data/r-topic-model.json'))



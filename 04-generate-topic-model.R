create_topic_model <- function(){
  #CREATES TOPIC MODEL AND THE LDAVIS JSON
  #REQUIRES THE FEATHER FILES
  library(tidyverse)
  library(tidytext)
  library(topicmodels)
  library(stringr)
  library(tidytext)
  # Load data ---------------------------------------------------------------
  
  responses_tidy <- read_rds("data/responses-tidy.rds")
  responses_words <- read_rds("data/words-tidy.rds")
  
  #Check number_of_topics exists as specified in 00-build.R, otherwise put it to 10
  if (!exists("number_of_topics")) {
    number_of_topics = 10
  }
  # Trim words -----------------------------------------------------------
  # Words in 50% or more of documents and words that appear in less than 2% of documents
  
  
  number_of_documents <- nrow(responses_tidy)
  
  word_document_frequency <- responses_words %>%
    unique %>% # Each word will only appear once
    count(word, sort = TRUE) %>%
    mutate(frequency = n/number_of_documents)
  
  common_words <- word_document_frequency %>%
    filter((frequency < 0.5) & (frequency > 0.02)) %>%
    select(word) %>% unlist
  
  responses_words_trimmed <- filter(responses_words, word %in% common_words)
  
  
  # Topic Modelling ---------------------------------------------------------
  
  
  responses_dtm <- responses_words_trimmed %>%
    mutate(document = uuid) %>%
    group_by(document, word) %>%
    summarise(count = n()) %>%
    ungroup %>%
    cast_dtm(document, word, count)
  
  #responses_dtm
  #92% sparse
  
  # #Fit the topic model - takes about 10 mins. Set inital value osuch that  it is closer to the convergance already found
  # #By default alpha starts at 50/k # Verbose means status reported every 3rd iteration
  # #Only run once and then saved as RDS
  # #I set a seed so its reproducable
  # topic_model <- LDA(responses_dtm, k = 20, control = list(seed = 1234, verbose = 3, alpha = 0.03))
  # saveRDS(topic_model, "data/r-topic-model-20-topics.rds")
  topic_model <- LDA(responses_dtm, k = number_of_topics, control = list(seed = 7331, verbose = 3))
  saveRDS(topic_model, "data/r-topic-model-10-topics.rds")
  
  
  
  
  # Create LDAvis json file -------------------------------------------------
  
  library(LDAvis)
  #help(createJSON, package = "LDAvis")
  
  # LDA calls the matricies beta and gamma. LDA vis calls them phi and theta respectivly. 
  # THe beta values are logarithims of the probability so we need to take exponential
  # LDA vis also requires the total number of words in each document.
  
  words_per_doc <- tidy(responses_dtm) %>%
    group_by(document) %>%
    summarise(length = sum(count)) %>%
    ungroup
  
  # Make sure the ordering is the same
  # sum(topic_model@documents != words_per_doc$document) == 0
  # PASS OK
  
  # And it requires the term frequencies
  word_frequencies <- responses_words_trimmed %>%
    group_by(word) %>%
    summarise(freq = n()) %>%
    ungroup
  
  
  #Create the JSON file for the vis
  ldavis_json <- createJSON(phi = exp(topic_model@beta), theta = topic_model@gamma, doc.length = words_per_doc$length,
                            vocab = topic_model@terms, term.frequency = word_frequencies$freq )
  
  cat(ldavis_json, file = "data/ldavis.json")
  
  # Save tidy model output ordered the same as the LDA vis ---------------------------------------------
  # reorder r topic model to match JSON
  topic_lookup_json <- data.frame(original_topic = RJSONIO::fromJSON("data/ldavis.json")$topic.order) %>%
    mutate(json_topic = row_number())
  
  # Save tidy model output for plotting
  topic_model_gamma <- tidy(topic_model, "gamma") %>%
    mutate(uuid = as.integer(document)) %>%
    left_join(topic_lookup_json, by = c("topic" = "original_topic")) %>%
    select(uuid, topic = json_topic, gamma)
  write_rds(topic_model_gamma, "data/topic-model-gamma.rds")
  
  topic_model_beta <- tidy(topic_model, "beta") %>%
    left_join(topic_lookup_json, by = c("topic" = "original_topic")) %>%
    select(term, topic = json_topic, beta)
  write_rds(topic_model_beta, "data/topic-model-beta.rds")
  
  # Most relevant words per topic -------------------------------------------------
  # Save the 6 most relevant words with lambda = 0.6
  #Probability of a term being in the topic is topic_model_beta

  #Proability of being in the corpus
  word_freq <- responses_words %>%
    group_by(word ) %>%
    summarise(count = n()) %>% 
    ungroup %>%
    mutate(prob = count / sum(count)) %>%
    arrange(-prob)
  
  #Relevance function
  # https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
  lambda = 0.6
  get_relevance <- function(lambda){
    left_join(topic_model_beta, word_freq, by = c("term" = "word")) %>%
      mutate(relevance = (lambda * log(beta)) + 
               ((1 - lambda) * log(beta / prob))) %>%
      arrange(topic, -relevance) %>% 
      mutate(positive_relevance = relevance - min(relevance)) %>%
      select(topic, term, relevance)
  }
  
  #Functon to select only the top terms from the relevance df
  get_top_n_terms <- function(topic_num, n_terms){
    a <- get_relevance(lambda = lambda) %>%
      filter(topic == topic_num) %>%
      arrange(-relevance)
    
    b <- a$term[1:n_terms]
    paste(b, collapse = ", ")
  }
  
  tibble(Topic = seq_len(number_of_topics)) %>%
    mutate(`Most relevant terms` = map_chr(Topic, get_top_n_terms, n_terms = 6)) %>%
    write_rds("data/topic-model-most-relevant-words.rds")
}

# Create the topic model
# Don't run often as it takes 5 mins
create_topic_model()


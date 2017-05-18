#Similarity function. Take some text and return a df of all the responses sorted by similarity.
# Requires topic model.rdata
# Requires responses-tidy.feather

get_similar_responses <- function(new_document){
  library(tidyverse)
  library(tidytext)
  library(stringr)
  
  # First load the jenson shanon functions
  ########################
  # https://github.com/tillbe/jsd/blob/master/R/divergence.R
  
  #' Computes the Jensen-Shannon divergence between two probabiliy distributions.
  #'
  #' @param P A probility distribution (vector summing to one).
  #' @param Q A probility distribution (vector summing to one).
  #' @return The JSD of \code{P} and \code{Q}.
  #'
  #' @examples
  #' P = prop.table(sample(1:10, 20, replace = TRUE))
  #' Q = prop.table(sample(5:15, 20, replace = TRUE))
  #'
  #' JSD(P,Q)
  #' JSD(Q,P)
  
  JSD = function(P, Q) {
    M = (P + Q)/2
    jsd = 0.5 * KLD(P, M) + 0.5 * KLD(Q, M)
    return(jsd)
  }
  
  #' Computes the Kullback-Leibler divergence between two probabiliy distributions.
  #'
  #'
  #'
  #' KLD is defined as: \deqn{KLD(A,B) = \sum{A \times \log\left(\frac{A}{B}\right)}}
  #'
  #' @param A A probility distribution (vector summing to one).
  #' @param B A probility distribution (vector summing to one).
  #' @return The KLD of \code{A} and \code{B}.
  #'
  #' @examples
  #' A = prop.table(sample(1:10, 20, replace = TRUE))
  #' B = prop.table(sample(5:15, 20, replace = TRUE))
  #'
  #' KLD(A,B)
  #' KLD(B,A)
  
  KLD = function(A, B) {
    sum(A * log(A/B))
  }
  
  # LOAD DATA
  #---------------------------------------
  #Load the topic model and data
  topic_model <- read_rds("../data/r-topic-model-10-topics.rds")
  
  original_documents <- read_rds("../data/responses-tidy.rds") 
  
  # Tidy and transform into DTM
  # ----------------------------------------------
  data("stop_words")
  
  new_words <- data.frame(text = new_document, stringsAsFactors = F) %>%
    mutate(text = str_replace_all(text, "[^a-zA-Z]", " ") ) %>%
    mutate(document = 0) %>%
    unnest_tokens(word, text, format = "text") %>%
    anti_join(stop_words, by = "word")
  
  new_dtm <- new_words %>%
    group_by(document, word) %>%
    summarise(count = n()) %>% ungroup %>%
    cast_dtm(document, word, count)
  
  # Get the document topic distributions
  new_topic_distribution <- posterior(topic_model ,new_dtm)$topics %>% as.numeric
  old_topic_distribution <- tidy(topic_model, matrix = "gamma")
  
  # Calculate jensen shanon distance
  # Quite slow - can be parallelized
  get_difference_score <- function(document_id){
    #Get the original document distribution as a numeric
    id_distribution <- filter(old_topic_distribution, document == document_id) %>%
      select(-document) %>% arrange(topic) %>% 
      select(gamma) %>% unlist 
    
    sqrt(JSD(id_distribution, new_topic_distribution)^2)
  } 
  
  similarity_scores <- select(old_topic_distribution, document) %>% unique %>%
    mutate(difference = map_dbl(document, get_difference_score)) 
  
  output_similarity_df <- similarity_scores  %>%
    inner_join(original_documents, by = c("document" = "uuid")) %>% 
    unique %>% arrange(difference)
  
  return(output_similarity_df)
  
}
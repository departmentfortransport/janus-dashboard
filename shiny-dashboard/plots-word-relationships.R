#Functions for word relationships plots
library(tidyverse)
library(igraph)
library(ggraph)
load("../data/bigrams.RData")

plot_word_combo_counts <- function(title_text, n_words_to_plot, facet_by = NULL){
  # Returns a bar chart giving the n most common bigrams and the frequency
  # If a facet_by string is given e.g. ("col1") it will facet by that colum 
  if(is.null(facet_by)){
    df <- bigram_count %>%
      top_n(n_words_to_plot, n) %>%
      arrange(n) %>%
      mutate(bigram = forcats::as_factor(bigram))
    p <- ggplot(df, aes(x = bigram, y = n )) + geom_bar(stat = "identity") + 
      coord_flip() + ggtitle(title_text) + xlab("Word combo") + ylab("Count")
  }
  if(!is.null(facet_by)){
    df_name <- paste0("bigram_count_", facet_by)
    df <- get(df_name) %>%
      group_by(group) %>%
      top_n(n_words_to_plot, n) %>%
      ungroup %>%
      arrange(group, n) %>%
      mutate(order = row_number())
    p <- ggplot(df, aes(x = order, y = n)) + geom_bar(stat = "identity") +
      facet_wrap(~group, ncol = 2, scales = "free") + 
      scale_x_continuous(
        breaks = df$order,
        labels = df$bigram,
        expand = c(0,0)) +
      coord_flip() + ggtitle(title_text) + xlab("Word combo") + ylab("Count")
  }
  p
}

plot_before_after <- function(title_text, n_words_to_plot, word, before_or_after){
  # Plots the frequencies of words that come before or after the given word
  # before_or_after is a string either "Before" or "After"
  word <- tolower(word)
  df <- bigram_count %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  if (before_or_after == "Before"){
    df <- df %>%
      filter(word2 == word) %>%
      select(related_word = word1, n)
    if(nrow(df) == 0) return(NULL)
  } else if (before_or_after == "After"){
    df <- df %>%
      filter(word1 == word) %>%
      select(related_word = word2, n)
    if(nrow(df) == 0) return(NULL)
  } else return(NULL)
  df %>%
    top_n(n_words_to_plot, n) %>%
    arrange(n) %>%
    mutate(related_word = forcats::as_factor(related_word)) %>%
    ggplot(aes(x = related_word, y = n)) + geom_bar(stat = "identity") +
    coord_flip() + ggtitle(title_text) + xlab("Related word") + ylab("Count")
}

plot_word_network <- function(number_of_links){
  #Code from http://tidytextmining.com/ngrams.html
  bigram_graph <- bigram_count %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    top_n(number_of_links, n) %>%
    graph_from_data_frame()
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

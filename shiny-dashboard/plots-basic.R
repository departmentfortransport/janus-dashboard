library(tidyverse)
library(wordcloud)
library(tidytext)

load("../data/word-counts.RData")
load("../data/sentiment-scores.RData")
sentiments_bing <- tidytext::get_sentiments("bing")
sentiments_nrc <- tidytext::get_sentiments("nrc")

# Loads basic word counts data frames
# word_counts_all, word_counts_colx


# Word Count Bars ---------------------------------------------------------

plot_word_counts <- function(title_text, n_words_to_plot, facet_by = NULL){
  # Returns a word count bar chart
  # If a facet_by string is given e.g. ("col1") it will facet by that colum 
  
  if(is.null(facet_by)){
    #Select the top words and convert the word column to a ordered factor for plotting
    df <- word_counts_all %>%
      top_n(n_words_to_plot, n) %>%
      arrange(n) %>%
      mutate(word = forcats::as_factor(word))
    
    p <- ggplot(df, aes(x = word, y = n )) + geom_bar(stat = "identity") + 
      coord_flip() + ggtitle(title_text) + xlab("Word") + ylab("Count")
  }
  
  if(!is.null(facet_by)){
    if(facet_by %in% c("col1", "col2", "col3")){
      #Select the top words and convert the word column to a ordered factor for plotting
      df_name <- paste0("word_counts_", facet_by)
      df <- get(df_name) %>%
        group_by(group) %>%
        top_n(n_words_to_plot, n) %>%
        ungroup %>%
        arrange(group, n) %>%
        mutate(order = row_number())
        
      # The plot with the help of
      # https://www.r-bloggers.com/ordering-categories-within-ggplot2-facets/
      p <- ggplot(df, aes(x = order, y = n)) + geom_bar(stat = "identity") +
        facet_wrap(~group, ncol = 2, scales = "free") + 
        scale_x_continuous(
          breaks = df$order,
          labels = df$word,
          expand = c(0,0)) +
        coord_flip() + ggtitle(title_text) + xlab("Word") + ylab("Count")
      
      
    }
  }
  p
}


# Word Cloud --------------------------------------------------------------

plot_word_cloud <- function(n_words_to_plot, filter_by = NULL, filter_value = NULL  ){
  # Return a word cloud plot
  # If filter by is supplied you also have to supply a value
  # eg facet_by = "col2", facet_value = "London"
  
  if(is.null(filter_by)){
    p <- wordcloud(word_counts_all$word, word_counts_all$n,
              max.words = n_words_to_plot, colors = brewer.pal(4, "Dark2"))
  }
  
  if(!is.null(filter_by)){
    if(filter_by %in% c("col1", "col2", "col3")){
      df_name <- paste0("word_counts_", filter_by)
      df <- get(df_name) %>%
        filter(group == filter_value)
      
      p <- wordcloud(df$word, df$n,
                max.words = n_words_to_plot, colors = brewer.pal(4, "Dark2"))
    }
  }
  p
}


# Word cloud by column ----------------------------------------------------
plot_split_word_cloud <- function(n_words_to_plot, compare_col){
  df_name <- paste0("word_counts_", compare_col)
  df <- get(df_name) %>%
    spread(group, n, fill = 0) %>%
    as.data.frame
  rownames(df) <- df$word
  df$word <- NULL
  if(length(df) <= 6){
    comparison.cloud(as.matrix(df), max.words = n_words_to_plot)
  }
} 

# Word cloud by sentiment -------------------------------------------------

# Helper function to convert to matrix with word as row names
as_word_matrix <- function(df, group_col){
  x <- df %>%
    spread_(group_col, "n") %>%
    as.data.frame
  rownames(x) <- x$word
  x$word <- NULL
  as.matrix(x)
}

plot_sentiment_cloud <- function(n_words_to_plot, filter_by = NULL, filter_value = NULL ){
  # Creates a word cloud seperated by positive and negative words. 
  # If a filter_by col and filter_value is given the word cloud is drawn from a filtered down df
  if(is.null(filter_by)){
    df <- word_counts_all %>% 
      inner_join(sentiments_bing, by = "word") %>%
      spread(sentiment, n, fill = 0) %>%
      as.data.frame
    rownames(df) <- df$word
    df$word <- NULL
    p <- comparison.cloud(as.matrix(df), max.words = n_words_to_plot, 
                     colors = c("#F8766D", "#00BFC4"), title.size=2.5, scale=c(5,1.5))
  }
  if(!is.null(filter_by)){
    if(filter_by %in% c("col1", "col2", "col3")){
      df_name <- paste0("word_counts_", filter_by)
      df <- get(df_name) %>%
        filter(group == filter_value) %>%
        select(-group) %>%
        inner_join(sentiments_bing, by = "word") %>%
        spread(sentiment, n, fill = 0) %>%
        as.data.frame
      rownames(df) <- df$word
      df$word <- NULL
      # Add a check to see if there are no entries for one of the sentiments which makes word cloud fail
      if(length(names(df)) != 2) return(NULL)
      p <- comparison.cloud(as.matrix(df), max.words = n_words_to_plot, 
                       colors = c("#F8766D", "#00BFC4"))
    }
  }
  p
}


# Sentiment score ---------------------------------------------------------


plot_sentiment_score <- function(title_text, split_by = NULL){
  if(is.null(split_by)){
    avg_score <- mean(sentiment_per_doc$sentiment_score)
    p <- sentiment_per_doc %>%
      ggplot(aes(sentiment_score)) + geom_histogram(binwidth = 2) +
      xlab("Sentiment score")
  }
  if(!is.null(split_by)){
    if(split_by %in% c("col1", "col2", "col3")){
      df <- sentiment_per_doc %>%
        group_by_(split_by) %>%
        summarise(sentiment_score = mean(sentiment_score)) %>%
        ungroup %>%
        rename_("group" = split_by) %>%
        arrange(sentiment_score)
      
      p <- ggplot(df, aes(x = forcats::as_factor(group), y = sentiment_score)) + 
        geom_bar(stat = "identity") +
        coord_flip() +
        xlab("Category") +
        ggtitle(title_text) +
        ylab("Average sentiment score")
        
    }
  }
  p
}


# NRC Score ---------------------------------------------------------------
plot_emotion_score <- function(title_text, filter_by = NULL, filter_value = NULL){
  # Bar chart giving percent of words falling into each NRC category
  if(is.null(filter_by)){
    df <- nrc_sentiment_per_doc %>%
      filter(sentiment != "none") %>%
      group_by(sentiment) %>%
      summarise(sentiment_score = mean(sentiment_score)) %>%
      ungroup %>%
      arrange(sentiment_score)
    
    p <- ggplot(df, aes(x = forcats::as_factor(sentiment), y = sentiment_score)) +
             geom_bar(stat = "identity") +
             coord_flip() +
             xlab("Category") +
             ylab("Average percent of words classified in the category") +
             ggtitle(title_text)
  }
  if(!is.null(filter_by)){
    if(filter_by %in% c("col1", "col2", "col3")){
      df <- nrc_sentiment_per_doc %>%
        filter(sentiment != "none") %>%
        filter_(paste0(filter_by, "== '", filter_value, "'")) %>%
        group_by(sentiment) %>%
        summarise(sentiment_score = mean(sentiment_score)) %>%
        ungroup %>%
        arrange(sentiment_score)
      
      p <- ggplot(df, aes(x = forcats::as_factor(sentiment), y = sentiment_score)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        xlab("Category") +
        ylab("Average percent of words classified in the category") +
        ggtitle(title_text)
    }
  }
  p
}

# NRC Score breakdown of words --------------------------------------------
plot_emotion_word_breakdown <- function(title_text, n_words_to_plot, filter_by = NULL, filter_value = NULL){
  # Word count plot facetted by NRC grouping
  my_bar_plot <- function(df){
    a <- df %>%
      left_join(sentiments_nrc, by = "word") %>%
      replace_na(list(sentiment = "none")) %>%
      group_by(sentiment) %>%
      top_n(n_words_to_plot, n) %>%
      ungroup %>%
      arrange(sentiment, n) %>%
      mutate(order = row_number())
    
    ggplot(a, aes(x = order, y = n)) + geom_bar(stat = "identity") +
      facet_wrap(~sentiment, ncol = 3, scales = "free") + 
      scale_x_continuous(
        breaks = a$order,
        labels = a$word,
        expand = c(0,0)) +
      coord_flip() + ggtitle(title_text)
  }
  if(is.null(filter_by)){
    p <- my_bar_plot(word_counts_all)
  }
  if(!is.null(filter_by)){
    if(filter_by %in% c("col1", "col2", "col3")){
      df_name <- paste0("word_counts_", filter_by)
      p <- get(df_name) %>%
        filter(group == filter_value) %>%
        my_bar_plot()
    }
  }
  p
}

# #TESTING
# plot_split_word_cloud(100, "col3")
# plot_split_word_cloud(100, "col1")
# plot_emotion_score("title") 
# plot_emotion_score("title", "col3", "GB")
# plot_emotion_word_breakdown("title", 8)
# plot_emotion_word_breakdown("title", 6, "col2", "bs")
# plot_sentiment_score("title") 
# plot_sentiment_score("title", "col1")
# plot_word_cloud(200) #Plots and then returns NULL
# plot_word_cloud(100, "col3", "GB") #Plots and then returns NULL
# plot_sentiment_cloud(400) 
# plot_sentiment_cloud(100, "col2", "bias") 
# plot_word_counts("title", 10) 
# plot_word_counts("title", 10, "col3")

library(shiny)
library(dplyr)
source("plots-basic.R")
source("dataframe-filter.R")
source("plots-word-relationships.R")
source("plots-topics.R")
source("document-similarity-funcs.R")

df_responses_tidy <- read_rds("../data/responses-tidy.rds")

label_col1 <- "Site"
label_col2 <- "Type"
label_col3 <- "Country"

get_column_values <- function(column_name){
  if(column_name == "None") return(NULL)
  x <- df_responses_tidy %>% 
    select_(column_name) %>% unique %>%
    unlist
  names(x) <- NULL
  x
}

get_column_label <- function(column_name){
  if(column_name == "col1") return(tolower(label_col1))
  if(column_name == "col2") return(tolower(label_col2))
  if(column_name == "col3") return(tolower(label_col3))
  else NULL
}

shinyServer(function(input, output) {
  
  # Dynamic UI Controls ----------------------------------------------------------------
  output$controls_word_cloud <- renderUI({
    if(input$input_word_cloud_filter_by != "None"){
      selectInput("input_word_cloud_filter_value", "Filter to",
                  choices = get_column_values(input$input_word_cloud_filter_by))
    }
  })
  output$controls_sentiment_cloud <- renderUI({
    if(input$input_sentiment_cloud_filter_by != "None"){
      selectInput("input_sentiment_cloud_filter_value", "Filter to",
                  choices = get_column_values(input$input_sentiment_cloud_filter_by))
    }
  })
  output$controls_emotion_score <- renderUI({
    if(input$input_emotion_score_filter_by != "None"){
      selectInput("input_emotion_score_filter_value", "Filter to",
                  choices = get_column_values(input$input_emotion_score_filter_by))
    }
  })
  output$controls_emotion_breakdown <- renderUI({
    if(input$input_emotion_breakdown_filter_by != "None"){
      selectInput("input_emotion_breakdown_filter_value", "Filter to",
                  choices = get_column_values(input$input_emotion_breakdown_filter_by))
    }
  })
  
  # Word and sentiment plots --------------------------------------------------------------
  output$plot_word_counts <- renderPlot({
    if(input$input_word_counts_facet_by == "None"){
      plot_word_counts("Word counts", input$input_word_counts_n)
    } else{
      t <- paste0("Word counts split by ", get_column_label(input$input_word_counts_facet_by))
      plot_word_counts(t, input$input_word_counts_n, input$input_word_counts_facet_by)
    } 
  })
  output$plot_word_cloud <- renderPlot({
    if(input$input_word_cloud_filter_by == "None"){
      plot_word_cloud(input$input_word_cloud_n)
    } else {
      plot_word_cloud(input$input_word_cloud_n, 
                      input$input_word_cloud_filter_by,
                      input$input_word_cloud_filter_value)
    }
  })
  output$plot_split_word_cloud <- renderPlot({
    plot_split_word_cloud(input$input_split_word_cloud_n, input$input_split_word_cloud_split_by)
  })
  
  output$plot_sentiment_score <- renderPlot({
    if(input$input_sentiment_score_split_by == "None"){
      plot_sentiment_score("Sentiment score")
    } else {
      t <- paste0("Sentiment score by ", get_column_label(input$input_sentiment_score_split_by))
      plot_sentiment_score(t, input$input_sentiment_score_split_by)
    }
  })
  output$plot_sentiment_cloud <- renderPlot({
    if(input$input_sentiment_cloud_filter_by == "None"){
      plot_sentiment_cloud(input$input_sentiment_cloud_n)
    } else {
      plot_sentiment_cloud(input$input_sentiment_cloud_n,
                           input$input_sentiment_cloud_filter_by,
                           input$input_sentiment_cloud_filter_value)
    }
  })
  output$plot_emotion_score <- renderPlot({
    if(input$input_emotion_score_filter_by == "None"){
      plot_emotion_score("Emotion scores")
    } else {
      t <- paste0("Emotion scores where ", 
                  get_column_label(input$input_emotion_score_filter_by),
                  " = ",
                  input$input_emotion_score_filter_value)
      plot_emotion_score(t,
                         input$input_emotion_score_filter_by,
                         input$input_emotion_score_filter_value)
    }
  })
  output$plot_emotion_breakdown <- renderPlot({
    if(input$input_emotion_breakdown_filter_by == "None"){
      plot_emotion_word_breakdown("Most frequent words by emotion",
                             input$input_emotion_breakdown_n)
    } else {
      t <- paste0("Most frequent words by emotion where ",
                  get_column_label(input$input_emotion_breakdown_filter_by),
                  " = ",
                  input$input_emotion_breakdown_filter_value)
      plot_emotion_word_breakdown(t,
                             input$input_emotion_breakdown_n,
                             input$input_emotion_breakdown_filter_by,
                             input$input_emotion_breakdown_filter_value)
    }
  })
  # Word relationship plots -------------------------------------------------
  output$plot_word_combo_counts <- renderPlot({
    if(input$input_word_combo_facet_by == "None"){
      plot_word_combo_counts("Most common word combinations", input$input_word_combo_n)
    } else {
      t <- paste0("Most common word combinations split by ", get_column_label(input$input_word_counts_facet_by))
      plot_word_combo_counts(t, input$input_word_combo_n, input$input_word_combo_facet_by)
    }
  })
  output$plot_before_after <- renderPlot({
    t <- paste("Most common words appearing",
                tolower(input$input_before_after_radio),
                tolower(input$input_before_after_word))
    plot_before_after(title_text = t, 
                      n_words_to_plot = input$input_before_after_n,
                      word = input$input_before_after_word,
                      before_or_after = input$input_before_after_radio)
  })
  output$plot_word_network <- renderPlot({
    plot_word_network(input$input_word_network_n)
  })
  
  #Topic model page ----------------------------------------
  output$LDAvis <- renderVis(
    read_file("../data/ldavis.json")
  )
  
  output$plot_topic_differences <- renderPlot(
    plot_topic_differences(input$topic_a, input$topic_b)
  )
  
  output$table_all_topics_relevant_terms <- renderTable(
    read_rds("../data/topic-model-most-relevant-words.rds")
  )
  
  output$plot_topic_coverage <- renderPlot(
    if (input$topic_coverage_split_by == "None") {
      plot_topic_total_coverage()
    } else {
      plot_topic_total_coverage(input$topic_coverage_split_by)
    }
  )
  

  # Topic similarity -------------------------------------
  #Similarity page
  
  similarity_df <- eventReactive(input$similarity_go_Button, {
    get_similar_responses(input$similarity_text_input)
  })
  output$similar_responses_data_table <- renderDataTable(
    similarity_df()
  )
  
  ############## main page edit nadeem ############################
  # having to call col1 etc isnt generic to any data set. label_col1 doesnt work
  # 'more filters' box isn't used yet
  
  filtered_df <- eventReactive(input$filter_go, {
    dataframe_filter(df_responses_tidy, input$filter_col1, input$filter_col2, input$filter_col3) %>%
      change_df_col(label_col1, label_col2, label_col3)
  })
  

  output$responses_tidy <- renderDataTable({

      filtered_df()
  })

  #################################################################
})



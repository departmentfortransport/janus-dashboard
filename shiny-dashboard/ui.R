library(shiny)
library(shinydashboard)
library(LDAvis)
library(tidyverse)

df_responses_tidy <- read_rds("../data/responses-tidy.rds")

label_col1 <- "Site"
label_col2 <- "Type"
label_col3 <- "Country"

options_columns <- list("None", "col1", "col2", "col3")
names(options_columns) <- c("None", label_col1, label_col2, label_col3)

options_columns_without_none <- list("col1", "col2", "col3")
names(options_columns_without_none) <- c(label_col1, label_col2, label_col3)


shinyUI(
  dashboardPage(
    dashboardHeader(title = "Janus"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Text explorer", tabName = "tab_explorer", icon = icon("dashboard")),
        menuItem("Word analysis", tabName = 'tab_word_analysis', icon = icon('bar-chart')),
        menuItem("Sentiment analysis",tabName = 'tab_sentiment', icon = icon('smile-o')),
        menuItem("Word relationships", tabName = 'tab_word_relationships', icon = icon('arrows-alt')),
        menuItem("Topics", tabName = "tab_topics", icon = icon("th")),
        menuItem("Find similar items", tabName = "tab_similar", icon = icon("flask"))
        
      )
    ),
    dashboardBody(
      fluidRow(
        tabItems(
          # First tab content
          tabItem(tabName = "tab_explorer",
                  column( width = 12, offset = 0.5, 
                          h2("Table Filter"), 
                  # Nadeem is doing
                  fluidRow(
                  wellPanel( h4("Filter the table by the categories in each column"), 

                      if ( length(unique(df_responses_tidy$col1)) < 5 ){
                      checkboxGroupInput("filter_col1",
                                         label = label_col1, # not showing up
                                         choices = unique(df_responses_tidy$col1) )
                        }else{
                      selectInput("filter_col1", label=label_col1, choices=unique(df_responses_tidy$col1), multiple=TRUE)
                    },
                    

                      if ( length(unique(df_responses_tidy$col2)) < 5 ){
                      checkboxGroupInput("filter_col2",
                                         label = label_col2,
                                         choices = unique(df_responses_tidy$col2) )
                      }else{
                        selectInput("filter_col2", label=label_col2, choices=unique(df_responses_tidy$col2), multiple=TRUE)
                    },
                    

                      if ( length(unique(df_responses_tidy$col3)) < 5 ){
                      checkboxGroupInput("filter_col3",
                                         label = label_col3,
                                         choices = unique(df_responses_tidy$col3) )
                      }else{
                        selectInput("filter_col3", label=label_col3, choices=unique(df_responses_tidy$col3), multiple=TRUE)
                    },
                    
                
                      #checkboxGroupInput("filter_sentiment",
                      #                   label = "Sentiment",
                      #                   inline = TRUE, #inline not working
                      #                   choices = c("Positive",
                      #                               "Negative"),
                      #                   selected = c("Positive",
                      #                                "Negative")),
                      # numericInput("filter_min_words",
                      #              label = "Minimum number of words",
                      #              value = 0),
                      # numericInput("filter_max_words",
                      #              label = "Maximum number of words",
                      #              value = 1000),
                    
                  
                      actionButton("filter_go", "Go")
                    
                  )),
                  column(12,
                  dataTableOutput("responses_tidy"))
          )),
                  
          tabItem(tabName = "tab_word_analysis",
                  tabBox(width = 12,
                         tabPanel("Word Counts",
                                  wellPanel(
                                    selectInput("input_word_counts_facet_by", "Split by",
                                                choices = options_columns),
                                    numericInput("input_word_counts_n", "Number of words",
                                                 value = 12, min = 3)),
                                  plotOutput("plot_word_counts", height = "500px")),
                         tabPanel("Word Cloud",
                                  wellPanel(
                                    selectInput("input_word_cloud_filter_by", "Filter by",
                                                choices = options_columns),
                                    uiOutput("controls_word_cloud") ,
                                    numericInput("input_word_cloud_n", "Maximum number of words to show",
                                                 value = 50, min = 10),
                                    plotOutput("plot_word_cloud")
                                  )),
                         tabPanel("Split Word Cloud",
                                  wellPanel(
                                    "The dimension you choose must only have a few categories for the plot to show",
                                    selectInput("input_split_word_cloud_split_by", "Split by",
                                                choices = options_columns_without_none),
                                    numericInput("input_split_word_cloud_n", "Maximum nunber of words to show",
                                                 value = 50, min = 10)),
                                  plotOutput("plot_split_word_cloud")
                         )
                  )),
          tabItem(tabName = "tab_sentiment",
                  tabBox(width = 12,
                         tabPanel("Sentiment",
                                  h4("Sentiment score"),
                                  wellPanel(
                                    selectInput("input_sentiment_score_split_by", "Split by",
                                                choices = options_columns)),
                                  plotOutput("plot_sentiment_score"),
                                  h4("Cloud of words contributing to sentiment score"),
                                  wellPanel(
                                    selectInput("input_sentiment_cloud_filter_by", "Filter by",
                                                choices = options_columns),
                                    uiOutput("controls_sentiment_cloud"),
                                    numericInput("input_sentiment_cloud_n", "Maximum number of words to show",
                                                 value = 50, min = 10)),
                                    plotOutput("plot_sentiment_cloud"),
                                  hr(),
                                  "Sentiment score is calculated as:",
                                  withMathJax(paste0("$$ \\frac{(\\text{number of positive words} -",
                                                     "\\text{number of negative words})}",
                                                     "{\\text{total number of words}}*100 $$"))
                                  ),
                         tabPanel("Emotion",
                                  h4("Emotion scores"),
                                     wellPanel(
                                       selectInput("input_emotion_score_filter_by", "Filter by",
                                                   choices = options_columns),
                                       uiOutput("controls_emotion_score")),
                                       plotOutput("plot_emotion_score"),
                                  h4("Words contributing to the emotion score"),
                                  wellPanel(
                                    selectInput("input_emotion_breakdown_filter_by", "Filter by",
                                                choices = options_columns),
                                    uiOutput("controls_emotion_breakdown"),
                                    numericInput("input_emotion_breakdown_n", "Number of words to show",
                                                 value = 8, min = 2)),
                                  plotOutput("plot_emotion_breakdown", height = "600px"),
                                  hr(),
                                  paste0("The emotion score is the percent of words", 
                                         "that fall into the given emotion category")
                                     )
                         )
          ),
          tabItem(tabName = "tab_word_relationships",
                  tabBox(width = 12,
                    tabPanel("Word combos",
                             h4("The most frequeny 2-word combinations"),
                             wellPanel(
                               selectInput("input_word_combo_facet_by", "Split by",
                                           choices = options_columns),
                               uiOutput("controls_word_combo"),
                               numericInput("input_word_combo_n", "Number of combinations to show",
                                            value = 10, min = 1)
                             ),
                             plotOutput("plot_word_combo_counts")
                    ),     
                    tabPanel("Words before and after",
                             h4("Enter a word and see what most often comes before or after it"),
                             wellPanel(
                               textInput("input_before_after_word", "Word"),
                               radioButtons("input_before_after_radio", label = "What words come..",
                                            choices = c("Before", "After")),
                               numericInput("input_before_after_n", "Maximum number of words to show",
                                            value = 10, min = 1)
                             ),
                             plotOutput("plot_before_after")),
                    tabPanel("Word network",
                             wellPanel(
                               numericInput("input_word_network_n", "Number of links to show",
                                            value = 25, min = 2)),
                             plotOutput("plot_word_network"))
                  )
          ),
          tabItem(tabName = "tab_topics",
                  tabBox(width = 12,
                         tabPanel("Topics",
                                  h4("The most relvant terms in each topic"),
                                     tableOutput("table_all_topics_relevant_terms"),
                                  h4("How common is each topic?"),
                                  wellPanel(
                                    selectInput("topic_coverage_split_by",
                                                label = "Split the topic coverage by:",
                                                choices = options_columns)
                                  ),
                                  plotOutput("plot_topic_coverage")),
                         tabPanel("Topic differences",
                                  h4("Select two topics and explore how they differ"),
                                  wellPanel(
                                    selectInput("topic_a", label = "Select first topic", choices = seq(10), selected = 1 ),
                                    selectInput("topic_b", label = "Select second topic", choices = seq(10), selected = 2 )
                                    ),
                                  plotOutput("plot_topic_differences")
                                  ),
                         tabPanel("Topics in depth",
                                  h3("Topic model explorer"),
                                  "Put some explanatory text here",
                                  visOutput("LDAvis")
                         )
          )),
          tabItem(tabName = "tab_similar",
                  fluidRow(
                    column(
                      width = 11, offset = 0.5,
                      box(width = 12,
                        textAreaInput("similarity_text_input", "Enter some text and find similar items",
                                      "Type or copy/paste your text here ..."),
                        actionButton("similarity_go_Button", "Go!")
                        ),
                      dataTableOutput("similar_responses_data_table")
                      )
                  )
                  
          )
        )
      )

    )
  )
)
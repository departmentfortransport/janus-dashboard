library(shinydashboard)
library(LDAvis)
library(tidyverse)



load("../data/counts-sentiment-bigrams.RData")
source("topic-plot-funcs.R")
source("document-similarity-funcs.R")
source("plots.R")
source("topic-relevance-function.R")

ui <- dashboardPage(
  dashboardHeader(title = "Janus"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Response explorer", tabName = "tab_explorer", icon = icon("dashboard")),
      menuItem("Plots", tabName = 'static', icon = icon('bar-chart')),
      menuItem("Topics", tabName = "tab_topics", icon = icon("th")),
      menuItem("Topic differences", tabName = "tab_differences", icon = icon("comment")),
      menuItem("Similar responses", tabName = "tab_similar", icon = icon("flask")),
      menuItem("Word relationships", tabName = 'bi_words', icon = icon('arrows-alt'))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "tab_explorer",
              fluidRow(
                box(
                  width = 4,
                  title = "Questions",
                    status = "info",
                    checkboxGroupInput("filter_q",
                                       label = "Question",
                    choices = c("Unstructured",
                                "Q1",
                                "Q2",
                                "Q3",
                                "Q4",
                                "Q5"))
                    ),
                
                box(
                  width = 4,
                  title = "Response Type",
                  status = "info",
                  checkboxGroupInput("filter_response_type",
                                     label = NULL,
                                     choices = c("Organisation",
                                                 "Individual",
                                                 "Email - no attachment",
                                                 "Email - attachment",
                                                 "Survey monkey"),
                                     selected = c("Organisation",
                                                  "Individual",
                                                  "Email - no attachment",
                                                  "Email - attachment",
                                                  "Survey monkey"))
                ),
                box(
                  width = 4,
                  title = "More filters",
                  status = "info",
                  checkboxGroupInput("filter_sentiment",
                                     label = "Sentiment",
                                     inline = TRUE, #inline not working
                                     choices = c("Positive",
                                                 "Negative"),
                                     selected = c("Positive",
                                                  "Negative")),
                  numericInput("filter_min_words",
                               label = "Minimum number of words",
                               value = 0),
                  numericInput("filter_max_words",
                               label = "Maximum number of words",
                               value = 1000)
                )
              ),
            fluidRow(
              box(
                width = 4,
                solidHeader = TRUE,
                status = "success",
                actionButton("filter_go", "Go")
              )
            ),
            dataTableOutput("main_data_table")
      ),
      
      # Second tab content
      tabItem(tabName = "tab_topics",
              visOutput("LDAvis"),
              tableOutput("table_all_topics_top_terms"),
              plotOutput("plot_topic_percent_by_Q", height = "800px")
      ),
      
      tabItem(tabName = "static",
              fluidPage(
                navlistPanel(
                  tabPanel('Word Count', plotOutput('basic_wc')),
                  tabPanel('Bing Grouping Word Count', plotOutput('bing_wc')),
                  tabPanel('NRC Grouping Word Count', plotOutput('nrc_wc')),
                  tabPanel('Bing Word Cloud',
                           wellPanel(
                             selectInput(inputId = 'select', label = 'Select Question', selected = NULL,
                                         choices = as.list(question_words$question) )),
                           plotOutput('word_cloud')),
                  tabPanel('Distribution by Type', plotOutput('type_dist')),
                  tabPanel('Sentiment score of Questions', plotOutput('ques_sent', height = "600px"))
                )
              )
      ),
      
      tabItem(tabName = "tab_similar",
              textAreaInput("similarity_text_input", "Enter a consultation response and find similar responses",
                            "I think night flights are ..."),
              actionButton("similarity_go_Button", "Go!"),
              dataTableOutput("similar_responses_data_table")
      ),
      tabItem(tabName = "bi_words",
              h2("Enter a word and see what it preceeds"),
              fluidPage(
                tabsetPanel(
                  tabPanel('Word Sentiment',
                           wellPanel(
                             textInput(inputId = 'word', value = '', label = 'Search Term'),
                             radioButtons('button', 'Sentiment type:',
                                          c('Before' = 'prec', 'After' = 'proc')
                             ),
                             actionButton(inputId = 'go',
                                          label = 'Update')),
                           plotOutput('barchart')
                  ),
                  tabPanel('Vertex Graph', plotOutput('vertex_graph'))
                )
              )
      ),
      tabItem(tabName = "tab_summary",
              h2("Plots showing differences accross questions")
      ),
      tabItem(tabName = "tab_differences",
              selectInput("topic_a", label = "Select first topic", choices = seq(10), selected = 1 ),
              selectInput("topic_b", label = "Select second topic", choices = seq(10), selected = 2 ),
              plotOutput("plot_topic_differences")
      )
    )
  )
)

server <- function(input, output) {
  
  # TOPIC VIS -------------------------

  
  clouds <- eventReactive( input$select,{
    word_cloud(input$select)
  })

  
  # Main data table page
  responses_df_output <- eventReactive(input$filter_go, {
    read_rds("../data/responses-tidy.rds")
  })
  output$main_data_table <- renderDataTable(
    responses_df_output()
  )
  #Topic model page
  output$LDAvis <- renderVis(
    read_file("../data/ldavis.json")
  )
  
  output$plot_topic_differences <- renderPlot(
    plot_differences(input$topic_a, input$topic_b)
  )
  
  output$table_all_topics_top_terms <- renderTable(
    get_topic_relevance_df(lambda = 0.6, n_terms = 8)
  )
  
  output$plot_topic_percent_by_Q <- renderPlot(
    plot_topic_percentage_by_Q()
  )

  #Similarity page
  
  similarity_df <- eventReactive(input$similarity_go_Button, {
    get_similar_responses(input$similarity_text_input)
  })
  output$similar_responses_data_table <- renderDataTable(
    similarity_df()
  )
  
  
  #Static plots
  output$basic_wc <- renderPlot({ 
    basic_wc
  })
  output$bing_wc <- renderPlot({
    bing_wc
  })
  output$nrc_wc <- renderPlot({
    nrc_wc
  })
  output$word_cloud <- renderPlot({
    clouds()
  })
  output$type_dist <- renderPlot({
    type_dist
  })
  output$ques_sent <- renderPlot({
    ques_sent_plot
  })
  #####
  
  data <- eventReactive(input$go, {switch(input$button,
                                          prec = prec_function(input$word),
                                          proc = proc_function(input$word))})
  
  output$barchart <- renderPlot({ 
    
    #code for output, with input$x as the input for the graph
    data()
    
  })

  #vertex graph
  output$vertex_graph <- renderPlot({ 
    vertex_graph
  })
  


}

shinyApp(ui, server)
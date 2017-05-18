library(tidyverse)
library(topicmodels)

topic_model_beta <- read_rds("../data/topic-model-beta.rds")
topic_model_gamma <- read_rds("../data/topic-model-gamma.rds")
df_responses_tidy <- read_rds("../data/responses-tidy.rds")

# Topic coverage
plot_topic_total_coverage <- function(facet_by = NULL){
  if (is.null(facet_by)) {
    df <- topic_model_gamma %>%
      group_by(topic) %>%
      summarise(total_gamma = sum(gamma)) %>%
      ungroup %>%
      mutate(number_of_docs = length(unique(topic_model_gamma$uuid)),
             coverage = total_gamma / number_of_docs )
    p <- ggplot(df, aes(factor(topic), coverage)) + geom_bar(stat = "identity") +
      coord_flip() + ggtitle("Coverage of each topic across all the text") +
      xlab("Topic number") + ylab("Coverage (% of text)") +
      scale_y_continuous(labels = scales::percent)
  } else if (facet_by %in% c("col1", "col2", "col3")) {
    # Do a faceted plot
    column_data <- select(df_responses_tidy, -text)
    docs_per_col <- column_data %>%
      group_by_(facet_by) %>%
      count() %>%
      ungroup
    df <- topic_model_gamma %>%
      mutate(uuid = as.character(uuid)) %>%
      left_join(column_data, by = "uuid") %>%
      group_by_("topic", facet_by) %>%
      summarise(total_gamma = sum(gamma)) %>%
      ungroup %>%
      left_join(docs_per_col, by = facet_by) %>%
      mutate(coverage = total_gamma / n) %>%
      arrange_(facet_by, "coverage") %>%
      mutate(order = row_number())
    
    p <- ggplot(df, aes(x = order, y = coverage)) + geom_bar(stat = "identity") +
      coord_flip() + ggtitle("Coverage of each topic across all the text") +
      xlab("Topic number") + ylab("Coverage (% of text)") +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(c(facet_by), ncol = 2, scales = "free") + 
      scale_x_continuous(
        breaks = df$order,
        labels = df$topic,
        expand = c(0,0))
  }
  p
}
  



# Difference plot ---------------------------------------------------------
# Difference between two topics
plot_topic_differences <- function(topic_a_number, topic_b_number){
  
  topic_a <- str_c("topic", topic_a_number)
  topic_b <- str_c("topic", topic_b_number)
  beta_spread <- topic_model_beta %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    select_("term", topic_a, topic_b) %>%
    mutate_("difference" = str_c(topic_b, " - ",  topic_a)) %>%
    arrange(difference)
  
  rbind(head(beta_spread, 10), tail(beta_spread, 10)) %>%
    arrange(difference) %>%
    ggplot(aes(x = reorder(term, difference), y = difference)) + geom_bar(stat = "identity") +
    coord_flip() + ggtitle(str_c("Difference between topics " , topic_a_number, " and ", topic_b_number ))
}
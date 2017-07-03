# The script takes the generic_data.csv file in the data folder and creates all
# subsequent dataframes and topic model obejcts that are required for the 
# dashboard

#source("make generic data.R")

# Uncomment if you want to add stopwords
#extra_stop_words <- c("some", "extra", "stopwords", "to", "exclude")

# Topics for topic modelling. Keep low so they topics fit on the dashboard
number_of_topics <- 10 

# The names of the three columns that will be used for splitting the data by in the dashboard
col1_label <- "site_url"
col2_label <- "type"
col3_label <- "country"


source("01-tidy-data.R")
source("02-word-counts.R")
source("03-bigrams.R")
source("04-generate-topic-model.R")
source("05-sentiment.R")
# If data is needed in MongoDB
# source("06-save-to-mongodb.R")

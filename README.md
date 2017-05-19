![](screenshot.PNG)

# Getting started

The repository contains 

* Demo text data file
* [R](https://cran.r-project.org/) scripts to process the data
* A [shiny](https://shiny.rstudio.com/) dashboard to visualise the processed data

## Data format
The data file needs to go in the data folder as a csv file with a headings that include "uuid" and "text", which include unique keys and the text data. Look at generic-data.csv for an example and then overwrite this file.

The example data provided was traken from a [Kaggle fake news dataset](https://www.kaggle.com/mrisdal/fake-news).

## Modelling
The first step before running the dashboard is to run 00-build.R. This will extract word counts, sentiment and topic models for use in the dashboard.

Before running, make sure you specify the three strings that refer to categorical columns in the data file. You can also change the number of topics in topic modelling.

## Run the app
Load ui.R or server.R in the shiny-dashboard folder and click run-app in [RStudio](https://www.rstudio.com/)

## Package dependencies
[Packrat](https://rstudio.github.io/packrat/) has been used to manage package dependencies.




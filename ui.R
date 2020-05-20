library(shiny)
library(markdown)
library(RColorBrewer)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)

shinyUI(dashboardPage(
    skin = "green",
    dashboardHeader(title = "Predict Next Word",titleWidth = 300),
    dashboardSidebar(width = 300,
                     textInput("inputString", "Enter words here", value = ""),
                     tags$p(" This is an application that was build for the capstone project of the Coursera Data Science specialization.

The application is intended to take a string of words and predict the next word, based on the probability of occurence.
                            Enter some words avove to receive the next word prediction"),
                     HTML(paste0(
                         "<script>",
                         "var today = new Date();",
                         "var yyyy = today.getFullYear();",
                         "</script>",
                         "<p style = 'text-align: center;'><small>&copy; - <a href='Gaspare Mattarella' target='_blank'>Gaspare Mattarella</a> - <script>document.write(yyyy);</script></small></p>")
                         
                     )
    ),
    dashboardBody(
        h2("Next Word"),
        verbatimTextOutput("prediction"),
        strong("Sentence Input:"),
        tags$style(type='text/css'), 
        textOutput('text1'),
        br(),
        strong(""),
        tags$style(type='text/css'),
        textOutput('text2')
    ) 
    
)

)



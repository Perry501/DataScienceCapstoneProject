
library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("cerulean"),
  
  titlePanel("Data Science Capstone Project"),
  
  navbarPage("Predict the Next Term",
          tabPanel("The App",
                   sidebarLayout(
                           sidebarPanel(
                                   h4("Enter your sentence below. Once entered, press \"Next Word\" button to formulate prediction"),
                                   textInput("input", "Enter your statement here.."),
                                   helpText("Note: This application will use the statement above to help predict the most likely next word."),
                                   submitButton("Next Word")
                                   
                           ),
                           
                           mainPanel(
                                   h2("Your sentence was..."),
                                   textOutput("OriginalStatement"),
                                   br(),
                                   h2("The most likely next word is..."),
                                   textOutput("PredictedNextWord")
                           )
                   )
          ),
          tabPanel("About" ,
                   mainPanel(
                           h3("About"),
                           textOutput("About"),
                           htmlOutput("Link")
                   )
          )    
  )
  
))

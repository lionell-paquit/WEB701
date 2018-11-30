#
# Programmer: Lionell Carlo Paquit 
# Class: WEB701
#
# Simple Twitter Data Analysis in R Programming using Shiny in Web Integration
# 
#    https://nell-paquit.shinyapps.io/WEB701/
#
#
#library(rsconnect)
#rsconnect::deployApp(appDir = getwd(), appName = "web701",
#          appTitle = "Twitter Data Analysis", launch.browser = TRUE)

library(shiny)
library(twitteR)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud2)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("WEB701 Twitter Data Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
       
       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                        tags$div("Loading...",id="loadmessage")),
       
       #side panel
       conditionalPanel(condition="input.tabselected==1",
                        helpText("This web application is a simple twitter data analysis where words are plotted using bar chart and
                                 wordcloud in terms of the frequency of words in a tweet from a given search term."),
                        textInput(inputId = "keyword",
                                  label = "Twitter Keyword:"),
                        actionButton("update", "Get data"),
                        hr(),
                        sliderInput("tws", "Maximum number of tweets to obtain:", 
                                    min=100, max= 500, value=250, step = 1),
                        sliderInput("numWords", "Words in bar graph:", 
                                    min=1, max= 35, value=10, step = 1),
                        sliderInput("max",
                                    "Size of wordcloud:",
                                    min = 5,  max = 100, value = 50, step = 5),
                        actionButton("newCloud", "Generate new wordcloud"))),
  
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Word Plots", plotOutput("freqPlot"), wordcloud2Output("wordPlot"), value = 1,  
                 conditionalPanel(condition="input.tabselected==1")),id = "tabselected")
    )
  )
))

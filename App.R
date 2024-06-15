# Library 
library(shiny)
library(shinydashboard) #
library(maps)  #
library(dplyr)
library(leaflet)  #
library(shinycssloaders)  #
library(shinythemes)
#library(datadigest)  #
library(rio)  #
library(DT)
library(stargazer)
# UI


ui<-
# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "Machine Learning", dropdownMenuOutput("msgOutput")),
  dashboardSidebar(
    sliderInput(
      "Slider1",
      label = h3("Train/Test Split %"),
      min = 0,
      max = 100,
      value = 75
    ),
    textOutput("cntTrain"),
    textOutput("cntTest"),
    br()
    
    #
    # menuItem(
    #   "Generate Report",
    #   tabName = "sectors",
    #   icon = icon("download"),
    #   radioButtons(
    #     'format',
    #     'Document format',
    #     c('HTML', 'Word'),
    #     inline = FALSE,
    #     selected = 1
    #   ),
    #   downloadButton("report", "Download Report", class = "butt"),
    #   tags$head(tags$style(".butt{color: blue !important;}"))
    # )
    
  )
)

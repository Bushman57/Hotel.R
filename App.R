#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(caret)
library(stargazer)
library(corrplot)
#library(datadigest) #Not available
library(rio)
library(DT)
library(stargazer)

  # Define UI for application that draws a histogram
ui <- dashboardPage(
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
      
    ),
    dashboardBody(
      fluidPage(
        box(
          selectInput(
            "SelectX",
            label = "Select variables:",
            choices = names(Final_data),
            multiple = TRUE,
            selected = names(Final_data)
          ),
          solidHeader = TRUE,
          width = "3",
          status = "primary",
          title = "X variable"
        ),
        box(
          selectInput("SelectY", label = "Select variable to predict:", choices = names(Final_data)),
          solidHeader = TRUE,
          width = "3",
          status = "primary",
          title = "Y variable"
        )
        
        
        
      ),
      
      fluidPage(  
        
        tabBox(
          id = "tabset1",
          height = "1000px",
          width = 12,
          
          tabPanel("Data",
                   box(withSpinner(DTOutput(
                     "Data"
                   )), width = 12)),
          tabPanel(
            "Data Summary",
            box(withSpinner(verbatimTextOutput("Summ")), width = 6),
            box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
          ),
          
          # 
          # tabPanel("Data Strucure",
          #          # box(
          #          #   withSpinner(verbatimTextOutput("structure")), width = "100%"
          #          # ),
          #          explorerOutput("digest")
          #          ),
          tabPanel("Plots",
                   box(withSpinner(plotOutput(
                     "Corr"
                   )), width = 12)),
          #box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12),
          tabPanel(
            "Model",
            box(
              withSpinner(verbatimTextOutput("Model")),
              width = 6,
              title = "Model Summary"
            ),
            # box(
            #   withSpinner(verbatimTextOutput("Model_new")),
            #   width = 6,
            #   title = "Model Summary"
            # ),
            # 
            box(
              withSpinner(verbatimTextOutput("ImpVar")),
              width = 5,
              title = "Variable Importance"
            )
          ),
          #textOutput("correlation_accuracy"),
          tabPanel(
            "Prediction",
            box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
            box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
          )
        )
      )
    )
)

dd<-Final_data

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  InputDataset <- reactive({
    Final_data
  })
  
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- Final_data
    }
    else{
      dt <- Final_data[, c(input$SelectX)]
    }
    
  })
  
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  output$Data <- renderDT(InputDataset())
  
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  
  #Code section for Generalized Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
  
  Linear_Model <- reactive({
    glm(f(),family = Gamma() ,data = trainingData())
  })
  
  output$Model <- renderPrint(summary.glm(Linear_Model()))
  output$Model_new <-
    renderPrint(
    pander::pander(summary.glm(Linear_Model()))
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict.glm(Linear_Model(),type = 'response',newdata =  testData())
  })
  
  tmp <- reactive({
    tmp1 <- testData()
    tmp1[, c(input$SelectY)]
    #tmp1[,c('Amount')]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  Fit <-
    reactive({
      (
        plot(
          actuals_preds()$actuals,
          actuals_preds()$predicted,
          pch = 16,
          cex = 1.3,
          col = "blue",
          main = "Best Fit Line",
          xlab = "Actual",
          ylab = "Predicted"
        )
      #  ggplot(data = actuals_preds(),aes(actuals_preds()$actuals,actuals_pred()$predicted))+
      #    geom_point(col = 'blue')+
      #    geom_smooth(method = 'lm',formula = 'y~x',se =FALSE, col ='red')
      )
    })
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(Linear_Model())
    par(mfrow = c(1, 1)) # Change back to 1 x 1
    
  })
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

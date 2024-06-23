library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(htmltools)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")  # Include Bootstrap CSS
  ),
  titlePanel("Receipt Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("customer_name", "Customer Name"),
      textInput("item_name", "Item Name"),
      numericInput("quantity", "Quantity", 1, min = 1),
      numericInput("price", "Price per Item", 0, min = 0),
      actionButton("add", "Add Item"),
      actionButton("generate", "Generate Receipt"),
      actionButton("print", "Print Receipt")
    ),
    mainPanel(
      DTOutput("table"),
      uiOutput("receipt")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  items <- reactiveVal(data.frame(Item = character(), Quantity = numeric(), Price = numeric(), stringsAsFactors = FALSE))
  
  observeEvent(input$add, {
    new_item <- data.frame(Item = input$item_name, Quantity = input$quantity, Price = input$price, stringsAsFactors = FALSE)
    items(rbind(items(), new_item))
  })
  
  output$table <- renderDT({
    datatable(items(), options = list(pageLength = 5))
  })
  
  output$receipt <- renderUI({
    req(input$generate)
    
    receipt_items <- items()
    total <- sum(receipt_items$Quantity * receipt_items$Price)
    
    receipt_html <- tagList(
      div(
        id = "receipt-content",  # Add an ID to the receipt content for printing
        h2("Receipt"),
        p(paste("Customer Name:", input$customer_name)),
        tags$table(
          class = "table table-striped",  # Bootstrap table class
          style = "width: 100%;",
          tags$thead(
            tags$tr(
              tags$th("Item"),
              tags$th("Quantity"),
              tags$th("Price"),
              tags$th("Total")
            )
          ),
          tags$tbody(
            lapply(1:nrow(receipt_items), function(i) {
              tags$tr(
                tags$td(receipt_items$Item[i]),
                tags$td(receipt_items$Quantity[i]),
                tags$td(sprintf("$%.2f", receipt_items$Price[i])),
                tags$td(sprintf("$%.2f", receipt_items$Quantity[i] * receipt_items$Price[i]))
              )
            })
          ),
          tags$tfoot(
            tags$tr(
              tags$td(colspan = 3, "Total"),
              tags$td(sprintf("$%.2f", total))
            )
          )
        ),
        tags$button(
          "Print Receipt", 
          id = "print-receipt", 
          class = "btn btn-primary", 
          onclick = "printReceipt()"
        )
      ),
      tags$script(
        "function printReceipt() {
          var content = document.getElementById('receipt-content').innerHTML;
          var printWindow = window.open('', '_blank');
          printWindow.document.open();
          printWindow.document.write('<html><head><title>Print Receipt</title></head><body>');
          printWindow.document.write(content);
          printWindow.document.write('</body></html>');
          printWindow.document.close();
          printWindow.print();
          printWindow.close();
        }"
      )
    )
    
    HTML(as.character(receipt_html))
  })
  
  observeEvent(input$print, {
    # Nothing needed here since we are using a JavaScript function for printing
  })
}

# Run the app
shinyApp(ui, server)

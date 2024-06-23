library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets4)
library(DT)

# Authorize app
gs4_auth(email = "savinskamau01@gmail.com", cache = ".secrets")

# Read the Google Sheets document
sheet_id <- '1_PfLub_3Ei2S6tCxkN5IU53FvP5jb-rlYfXBBYfenek'
sales_data <- read_sheet(sheet_id, sheet = 1)
initial_stock <- read_sheet(sheet_id, sheet = 2)

# Preprocess sales data
sales_data$Date <- as.Date(sales_data$Date, format = "%Y-%m-%d")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Footwear Management System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Add Sales", tabName = "add_sales", icon = icon("shopping-cart")),
      menuItem("Update Stock", tabName = "update_stock", icon = icon("warehouse")),
      menuItem("Stock Value", tabName = "stock_value", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          position: fixed;
          width: 230px;
        }
        .main-header .navbar {
          margin-left: 230px;
        }
        .main-sidebar {
          position: fixed;
          width: 230px;
          height: 100%;
          overflow: auto;
        }
        .content-wrapper, .main-footer {
          margin-left: 230px;
        }
        .tab-content {
          position: relative; /* Ensure relative positioning for proper footer placement */
          padding-bottom: 60px; /* Adjust as necessary */
        }
        .sidebar-footer {
          position: relative;
          bottom: 0;
          width: 100%;
          text-align: center;
          padding: 10px;
          background-color: #222d32;
          color: white;
        }
        .sidebar-footer img {
          display: block;
          margin: 0 auto;
        }
        .sidebar-footer h4, .sidebar-footer a {
          color: white;
          text-align: center;
          display: block;
        }
        @media (max-width: 768px) {
          .main-header .logo, .main-sidebar {
            width: 100%;
            position: relative;
          }
          .main-header .navbar, .content-wrapper, .main-footer {
            margin-left: 0;
          }
          .main-sidebar {
            display: none;
            position: fixed;
            width: 230px;
            height: 100%;
            z-index: 1000;
            background-color: #222d32;
          }
          .main-sidebar.shown {
            display: block;
          }
          .sidebar-toggle {
            position: fixed;
            left: 15px;
            top: 15px;
            z-index: 1001;
          }
          .sidebar-footer {
            position: relative;
            width: 100%;
          }
        }
        .scrollable-table {
          overflow-x: auto;
        }
      ")),
      tags$script(HTML("
        $(document).on('click', '.sidebar-toggle', function() {
          $('.main-sidebar').toggleClass('shown');
        });
        $(document).on('click', '.sidebar-menu a', function() {
          if ($(window).width() <= 768) {
            $('.main-sidebar').addClass('hidden');
          }
        });
        
        $(document).on('click', '.sidebar-menu a', function() {
          setTimeout(function() {
            $('.main-sidebar').removeClass('hidden');
          }, 200);
        });

      "))
    ),
    tabItems(
      tabItem(tabName = "add_sales",
              fluidRow(
                box(title = "Enter Sales Data", status = "primary", solidHeader = TRUE, width = 12,
                    dateInput("sales_date", "Date of Sale:", value = Sys.Date(), max = Sys.Date(), min = Sys.Date() - 5),
                    numericInput("crocks_small", "Crocks Small:", value = 0, min = 0),
                    numericInput("crocks_large", "Crocks Large:", value = 0, min = 0),
                    numericInput("rubbers", "Rubbers:", value = 0, min = 0),
                    numericInput("gumboots", "Gumboots:", value = 0, min = 0),
                    numericInput("slides", "Slides:", value = 0, min = 0),
                    numericInput("slipers_large", "Slipers Large:", value = 0, min = 0),
                    numericInput("slipers_small", "Slipers Small:", value = 0, min = 0),
                    actionButton("add_sales", "Add Sales", class = "btn-primary"),
                    div(class = "sidebar-footer",
                        tags$img(src = "https://d18lev1ok5leia.cloudfront.net/chesapeakebay/field-guide/bald-eagle/_700x600_fit_center-center_none/BaldEagleFieldGuide_1800-03-20170420-IMG_2639.jpg", height = "40px", width = "100px"),
                        tags$h4("TaiStat Consultancy Firm"),
                        tags$a(href = "mailto:dataconsultancy@taistat.com", "dataconsultancy@taistat.com")
                    )
                )
              )
      ),
      tabItem(tabName = "update_stock",
              fluidRow(
                box(title = "Update Stock Levels", status = "primary", solidHeader = TRUE, width = 12,
                    pickerInput(
                      inputId = "footwear",
                      label = "Footwear Type:",
                      choices = NULL,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        create = TRUE # Allows creating new options
                      )
                    ),
                    numericInput("buying_price", "Buying Price per Unit:", value = 0, min = 0),
                    numericInput("selling_price", "Selling Price per Unit:", value = 0, min = 0),
                    numericInput("new_stock", "Number of New Stock:", value = 0, min = 0),
                    actionButton("update_stock", "Update Stock", class = "btn-primary"),
                    div(class = "sidebar-footer",
                        tags$img(src = "https://d18lev1ok5leia.cloudfront.net/chesapeakebay/field-guide/bald-eagle/_700x600_fit_center-center_none/BaldEagleFieldGuide_1800-03-20170420-IMG_2639.jpg", height = "40px", width = "100px"),
                        tags$h4("TaiStat Consultancy Firm"),
                        tags$a(href = "mailto:dataconsultancy@taistat.com", "dataconsultancy@taistat.com")
                    )
                )
              )
      ),
      tabItem(tabName = "stock_value",
              fluidRow(
                box(title = "Total Stock Value", status = "primary", solidHeader = TRUE, width = 12,
                    textOutput("total_stock_value")
                ),
                box(title = "Stock Information", status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "scrollable-table", dataTableOutput("stock_info"))
                ),
                box(title = "Sales Data", status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "scrollable-table", dataTableOutput("sales_table")),
                    div(class = "sidebar-footer",
                        tags$img(src = "https://d18lev1ok5leia.cloudfront.net/chesapeakebay/field-guide/bald-eagle/_700x600_fit_center-center_none/BaldEagleFieldGuide_1800-03-20170420-IMG_2639.jpg", height = "40px", width = "100px"),
                        tags$h4("TaiStat Consultancy Firm"),
                        tags$a(href = "mailto:dataconsultancy@taistat.com", "dataconsultancy@taistat.com")
                    )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive sales data
  reactive_sales_data <- reactiveVal(sales_data)
  
  # Reactive stock data
  reactive_stock_data <- reactiveVal(initial_stock)
  
  # Update the dropdown choices for footwear types
  observe({
    updatePickerInput(session, "footwear", choices = unique(initial_stock$FootWear))
  })
  
  # Add new sales
  observeEvent(input$add_sales, {
    sales_data <- reactive_sales_data()
    existing_row_index <- which(sales_data$Date == as.Date(input$sales_date))
    
    new_sales <- data.frame(
      Date = as.Date(input$sales_date),  # Ensure Date is converted to Date type
      Crocks_Small = input$crocks_small,
      Crocks_Large = input$crocks_large,
      Rubbers = input$rubbers,
      Gumboots = input$gumboots,
      Slides = input$slides,
      Slipers_Large = input$slipers_large,
      Slipers_Small = input$slipers_small,
      Total = NA,
      Profit = NA
    )
    
    stock <- reactive_stock_data()
    new_sales$Total <- input$crocks_small * stock$Selling_Price[stock$FootWear == "Crocks_Small"] +
      input$crocks_large * stock$Selling_Price[stock$FootWear == "Crocks_Large"] +
      input$rubbers * stock$Selling_Price[stock$FootWear == "Rubbers"] +
      input$gumboots * stock$Selling_Price[stock$FootWear == "Gumboots"] +
      input$slides * stock$Selling_Price[stock$FootWear == "Slides"] +
      input$slipers_large * stock$Selling_Price[stock$FootWear == "Slipers_Large"] +
      input$slipers_small * stock$Selling_Price[stock$FootWear == "Slipers_Small"]
    
    new_sales$Profit <- input$crocks_small * stock$Profit[stock$FootWear == "Crocks_Small"] +
      input$crocks_large * stock$Profit[stock$FootWear == "Crocks_Large"] +
      input$rubbers * stock$Profit[stock$FootWear == "Rubbers"] +
      input$gumboots * stock$Profit[stock$FootWear == "Gumboots"] +
      input$slides * stock$Profit[stock$FootWear == "Slides"] +
      input$slipers_large * stock$Profit[stock$FootWear == "Slipers_Large"] +
      input$slipers_small * stock$Profit[stock$FootWear == "Slipers_Small"]
    
    if (length(existing_row_index) > 0) {
      sales_data[existing_row_index, ] <- new_sales
    } else {
      sales_data <- rbind(sales_data, new_sales)
    }
    
    # Update reactive sales data
    reactive_sales_data(sales_data)
    
    # Write updated sales data to Google Sheet
    write_sheet(sales_data, sheet_id, sheet = 1)
    
    # Clear input fields
    updateNumericInput(session, "crocks_small", value = 0)
    updateNumericInput(session, "crocks_large", value = 0)
    updateNumericInput(session, "rubbers", value = 0)
    updateNumericInput(session, "gumboots", value = 0)
    updateNumericInput(session, "slides", value = 0)
    updateNumericInput(session, "slipers_large", value = 0)
    updateNumericInput(session, "slipers_small", value = 0)
    
    showNotification("Sales updated successfully", type = "message")
  })
  
  # Display sales data
  output$sales_table <- renderDataTable({
    reactive_sales_data()
  })
  
  # Update stock levels
  observeEvent(input$update_stock, {
    stock_data <- reactive_stock_data()
    row_index <- which(stock_data$FootWear == input$footwear)
    
    if (length(row_index) > 0) {
      stock_data$No_of_Stock[row_index] <- stock_data$No_of_Stock[row_index] + input$new_stock
      stock_data$Buying_Price[row_index] <- input$buying_price
      stock_data$Selling_Price[row_index] <- input$selling_price
      stock_data$Profit[row_index] <- input$selling_price - input$buying_price # Calculate profit
    } else {
      new_row <- data.frame(
        FootWear = input$footwear,
        No_of_Stock = input$new_stock,
        Buying_Price = input$buying_price,
        Selling_Price = input$selling_price,
        Profit = input$selling_price - input$buying_price # Calculate profit
      )
      stock_data <- rbind(stock_data, new_row)
    }
    
    # Update reactive stock data
    reactive_stock_data(stock_data)
    
    # Write updated data to Google Sheet
    write_sheet(stock_data, sheet_id, sheet = 2)
    
    # Clear input fields
    updateNumericInput(session, "buying_price", value = 0)
    updateNumericInput(session, "selling_price", value = 0)
    updateNumericInput(session, "new_stock", value = 0)
    
    showNotification("Stock updated successfully", type = "message")
  })
  
  # Calculate and display total stock value
  output$total_stock_value <- renderText({
    df <- reactive_stock_data()
    total_value <- sum(df$No_of_Stock * df$Selling_Price)
    formatted_value <- format(total_value, big.mark = ",", scientific = FALSE)
    paste("Kshs", formatted_value)
  })
  
  # Display stock information
  output$stock_info <- renderDataTable({
    reactive_stock_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

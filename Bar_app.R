library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets4)
library(DT)
library(data.table)
<<<<<<< HEAD
library(shinyjs)
library(htmltools)
library(dipsaus) 
library(shinymanager)
=======
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38

# Authorize app
gs4_auth(email = "savinskamau01@gmail.com", cache = ".secrets")

<<<<<<< HEAD

# Define credentials for shinymanager
credentials <- data.frame(
  user = c("user1", "user2"),
  password = c("password1", "password2"),
  stringsAsFactors = FALSE
)


=======
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38
# Read the Google sheet document

sheet_id <-'14WCJXPrTqObRA-purHkqnGc78duSSLOwQAGWyjmDIzs'
sales_data<-read_sheet(sheet_id,sheet = 'Sales')
stock_data<-read_sheet(sheet_id,sheet = 1)
<<<<<<< HEAD
sales_receipt<-read_sheet(sheet_id,sheet = 'Sales_Receipt')
=======
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38

# Preprocess sales data

#sales_data$Date <- as.Date(sales_data$Date, format = "%m/%d/%Y")

# Define UI

ui<-dashboardPage(
  dashboardHeader(title = "Bar Management System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Add Sales", tabName = "add_sales", icon = icon("shopping-cart")),
      menuItem("Update Stock", tabName = "update_stock", icon = icon("warehouse")),
<<<<<<< HEAD
      menuItem("Stock Value", tabName = "stock_value", icon = icon("dollar-sign")),
      menuItem("Receipt", tabName = "receipt_value", icon = icon("robot"))
=======
      menuItem("Stock Value", tabName = "stock_value", icon = icon("dollar-sign"))
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38
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
                box(title = 'Enter Sales Data', status = 'primary',solidHeader = TRUE, width = 12,
                    dateInput('sales_date', 'Date of Sale:',value = Sys.Date()),
                    numericInput('Ts_b', 'Tusker & Balozi:', value = 0,min = 0 ),
                    numericInput('Wc_G_C','White Cap, Guiness & Cider:', value = 0,min = 0),
                    numericInput('Cans','Cans:', value = 0,min = 0),
                    numericInput('Gur','Guarana:', value = 0,min = 0),
                    numericInput('Rb','RedBull:', value = 0,min = 0),
                    numericInput('Fe','Faxe:', value = 0,min = 0),
                    numericInput('SM','Smirnoff 0.25:', value = 0,min = 0),
                    numericInput('SM_5','Smirnoff 0.5:', value = 0,min = 0),
                    numericInput('SM_75','Smirnoff 0.75:', value = 0,min = 0),
                    numericInput('Rt','Richot 0.25:', value = 0,min = 0),
                    numericInput('Rt_5','Richot 0.5', value = 0,min = 0),
                    numericInput('Rt_75','Richot 0.75:', value = 0,min = 0),
                    numericInput('Vc','ViceRoy 0.25:', value = 0,min = 0),
                    numericInput('Vc_5','ViceRoy 0.5:', value = 0,min = 0),
                    numericInput('Vc_75','ViceRoy 0.75:', value = 0,min = 0),
                    numericInput('Gs','Gilbeys 0.25:', value = 0,min = 0),
                    numericInput('Gs_5','Gilbeys 0.5:', value = 0,min = 0),
                    numericInput('Gs_75','Gilbeys 0.75:', value = 0,min = 0),
                    numericInput('Kc','Kenya Cane 0.25:', value = 0,min = 0),
                    numericInput('Kc_5','Kenya Cane 0.5:', value = 0,min = 0),
                    numericInput('Kc_75','Kenya Cane 0.75:', value = 0,min = 0),
                    numericInput('Ch','Chrome 0.25:', value = 0,min = 0),
                    numericInput('Ch_75','Chrome 0.75:', value = 0,min = 0),
                    numericInput('Ke','Kane Extra / White Pearl & Triple Ace:', value = 0,min = 0),
                    numericInput('Ke_75','Kane Extra 0.75:', value = 0,min = 0),
                    numericInput('Vat','Vat69 0.25:', value = 0,min = 0),
                    numericInput('Vat_75','Vat69 0.75:', value = 0,min = 0),
                    numericInput('All','All Seasons 0.25:', value = 0,min = 0),
                    numericInput('All_35','All Seasons 0.35:', value = 0,min = 0),
                    numericInput('All_75','All Seasons 0.75:', value = 0,min = 0),
                    numericInput('Kon','Konyagi 0.25:', value = 0,min = 0),
                    numericInput('Kon_35','Konyagi 0.35:', value = 0,min = 0),
                    numericInput('Kon_75','Konyagi 0.75:', value = 0,min = 0),
                    numericInput('Tusker_L','Tusker Malt/Larger', value = 0,min = 0),
                    numericInput('Hun','Hunters .25:', value = 0,min = 0),
                    numericInput('Hun_75','Hunters .75:', value = 0,min = 0),
                    numericInput('Napoleon','Napoleon .25:', value = 0,min = 0),
                    numericInput('Wh','Best Whisky/Cream:', value = 0,min = 0),
                    numericInput('VnA','V&A Caribia Gin & Best Gin:', value = 0,min = 0),
                    numericInput('Soda_1','Soda 1ltr:', value = 0,min = 0),
                    numericInput('Cy','County 0.25:', value = 0,min = 0),
                    numericInput('Cy_75','County 0.75:', value = 0,min = 0),
                    numericInput('Red','Red Label 0.25:', value = 0,min = 0),
                    numericInput('Red_35','Red Label 0.35:', value = 0,min = 0),
                    numericInput('Red_75','Red Label 0.75:', value = 0,min = 0),
                    numericInput('Ko','Kibao 0.25:', value = 0,min = 0),
                    numericInput('Ko_35','Kibao 0.35:', value = 0,min = 0),
                    numericInput('Ko_75','Kibao 0.75:', value = 0,min = 0),
                    numericInput('Bw','Black & White 0.25:', value = 0,min = 0),
                    numericInput('Bw_35','Black & White 0.35:', value = 0,min = 0),
                    numericInput('Bw_75','Black & White 0.75:', value = 0,min = 0),
                    numericInput('Gm','Grants Mzinga:', value = 0,min = 0),
                    numericInput('By','Baileys 0.75:', value = 0,min = 0),
                    numericInput('Kk','Kenya King 0.25:', value = 0,min = 0),
                    numericInput('Delmonte','Delmonte:', value = 0,min = 0),
                    numericInput('Cap','Caprice:', value = 0,min = 0),
                    numericInput('Alvaro','Alvaro', value = 0,min = 0),
                    numericInput('Trust','Trust:', value = 0,min = 0),
                    numericInput('Soda','Soda 300ml:', value = 0,min = 0),
                    numericInput('Plastic','Plastic Soda/Soda 500ml:', value = 0,min = 0),
                    numericInput('Dasani','Dasani 0.5ml:', value = 0,min = 0),
                    numericInput('Dasani_1','Dasani 1ltr:', value = 0,min = 0),
                    numericInput('H2o','H2O 1ltr:', value = 0,min = 0),
                    numericInput('Pr','Predator:', value = 0,min = 0),
                    numericInput('AZ','Azam:', value = 0,min = 0),
                    numericInput('Fc','Four Cousins & CC:', value = 0,min = 0),
                    numericInput('Choice','1st Choice:', value = 0,min = 0),
                    numericInput('Cm','Captain Morgan 0.25:', value = 0,min = 0),
                    numericInput('Cm_75','Captain Morgan 0.75:', value = 0,min = 0),
                    numericInput('Best','Best', value = 0,min = 0),
                    numericInput('Mm','Minute Maid', value = 0,min = 0),
                    actionButton("add_sales", "Add Sales", class = "btn-primary"),
                    div(class = "sidebar-footer",
                        tags$img(src = "https://cdn3.vectorstock.com/i/1000x1000/67/12/beer-on-bright-yello-vector-40396712.jpg", height = "100px", width = "100px"),
                        tags$h4("Merry Mary Restaurant"),
                        tags$a(href = "mailto:savinskamau01.com", "bushmansavins@gmail.com")
                    )
                    )
              )),
      tabItem(tabName = 'update_stock',
              fluidRow(
                box(title = "Update Stock Levels", status = "primary", solidHeader = TRUE, width = 12,
                    pickerInput(
                      inputId = "BarSales",
                      label = "Drink Type:",
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
                        tags$img(src = "https://cdn3.vectorstock.com/i/1000x1000/67/12/beer-on-bright-yello-vector-40396712.jpg", height = "100px", width = "100px"),
                        tags$h4("Merry Mary Restaurant"),
                        tags$a(href = "mailto:savinskamau01.com", "bushmansavins@gmail.com")
                    )
                )
              )),
      tabItem(tabName = 'stock_value',
              fluidRow(
                box(title = "Total Stock Value", status = "primary", solidHeader = TRUE, width = 12,
                    textOutput("total_stock_value")
                ),
                box(title = "Stock Information", status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "scrollable-table", dataTableOutput("stock_info"))
                ),
                box(title = "Sales Data", status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "scrollable-table",DT:: dataTableOutput("sales_table")),
                    div(class = "sidebar-footer",
                        tags$img(src = "https://cdn3.vectorstock.com/i/1000x1000/67/12/beer-on-bright-yello-vector-40396712.jpg", height = "100px", width = "100px"),
                        tags$h4("Merry Mary Restaurant"),
                        tags$a(href = "mailto:savinskamau01.com", "bushmansavins@gmail.com")
                    )
                )
<<<<<<< HEAD
              )),
      tabItem(tabName = 'receipt_value',
              fluidRow(
                box(title = "Receipt Generator",
                    textInput("Waiters_Name", "Waiter's Name:"),
                    pickerInput(
                      inputId = "BarSales2",
                      label = "Drink Sold:",
                      choices = NULL,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        create = TRUE # Allows creating new options
                      )
                    ),
                    numericInput("quantity", "Quantity", 1, min = 1),
                    numericInput("price", "Price per Item", 0, min = 0),
                    actionButton("add", "Add item"),
                    actionButton("generate", "Generate Receipt"),
                    actionButton("Remove", "Me 🤡"),
                    #actionButtonStyled("Remove,","Me 🤡",
                    #                   btyn_type ="Button",type = "info"),
                    DTOutput("table"),
                    #DTOutput("table_2")
                    
                    #actionButtonStyled("submitbutton","Submit",
                    #btn_type = "button", type = "primary"),
                    ),
                box(title =h2('Receipt'),
                    uiOutput("receipt"),width = 6
                    
                    )
=======
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38
              ))
    )
  )
)


server <- function(input, output, session) {
<<<<<<< HEAD
  
  # Wrap server with shinymanager
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
=======
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38
  # Reactive sales data
  reactive_sales_data <- reactive({
    sales_data
  })
  # Reactive stock data
  reactive_stock_data <- reactive({
    stock_data
  })
<<<<<<< HEAD
  # Reactive sales_receipt data
  reactive_receipt_data<-reactive({
    sales_receipt
  })
  
  # Update the drop down choices for drink types
=======
  # Update the dropdown choices for footwear types
>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38
  observe({
    updatePickerInput(session, "BarSales", choices = unique(stock_data$Drinks))
  })
  
  # Add new sales
  observeEvent(input$add_sales,{
    new_sales<-data.table(
      Date<-as.Date(input$sales_date,format = "%m/%d/%Y"),
      TUSKER<-input$Ts_b,
      WHITE<-input$Wc_G_C, 
      CANS<-input$Cans,
      GUARANA<-input$Gur,
      REDBULL<-input$Rb,
      FAXE<-input$Fe,
      SMIRNOFF <-input$SM,
      SMIRNOFF_1<-input$SM_5 ,
      SMIRNOFF_2<-input$SM_75, 
      RICHOT <-input$Rt,
      RICHOT_1 <-input$Rt_5,
      RICHOT_2 <-input$Rt_75,
      VICEROY <-input$Vc,
      VICEROY_1 <-input$Vc_5,
      VICEROY_2 <-input$Vc_75,
      GILBEYS <-input$Gs,
      GILBEYS_1 <-input$Gs_5,
      GILBEYS_2 <-input$Gs_75,
      KENYACANE <-input$Kc,
      KENYACANE_1 <-input$Kc_5,
      KENYACANE_2 <-input$Kc_75,
      CHROME <-input$Ch,
      CHROME_2 <-input$Ch_75,
      KANE<-input$Ke,
      KANE_2 <-input$Ke_75,
      VAT <-input$Vat,
      VAT_2 <-input$Vat_75,
      ALL <-input$All,
      ALL_2<-input$All_35,
      ALL_3 <-input$All_75,
      KONYAGI <-input$Kon,
      KONYAGI_2 <-input$Kon_35,
      KONYAGI_3 <-input$Kon_75,
      TUSKER <-input$Tusker_L,
      HUNTERS <-input$Hun,
      HUNTERS_2 <-input$Hun_75,
      NAPOLEON <-input$Napoleon,
      BEST <-input$Wh,
      VnA <-input$VnA,
      SODA <-input$Soda_1,
      COUNTY <-input$Cy,
      COUNTY_2 <-input$Cy_75,
      RED <-input$Red,
      RED_2 <-input$Red_35,
      RED_3 <-input$Red_75,
      KIBAO <-input$Ko,
      KIBAO_2 <-input$Ko_35,
      KIBAO_3 <-input$Ko_75,
      BLACK <-input$Bw,
      BLACK_2 <-input$Bw_35,
      BLACK_3 <-input$Bw_75,
      GRANTS <-input$Gm,
      BAILEYS <-input$By,
      KENYA <-input$Kk,
      DELMONTE<-input$Delmonte,
      CAPRICE<-input$Cap,
      ALVARO<-input$Alvaro,
      TRUST<-input$Trust,
      SODA <-input$Soda,
      PLASTIC <-input$Plastic,
      DASANI <-input$Dasani,
      DASANI_1 <-input$Dasani_1,
      H20 <-input$H2o,
      PREDATOR<-input$Pr,
      AZAM<-input$AZ,
      FOUR <-input$Fc,
      CHOICE<-input$Choice,
      C_MORGAN <-input$Cm,
      C.MORGAN <-input$Cm_75,
      BEST <-input$Best,
      MINUTE <-input$Mm
    )
    
    #Put the data on the drive
    googlesheets4::sheet_append(ss=sheet_id,
                                data = new_sales,
                                sheet = "Sales")
    # Display sales data
    output$sales_table <- renderDataTable({
      reactive_sales_data()
    })
    
    showNotification("Sales added successfully", type = "message")
  })
  # Update stock levels
  observeEvent(input$update_stock, {
    new_data <- reactive_stock_data()
    row_index <- which(new_data$Drinks == input$BarSales)
    
    if (length(row_index) > 0) {
      new_data$Stock_update[row_index] <- new_data$Stock_update[row_index] + input$new_stock
      new_data$Buying_Price[row_index] <- input$buying_price
      new_data$Selling_Price[row_index] <- input$selling_price
    } else {
      new_row <- data.frame(
        Drinks = input$footwear,
        Stock_update = input$new_stock,
        Buying_Price = input$buying_price,
        Selling_Price = input$selling_price,
        Profit = input$selling_price - input$buying_price
      )
      new_data <- rbind(new_data, new_row)
    }
    
    # Write updated data to Google Sheet
    write_sheet(new_data, sheet_id, sheet = 1)
    
    showNotification("Stock updated successfully", type = "message")
  })
<<<<<<< HEAD
  # Receipt generator
  
  # Update the drop down choices for Bar drink types
  observe({
    updatePickerInput(session, "BarSales2", choices = unique(stock_data$Drinks))
  })
  
  items <- reactiveVal(data.frame(Item = character(), Quantity = numeric(), Price = numeric(), stringsAsFactors = FALSE))
  
  
  # Add a new row
  observeEvent(input$add, {   #
    new_item <- data.frame(Item = input$BarSales2, Quantity = input$quantity, Price = input$price, stringsAsFactors = FALSE)
    items(rbind(items(), new_item))
    
    #new_item <- data.frame(Item = input$BarSales2, Quantity = input$quantity, Price = input$price, stringsAsFactors = FALSE)
    #items()
    
  })
  
  # Remove the last row from the data frame
  observeEvent(input$Remove, {
    new_data <- items()
        if (nrow(new_data) > 0) {
      new_data<-new_data[-nrow(new_data), ]
      items(new_data)
    }
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
        tags$img(src = "https://cdn3.vectorstock.com/i/1000x1000/67/12/beer-on-bright-yello-vector-40396712.jpg", height = "100px", width = "100px"
                 , style = "position: absolute; top: 0; right: 0;"),
        #h2("Receipt"),
        p(paste("Waiter's Name:", input$Waiters_Name)),
        p(tags$a(href = "mailto:savinskamau01.com", "bushmansavins@gmail.com")),
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
          "Done", 
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
          printWindow.document.write(('Merry Mary Bar'));
          printWindow.document.write(content);
          printWindow.document.write('Your satisfaction matters');
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
  
  
  
  
  #receipt_items <- items()
  #total <- sum(receipt_items$Quantity * receipt_items$Price)
  #date_time <- Sys.time()
  #
  #receipt_data <- data.frame(
  #  Date_Time = date_time,
  #  Waiter_Name = input$cWaiters_name,
  #  Item = receipt_items$Item,
  #  Quantity = receipt_items$Quantity,
  #  Price = receipt_items$Price,
  #  Total = receipt_items$Quantity * receipt_items$Price,
  #  stringsAsFactors = FALSE
  #)
  
  
  observeEvent(input$generate,{
  # Write receipt data to Google Sheet
    receipt_items<-items()
    date_time<-Sys.time()
    
      
    receipt_data<-data.frame(
      Date_Time=date_time,
      Waiter_Name=input$Waiters_Name,
      Item = receipt_items$Item,
      Quantity = receipt_items$Quantity,
      Price = receipt_items$Price,
      Total = receipt_items$Quantity * receipt_items$Price,
      stringsAsFactors = FALSE
    )
    
    
    sheet_append(ss=sheet_id,data = receipt_data ,sheet ="Sales_Receipt" )
    showNotification("Sales added successfully", type = "message")
  })
  

}

# Wrap UI with shinymanager
ui <- secure_app(ui)

=======
  
}

>>>>>>> d7c1272e2ec55919e3f5772b628ea30cd9b78e38
shinyApp(ui, server)






















#1. Set up: Install libraries and set working drive ----

#install libraries for Shiny
library("shiny")
#library("shinydashboard")
library("shinyWidgets")
library("shinyalert")
#library("pixiedust")
#install libraries for app
library("qrcode")
#suppressPackageStartupMessages(library("gmailr"))

#install libraries for database update
library("RMariaDB")
library("DBI")
#library("dplyr")
library("DT")
library("xtable")

source('~/OrderApp/venueinfo.R')
priceList <- read.csv(paste0("price_list", ".csv", sep = ""), header = T)

if(is.na(match("service_charge", priceList[,1])) == F) {
  serviceChargePc <- priceList[match("service_charge", priceList[,1]),2]
  priceList <- priceList[-match("service_charge", priceList[,1]),]
} else {
  serviceChargePc <- 0
}
foodSections <- as.character(unique(priceList$Section[priceList$Section != "Drink" & priceList$Section != "Option"]))


#define function to allow flexible sections in food menu display
tabelize <- function(variables, price_list) {
  
  tables <- list() # create a list to hold all tables
  
  for (variable in variables) { # go through all possible values of variables
    table <- price_list[price_list$Section == variable, c(1,4,2)]
    tables[[as.character(variable)]] <- 
      # save table into slot in created list
      # print table as HTML with additional formatting options
      #
      print(xtable(table, caption=paste0("<h2 style='color:Black;'>", variable)),
            type="html",
            html.table.attributes='class="data table table-condensed"',
            caption.placement="top",
            include.rownames = FALSE,
            include.colnames = FALSE)

    
  }
  return(as.character(lapply(tables, paste))) # return HTML tables pasted together
}

#2. Shiny Server function to record information for a single order ----

shinyServer <- function(input, output, session) {
  
  #2.a. set options and initiate data frames to write order info and person info ----
  
  #set working drive to be working drive for app
  #setwd("/home/rstudio/ShinyApps/Order")
  
  hideTab(inputId = "inTabset", target = "panel0")
  hideTab(inputId = "inTabset", target = "panel1")
  hideTab(inputId = "inTabset", target = "panel2")
  hideTab(inputId = "inTabset", target = "panel4")
  
  options(shiny.maxRequestSize=60*1024^2)
  values <- reactiveValues()
  values$Db<- list()
  values$Rec<- data.frame(
    OrderName = "", OrderEmail = "", OrderTimeIn = "", OrderIntPhone = 0, OrderPhone = 0, OrderNumber = 0, OrderQrRef = "", OrderTimeOut = "", stringsAsFactors = FALSE
  )
  values$df <- data.frame(
    row_names = "", Item = "", Number = 0, Price = 0, Pub = "", TableNumber = 0, OrderNumber = 0, OrderQrRef = "", OrderStatus = "", 
    stringsAsFactors = FALSE
  )
  
  #2.b. Select Pub and Table Number.  Create Menu table to display ----
  
  #pubList <- as.list(read.csv(file = "pubList.csv", header = T))
  
  #output$SelectedTestCentre <- renderUI({selectInput(inputId = 'TestCentre',
  #                                                   label = 'Venue',
  #                                                   choices = pubList)})
  values$TestCentre <- venue
  
  output$TableNumber <- renderUI({selectInput(inputId = 'TableNumber',
                                              label = 'Table Number',
                                              choices = c(1:200))})
  
  observeEvent(input$confirmPubTable, {
    
    hideTab(inputId = "inTabset", target = "panel3")
    showTab(inputId = "inTabset", target = "panel0")
    showTab(inputId = "inTabset", target = "panel1")
    showTab(inputId = "inTabset", target = "panel4")
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
    
    #priceList <- read.csv(paste0("price_list_", values$TestCentre, ".csv", sep = ""), header = T)
    #foodSections <- as.character(unique(priceList$Section[priceList$Section != "Drink"]))
    
    display_drink <- priceList[priceList$Section == "Drink",c(1,4,2)]
    output$DrinkMenu <- renderTable({display_drink}, colnames = FALSE)

    output$foodTables <- renderUI({
      out <- tabelize(foodSections, priceList[priceList$Section != "Drink" & priceList$Section != "Option",]) 
      # additional options to make rendering possible
      return(div(HTML(out),class="shiny-html-output"))
    })
  })



  #2.c. Select Item from price list ----
  
  output$SelectedItem <- renderUI({selectInput(inputId = 'Item',
                                               label = 'Selection',
                                               choices = as.character(priceList$Item))
  })
  
  #2.d. Select Number of items ----
  
  output$SelectedItemNum <- renderUI({selectInput(inputId = 'ItemNum',
                                                  label = 'Quantity',
                                                  choices = c(1:20))
  })
  
  #2.e. Write order into order data frame ----
  
  observeEvent(input$submit, {
    if (input$submit > 0) {
      #values$df <- data.frame(row_names = "", Item = "", Number = 0, Price = 0, Pub = "", TableNumber = 0, OrderNumber = 0, OrderQrRef = "", OrderStatus = "")
      values$price <- priceList$Price[match(input$Item, priceList$Item)]
      values$OrderNum <- 0
      
      #create the order record in dataframe row
      values$TableNumber <- input$TableNumber
      newLine <- c(input$submit, input$Item, input$ItemNum, values$price, values$TestCentre, input$TableNumber, values$OrderNum, "", "Open") 
      values$df[dim(values$df)[[1]],] <- newLine
      values$df[dim(values$df)[[1]] + 1,] <- c(input$submit + 1, "Total", sum(as.numeric(values$df$Number)), sum(as.numeric(values$df$Number)*as.numeric(values$df$Price)), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
      
      if(serviceChargePc > 0 & input$submit == 1) {
        values$df <- values$df[-match("Total", values$df$Item),]
        values$df[dim(values$df)[[1]] + 1,] <- c(input$submit + 1, "Service Charge", 1, round(sum(as.numeric(values$df$Number)*as.numeric(values$df$Price))*serviceChargePc,2), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
        values$df[dim(values$df)[[1]] + 1,] <- c(input$submit + 1, "Total", sum(as.numeric(values$df$Number)), sum(as.numeric(values$df$Number)*as.numeric(values$df$Price)), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
      } else if(serviceChargePc > 0 & input$submit > 1) {
        values$df <- values$df[-match("Service Charge", values$df$Item),]
        values$df <- values$df[-match("Total", values$df$Item),]
        values$df[dim(values$df)[[1]] + 1,] <- c(input$submit + 1, "Service Charge", 1, round(sum(as.numeric(values$df$Number)*as.numeric(values$df$Price))*serviceChargePc,2), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
        values$df[dim(values$df)[[1]] + 1,] <- c(input$submit + 1, "Total", sum(as.numeric(values$df$Number)), sum(as.numeric(values$df$Number)*as.numeric(values$df$Price)), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
      }
      
      values$stringCaption <- (paste0(values$TestCentre,", Table Number ", as.character(values$TableNumber)))
      
      output$TableTitle <- renderText({values$stringCaption})
      output$OrderTable <- DT::renderDataTable(DT::datatable(values$df[,2:4], selection = 'single', rownames = FALSE, 
                                  options = list(paging = FALSE, searching = FALSE, bInfo = FALSE, autoWidth = TRUE
                                                 #, columnDefs = list(list(width = '1200px', targets = '_all))
                                  )))
    }
  })
  
   observeEvent(input$OrderTable_rows_selected, {
    
    shinyalert(
      text = "Delete selected item?",
      showCancelButton = TRUE,
        callbackR = function(x) { if(x != FALSE) {
          values$df = values$df[-c(input$OrderTable_rows_selected, dim(values$df)[[1]]),]
          values$df[dim(values$df)[[1]] + 1,] <- c(dim(values$df)[[1]] + 1, "Total", sum(as.numeric(values$df[,2])), sum(as.numeric(values$df[,2])*as.numeric(values$df[,3])), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
          }
        }, 
      inputId = "shinyalert"
      )
    })
  
#  observeEvent(input$clear, {
#    
#    if(input$clear > 0) {
#      #values$df <- data.frame(
#      #  row_number = "", Item = "", Number = 0, Price = 0, Pub = "", TableNumber = 0, OrderNumber = 0, OrderQrRef = "", OrderStatus = "", 
#      #  stringsAsFactors = FALSE
#      #)
#      session$reload()
#    }
#  })
  
  #Allow customer to downloadable privacy policy ----
  output$downloadPriv <- downloadHandler(
    filename = "GDPR Policy and Checklist.txt",
    content = function(file){
      file.copy("GDPR Policy and Checklist.txt", file)
    }
  )
  
  #2.f. Record the order ----
  
  observeEvent(input$confirm, {

    if (input$confirm > 0) {

      #output$NameCheck <- renderText({
      #  validate(need(input$CustName != "", message = "Error: Please enter your name and try again"))
      #  ""
      
      #output$GDPRCheck <-  renderText({
      #  validate(need(input$GDPR_ok == TRUE, message = "Error: If you have given your name, please consent to share name and phone number to proceed"))
      #  ""
      #})
    
      #output$EmailCheck <- renderText({
      #  validate(need(input$CustEmail != "", message = "Error: Please enter your email address and try again"))
      #  ""
      #})
      
      #output$EmailCheck2 <- renderText({
      #  validate(need(input$CustEmail == input$CustEmailCheck, message = "Error: Entered email addresses do not match"))
      #  ""
      #})
      
      #output$PhoneNumberCheck <- renderText({
      #  validate(need((substring(input$CustPhoneNumber,1,1) != "0" & input$CustPhoneNumber != ""), message = "Error: Please enter phone number ommitting leading 0"))
      #  ""
      #})
      
      #validate(need(input$CustEmail != "", message = "Error: Please enter a customer email and try again"))
      
      #if all items in order are from the same pub, output the order table
      #print(is.null(input$CustName))
      #warning if order is empty but try to proceed
      if(dim(values$df)[[1]] < 2) {
        shinyalert(
          text = "You have pressed confirm but there are no items in your order.  Please try again and press the 'Add to Order' button after making each selection.",
          showCancelButton = FALSE,
          inputId = "shinyalert"
        )
      } else if(input$GDPR_ok == FALSE & (input$CustName != "" | input$CustPhoneNumber != "")) {
        shinyalert(
          text = "Error: If you have given your name or phone number, please consent to share name and phone number to proceed",
          showCancelButton = FALSE,
          inputId = "shinyalert2"
        )
      } else {
        
        #Record Data to data frame prior to updating data base
        #OrderName = "", OrderDOB = "", OrderGender = "", OrderNhsNo = "", OrderEmail = "", OrderIntPhone = 0, OrderPhone = 0, OrderNumber = 0, RandEl = "", OrderQrRef = "", stringsAsFactors = FALSE
        n <- dim(values$Rec)[[1]]
        
        #values$Rec$OrderEmail[1] <- input$CustEmail
        values$Rec$OrderEmail[1] <- ""
        values$Rec$OrderTimeIn[1] <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
        if(input$GDPR_ok == TRUE) {
          values$Rec$OrderIntPhone[1] <- input$CustIntCode
          values$Rec$OrderPhone[1] <- input$CustPhoneNumber
          values$Rec$OrderName[1] <- paste0(gsub(" ", "_", input$CustName), strrep("*", max(0, 9 - nchar(input$CustName))))
        } else {
          values$Rec$OrderIntPhone[1] <- 0
          values$Rec$OrderPhone[1] <- 0
          values$Rec$OrderName[1] <- ""
        }
        
        #create the order number
        #establish database connection
        
        options(mysql = list(
          "host" = Sys.getenv("SQL_ENDPOINT"),
          "port" = Sys.getenv("SQL_PORT"),
          "user" = Sys.getenv("MY_UID"),
          "password" = Sys.getenv("MY_PWD")
        ))
        cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                        username = options()$mysql$user,
                        password = options()$mysql$password,
                        host     = options()$mysql$host,
                        port     = options()$mysql$port,
                        dbname = "BAR"
        )
        
        values$OrdNumInt <- dbGetQuery(cn, paste0("SELECT MAX(OrderNumber) FROM ", values$TestCentre, "Orders;"))
        values$OrderNum <- as.numeric(values$OrdNumInt[1,1]) + 1
        values$Rec$OrderNumber <- values$OrderNum
        values$df$OrderNumber <- values$OrderNum
        
        values$Rec$OrderNumber[1] <- values$OrderNum
        values$RandEl[1] <-formatC(as.integer(sample(1:100000000, 1)), width = 9, flag = "0")
        values$Rec$OrderQrRef[1] <- paste0(as.character(Sys.time()), substring(values$TestCentre, nchar(values$TestCentre) - 7, nchar(values$TestCentre)), values$Rec$OrderNumber[1])
        
        values$df$OrderQrRef <- rep(values$Rec$OrderQrRef[1], dim(values$df)[[1]])
        
        values$Db <- list(values$Rec, values$df)
        names(values$Db) <- c("OrderRecord", "CompOrderList")
        
        #add order and record to database
        
        dbWriteTable(cn, name = paste0(values$TestCentre, "Records"), value = values$Rec, append = TRUE)
        dbWriteTable(cn, name = paste0(values$TestCentre, "Orders"), value = values$df, append = TRUE)
        dbDisconnect(cn)
        
        #switch to order summary tab
        showTab(inputId = "inTabset", target = "panel2")
        updateTabsetPanel(session, "inTabset",
                          selected = "panel2")
        hideTab(inputId = "inTabset", target = "panel0")
        hideTab(inputId = "inTabset", target = "panel1")
        hideTab(inputId = "inTabset", target = "panel3")
        hideTab(inputId = "inTabset", target = "panel4")
        
        #clear order screen and give order received and confirmed message
        
        output$ConfMessage <- renderText({paste0("Order Received and Confirmed.  Order Number ", values$OrderNum, ", Table Number ", values$TableNumber)})
        output$OrderSummary <- renderTable({values$df[,2:4]})
        #values$df <- data.frame(Item = "", Number = 0, Price = 0, Pub = "", TableNumber = 0, OrderNumber = 0, OrderQrRef = "", OrderStatus = "")
        
        
        #2.g Allow customer to downloadable png qr code ----
        output$downloadPNG <- downloadHandler(
          filename = gsub(":", "_", paste0(input$CustEmail, Sys.time(),".png")),
          content = function(file = gsub(":", "_", paste0(input$CustEmail, gm_date(),".png"))) {
            png(file, width = 200, height = 200)
            print(qrcode_gen(values$Rec$OrderQrRef[1]))
            dev.off()
          })
      }
      }
  })      
  
  #2.h Start New Order Command
  observeEvent(input$refresh, {
    if(input$refresh > 0) {
      session$reload()
    }
  })
  
  #2.g Close App Command
  #observeEvent(input$quit, {
  #  if(input$quit == 1) {
  #    stopApp()
  #  }
  #})
}


#3. Shiny UI function

shinyUI <- fluidPage(
  
  #theme = "bootstrap.css",
  setBackgroundColor(
    color = "GhostWhite"
  ),
  
  #mainPanel(
    
  titlePanel(paste0(gsub("_", " ", gsub("_n", " &", venueDisplayTitle)), " Ordering App")),
  
    tabsetPanel(id = "inTabset",

      
      tabPanel(title = "Select Table Number", value = "panel3",
               #select pub/cafe
               #uiOutput(outputId = "SelectedTestCentre"),
               
               #select item type
               uiOutput(outputId = "TableNumber"),
               
               actionButton(inputId = "confirmPubTable", label = "Confirm Table Number")
               
      ),
      
      tabPanel(title = "Food Menu", value = "panel0",
                         #titlePanel("Food Menu"),
               uiOutput(outputId = "foodTables")
      ),
      tabPanel(title = "Drinks Menu", value = "panel4",
               titlePanel("Drinks Menu"),
               #tags$h4("Drinks"),
               tableOutput(outputId = "DrinkMenu")
      ),
                
    tabPanel(title = "Build Your Order", value = "panel1",
    titlePanel("Build Your Order"),
      
    helpText("Select item and quantity then click 'Add to Order'. Repeat to build order."),
    helpText("Please flick back to the Menu tabs if you want to take another look."),
    helpText("To remove a selection,  click on the item in the order summary."),
    #helpText("Please also select any optional choices from this dropdown list."),
    
    #select item type
    uiOutput(outputId = "SelectedItem"),
    
    #select number of items
    uiOutput(outputId = "SelectedItemNum"),
    
    #submit to order button
    actionButton(inputId = "submit", label = "Add to Order"),
    
    #clear order button
    #actionButton(inputId = "clear", label = "Clear Order"),
    
    #data file
    #fileInput(inputId = "file", label = "Upload Pricelist (csv file)"),
    #helpText("max file size is 60MB"),
    
    div(style="margin-bottom:10px"),
    
    #useShinyalert(rmd = FALSE),
    useShinyalert(),
    
    #textOutput(outputId = "TableTitle"),
    DT::dataTableOutput(outputId = "OrderTable", width = '500px'),
    
    div(style="margin-bottom:10px"),
    
    textInput(inputId = "CustName", label = "Name"),
    #textOutput(outputId = "NameCheck"),
    
    #input customer email
    #textInput(inputId = "CustEmail", label = "Email"),
    #textOutput(outputId = "EmailCheck"),
    
    #validate customer email
    #textInput(inputId = "CustEmailCheck", label = "Email Validation (please re-enter your email address)"),
    #textOutput(outputId = "EmailCheck2"),
    
    #input customer phone number
    
    div(style="display: inline-block;vertical-align:top; width: 75px;",textInput(inputId = "CustIntCode", label = "Int Code", value = +44)),
    div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 150px;",textInput(inputId = "CustPhoneNumber", label = "Phone Number")),
    div(style="display: clear"),
    #textOutput(outputId = "PhoneNumberCheck"),
    
    checkboxInput(inputId = "GDPR_ok", "I agree to my name and phone number being stored for sharing with NHS test and trace, as per the privacy policy.", FALSE),
    textOutput(outputId = "GDPRCheck"),
    #div(style="margin-bottom:10px"),
    downloadLink(outputId = "downloadPriv", label = "Privacy Policy"),

    #Proceed to payment button
    div(style="margin-bottom:10px"),
    actionButton(inputId = "confirm", label = "Confirm & Complete")#,
    
    ),
    
    tabPanel(title = "Order Confirmation", value = "panel2",
      div(style="margin-bottom:10px"),
      textOutput(outputId = "ConfMessage"),
      tableOutput(outputId = "OrderSummary"),
      div(style="margin-bottom:10px"),
      
      tags$b("Please take note of your Order Number and Download the QR code"),
      div(style="margin-bottom:10px"),
      
      downloadButton(outputId = "downloadPNG", label = "Download QR code order record"),
      
      div(style="margin-bottom:10px"),
      actionButton(inputId = "refresh", label = "Start a New Order"),
      #actionButton(inputId = "quit", label = "Close App")

      )
    )
  #)
)

shinyApp(ui = shinyUI, server = shinyServer)








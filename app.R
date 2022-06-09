####################################
# Anmol Lakhotia
####################################

# Load R packages
library(shiny)
library(shinythemes)
library(CFBUS342)


  # Define UI
  ui <- fluidPage(theme = shinytheme("yeti"),
    navbarPage(
      "BUS 342",
      tabPanel("Present Value Calculator",
               sidebarPanel(
                 tags$h3("Input:"),
                 tags$h5("Future Value"),
                 textInput("pv.fv", ""),
                 tags$h5("Periods"),
                 textInput("pv.periods", ""),
                 tags$h5("Interest Rate"),
                 textInput("pv.interest_rate", ""),), # sidebarPanel
               mainPanel(h1("Output"), h4("Present Value"), verbatimTextOutput("pv.txtout")) # mainPanel
               
      ), # Navbar 1, tabPanel
      
      tabPanel("Future Value Calculator",
               sidebarPanel(
                 tags$h3("Input:"),
                 tags$h5("Present Value"),
                 textInput("fv.pv", ""),
                 tags$h5("Periods"),
                 textInput("fv.periods", ""),
                 tags$h5("Interest Rate"),
                 textInput("fv.interest_rate", ""),), # sidebarPanel
               mainPanel(h1("Output"), h4("Future Value"), verbatimTextOutput("fv.txtout")) # mainPanel
               
      ), # Navbar 2, tabPanel
      tabPanel("Perpetuity Present Value Calculator",
               sidebarPanel(
                 tags$h3("Input:"),
                 tags$h5("Payment"),
                 textInput("perpetuity.payment", ""),
                 tags$h5("Discount Rate"),
                 textInput("perpetuity.discount_rate", ""),
                 tags$h5("Growth Rate"),
                 textInput("perpetuity.growth_rate", ""),), # sidebarPanel
               mainPanel(h1("Output"), h4("Present Value"), verbatimTextOutput("perpetuity.txtout")) # mainPanel
               
      ) # Navbar 3, tabPanel
      
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$pv.txtout <- renderText({
      PV(as.numeric(input$pv.fv), as.numeric(input$pv.periods), as.numeric(input$pv.interest_rate))
    })
    
    output$fv.txtout <- renderText({
      FV(as.numeric(input$fv.pv), as.numeric(input$fv.periods), as.numeric(input$fv.interest_rate))
    })
    
    output$perpetuity.txtout <- renderText({
      PERPETIUTY_PV(as.numeric(input$perpetuity.payment), as.numeric(input$perpetuity.discount_rate), as.numeric(input$perpetuity.growth_rate))
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
